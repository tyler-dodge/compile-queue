;;; -*- lexical-binding: t -*-

(defun compile-queue-shell-command-buffer-name (command)
  "Return the buffer name of the execution buffer for COMMAND.
return `compile-queue-shell-command-buffer-name' if available.

If `compile-queue-shell-command-name' is set,
return *`compile-queue-shell-command-name'*.

Otherwise return the command's string truncated."
  (or (compile-queue-shell-command--buffer-name command)
      (-some--> (compile-queue-shell-command--name command) (concat " *" it "*"))
      (concat " *" (s-truncate 20 (compile-queue-shell-command-full-command command)) "*")))

(defun compile-queue-shell-command-name (command)
  "Return the name of COMMAND.
return `compile-queue-shell-command-buffer-name' if available.
If `compile-queue-shell-command-buffer-name' is set, return
`compile-queue-shell-command-buffer-name' instead.

Otherwise return the command's string truncated."
  (or (compile-queue-shell-command--name command)
      (compile-queue-shell-command-buffer-name command)
      (concat (s-truncate 10 (compile-queue-shell-command-full-command command)))))

(defun compile-queue-shell-command--init-buffer (command)
  "Initialize the buffer COMMAND which is a `compile-queue-shell-command'.
Replaces the buffer if it already exists."
  (pcase-let* (((cl-struct compile-queue-shell-command (default-directory directory)) command)
               (buffer-name (compile-queue-shell-command-buffer-name command)))
    (unless (get-buffer buffer-name) (generate-new-buffer buffer-name))
    (with-current-buffer buffer-name

      (-some--> (get-buffer-process (current-buffer))
        (when (process-live-p it)
          (kill-process it)
          (set-process-filter it t)))
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (-some-> compile-queue-delegate-mode--gc-timer cancel-timer)
      (setq-local compile-queue-delegate-mode--gc-timer nil)
      (funcall
       (or (compile-queue-shell-command--major-mode command)
           compile-queue-shell-default-major-mode))

      (--each (compile-queue-shell-command--vars command)
        (set (car it) (eval (cdr it))))

      (compile-queue-delegate-mode 1)

      (prog1 (get-buffer buffer-name)
        (when directory (setq-local default-directory directory))
        (goto-char (point-max))))))

(cl-defmethod compile-queue-command-restart ((command compile-queue-shell-command)
                                             execution)
  (-let ((command (or (compile-queue-execution-command execution)
                      (error "Expected to be passed an `compile-queue-execution' containing a `compile-queue-shell-command'."))))
    (or (compile-queue-shell-command-p command) (error "Unexpected type for command %S" command))
    (unwind-protect
         (progn
           (setq compile-queue-delegate-mode--inhibit-process-sentinel t)
           (let* ((buffer (compile-queue-shell-command--init-buffer command))
                  (process (or (-some-> execution compile-queue-execution-command compile-queue-command--start-process)
                               (error "Failed to start the process: %S." execution))))
             (setf (buffer-local-value 'compile-queue-delegate-mode--queue buffer) (compile-queue-execution-queue execution))
             (setf (buffer-local-value 'compile-queue-delegate-mode--execution buffer) execution)
             (setf (buffer-local-value 'compile-queue-delegate-mode--process-filter-delegate buffer) (process-filter process))
             (setf (buffer-local-value 'compile-queue-delegate-mode--process-sentinel-delegate buffer) (process-sentinel process))
             (setf (compile-queue-execution--process execution) process)
             (set-process-sentinel process #'compile-queue-delegate-mode--process-sentinel)
             (set-process-filter process #'compile-queue-delegate-mode--process-filter)
             (with-current-buffer buffer (erase-buffer))))
       (setq compile-queue-delegate-mode--inhibit-process-sentinel nil))))

(cl-defgeneric compile-queue-shell-command-full-command (command))

(cl-defmethod compile-queue-shell-command-full-command ((command compile-queue-shell-command))
    "Return the full shell command for COMMAND."
  (compile-queue-shell-command--command command))

(cl-defmethod compile-queue-shell-command-full-command ((command compile-queue-ssh-shell-command))
    "Return the full shell command for COMMAND."
  (compile-queue-shell-command--command command))

(cl-defmethod compile-queue-command--start-process ((command compile-queue-shell-command))
  (compile-queue--save-var-excursion (process-environment)
    (--each (compile-queue-shell-command--env command)
      (add-to-list 'process-environment (concat (car it) "="
                                                (cdr it))))
    (let ((process-connection-type (compile-queue-shell-command--pty command)))
      (start-file-process-shell-command
       (compile-queue-shell-command-name command)
       (compile-queue-shell-command-buffer-name command)
       (compile-queue-shell-command-full-command  command)))))

(cl-defmethod compile-queue-command--start-process ((command compile-queue-ssh-shell-command))
  (let ((host (compile-queue-ssh-shell-command--host command)))
    (compile-queue--save-var-excursion (process-environment)
      
      
      (let* ((process-connection-type nil)
             (temp-file (make-temp-file "compile-queue-command" nil nil))
             (temp-file-name (f-filename temp-file))
             (destination-file (f-join "/tmp/" temp-file-name)))
        (chmod temp-file 0400)
        (with-temp-buffer
          (insert "#/bin/zsh\n")
          (insert (concat "trap 'rm " destination-file "' EXIT\n"))
          (cl-loop for (key . value) in (compile-queue-shell-command--env command)
                   do (insert "export " key "=" value "\n"))
          (insert (compile-queue-shell-command-full-command command))
          (write-region (point-min) (point-max) temp-file nil t))
        (let ((process-connection-type (compile-queue-shell-command--pty command)))
          (start-file-process-shell-command
           (compile-queue-shell-command-name command)
           (compile-queue-shell-command-buffer-name command)
           (s-join " "
                   (list
                    "scp" temp-file (concat host ":" destination-file)
                    "1>/dev/null"
                    "&&"
                    "ssh" host "bash" destination-file))))))))

(cl-defmethod compile-queue-command--execute ((command compile-queue-shell-command) promise queue group)
  "Execute PROMISE in a buffer related to QUEUE."
  (let* ((command (compile-queue-promise--command promise))
         (buffer (progn
                   (setf (compile-queue--outputting-executions queue) nil)
                   (--> (compile-queue-buffer-name queue)
                        (with-current-buffer (or (get-buffer it) (generate-new-buffer it))
                          (let ((inhibit-read-only t)) (erase-buffer))))
                   (compile-queue-shell-command--init-buffer command))))
    (with-current-buffer buffer
      (setq-local compile-queue-delegate-mode--queue queue)
      (let ((execution (compile-queue-execution-create
                        :buffer buffer
                        :group-execution group
                        :status-code nil
                        :promise promise
                        :queue queue)))
        (setq-local compile-queue-delegate-mode--execution execution)
        (prog1 execution
          (setf (compile-queue--target-execution queue) execution)
          (compile-queue-limit-output-to-target queue)
          (-some--> (compile-queue-item-before-start command)
            (funcall (compile-queue--callback execution) it))
          (let* ((process (compile-queue-command--start-process command)))

            (when (derived-mode-p #'comint-mode)
              (goto-char (point-min))
              (set-marker (process-mark process) (point))
              (setq-local comint-last-output-start (make-marker))
              (set-marker comint-last-output-start (point))
              (set-process-filter process #'comint-output-filter))
            (setf (compile-queue-execution--process execution) process)
            (setf (compile-queue-execution--buffer execution) buffer)
            (--doto process
              (when it
                (setq-local compile-queue-delegate-mode--process-filter-delegate (process-filter it))
                (setq-local compile-queue-delegate-mode--process-sentinel-delegate (process-sentinel it))
                (set-process-sentinel it #'compile-queue-delegate-mode--process-sentinel)
                (set-process-filter it #'compile-queue-delegate-mode--process-filter)))
            (compile-queue-view-mode--update-buffer queue)))))))

(provide 'compile-queue-shell-command)
