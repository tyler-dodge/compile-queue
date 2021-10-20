;;; -*- lexical-binding: t -*-
(defvar-local compile-queue-delegate-mode--process-filter-delegate nil
  "The process filter that compile-queue delegates to while running.")

(defvar-local compile-queue-delegate-mode--process-sentinel-delegate nil
  "The process sentinal that compile-queue delegates to while running.")

(defvar compile-queue-delegate-mode--inhibit-process-sentinel nil
  "Inhibit outputting process sentinel events when this is non-nil")

(defvar-local compile-queue-delegate-mode--queue nil
  "The compile-queue that the current buffer is referencing.")

(defvar-local compile-queue-delegate-mode--gc-timer nil
  "The timer for cleaning up the current buffer after the process completes.")

(defvar-local compile-queue-delegate-mode--execution nil
  "The execution that is related to the current buffer's process.")

(defvar-local compile-queue-delegate-mode--matcher-disabled nil
  "When non-nil, disable the current execution's matcher.
Set automatically if the matcher throws an error.")

(define-minor-mode compile-queue-delegate-mode
  "Minor mode for buffers that delegate to the compile-queue."
  :init-value nil
  :lighter " compile-queue-delegate"
  (if compile-queue-delegate-mode
      (progn
        (unless (memq 'compile-queue-delegate-mode--forward-change after-change-functions)
          (setq-local after-change-functions
                      (append (-some->> after-change-functions (-drop-last 1)) '(compile-queue-delegate-mode--forward-change t)))))
    (setq-local after-change-functions (--filter (not (equal it #'compile-queue-delegate-mode--forward-change)) after-change-functions))))

(defun compile-queue-delegate-mode--scroll-to-end (window)
  (with-selected-frame (window-frame window)
    (let* ((window-start
            (with-current-buffer (window-buffer window)
              (save-excursion
                (goto-char (point-max))
                (setq temporary-goal-column (current-column)) ; Prevents errors in line-move-visual
                (line-move-visual (ceiling (- (- (window-height window 'floor) 4))) t)
                (point))))
           (pt-max (with-current-buffer (window-buffer window) (point-max))))
      (when window-start
        (window-state-put (compile-queue-delegate-mode--tail-end-window-state window pt-max window-start) window)))))

(defun compile-queue-delegate-mode--tail-end-window-state (window new-pt new-start)
   (-let [(window-state &as &alist 'buffer (buffer-list &as &alist 'start start)) (window-state-get window t)]
    (setf (alist-get 'start buffer-list) new-start)
    (setf (alist-get 'point buffer-list) new-pt)
    window-state))

(defun compile-queue-delegate-mode--status-code-for-process (process status)
  "Return the STATUS from PROCESS as a status code."
  (pcase (process-status process)
    ((or 'exit 'signal 'failed)
     (pcase (s-trim status)
       ("finished" 0)
       ((rx line-start "exited abnormally with code" (* whitespace) (let code (* digit)))
        (string-to-number code))
       (_ nil)))
    (_ nil)))

(defun compile-queue-delegate-mode--process-filter (process output)
  "Process filter that delegates OUTPUT to the original PROCESS filter.
Also, manages scolling windows to the end if the point
is currently set at `point-max'."
  (let* ((process-buffer (process-buffer process))
         (queue (compile-queue-current process))
         (delegate
          (-some->> process-buffer
            (buffer-local-value 'compile-queue-delegate-mode--process-filter-delegate)))
         (execution (buffer-local-value 'compile-queue-delegate-mode--execution process-buffer))
         (start (marker-position (process-mark process))))
    (-some--> delegate (progn
                         (funcall it process output)))
    (when-let ((matcher (-some-> execution
                          compile-queue-execution--promise
                          compile-queue-promise--command
                          compile-queue-command--matcher)))
      (let ((inhibit-redisplay t))
        (with-current-buffer process-buffer
          (goto-char start)
          (when (and (not compile-queue-delegate-mode--matcher-disabled)
                     (condition-case error
                         (if (functionp matcher)
                             (funcall matcher output)
                           output)
                       (error
                        (setq-local compile-queue-delegate-mode--matcher-disabled t)
                        (error "%s" (error-message-string error)))
                       (user-error
                        (setq-local compile-queue-delegate-mode--matcher-disabled t)
                        (error "%s" (error-message-string error)))))
            (deferred:callback
              (-> execution
                  compile-queue-execution--promise
                  compile-queue-promise--deferred)
              (-> execution compile-queue-execution--buffer))
            (compile-queue-execute queue)))))))

(defun compile-queue-delegate-mode--process-sentinel (process status)
  "Delegating sentinel for compile-queue.
Delegates to the original `process-sentinel' for PROCESS except
when the `process-sentinel' is the default.
Handles notifying compile queue the process STATUS on completion."
  (catch 'inhibited
    (let* ((buffer (process-buffer process))
           (delegate
            (-some->> buffer
              (buffer-local-value 'compile-queue-delegate-mode--process-sentinel-delegate))))
      (when compile-queue-delegate-mode--inhibit-process-sentinel
           (progn
             (message "Skipping because inhibited")
             (throw 'inhibited t)))
      (unless (eq delegate #'internal-default-process-sentinel)
        (compile-queue--save-var-excursion ((inhibit-read-only t))
          (-some--> delegate (funcall it process status))))

      (unless (process-live-p process)
        (let* ((execution (-some--> (buffer-local-value 'compile-queue-delegate-mode--execution buffer)
                            (when (eq (compile-queue-execution--process it) process) it)))
               (command (-some-> execution compile-queue-execution--promise compile-queue-promise--command))
               (status-code (compile-queue-delegate-mode--status-code-for-process process status))
               (queue (-some-> (compile-queue-current process)))
               (is-target (when execution
                            (compile-queue-execution-eq-id
                             execution
                             (-some-> queue compile-queue--target-execution))))
               (non-zero-exit (not (eq status-code 0))))
          (when execution (setf (compile-queue-execution--status-code execution) status-code))
          (when is-target
            (when (compile-queue-execution-p (compile-queue--target-execution queue))
              (setf (compile-queue--target-execution queue)
                    (-some--> queue
                      (compile-queue--target-execution it)
                      (compile-queue-execution--group-execution it))))
            
            (when non-zero-exit (compile-queue-clean queue)))
          (-some->> command
            (compile-queue-item-after-complete)
            (funcall (compile-queue--callback execution)))

          (when is-target
            (progn
              (if non-zero-exit
                  (-some--> execution
                    (compile-queue-execution--promise it)
                    (compile-queue-promise--deferred it)
                    (unless (deferred:status it) (deferred:errorback it (string-trim status))))
                (-some--> execution
                  (compile-queue-execution--promise it)
                  (compile-queue-promise--deferred it)
                  (deferred:callback it (-some-> execution compile-queue-execution--buffer))))
              (unless (and (-some-> command compile-queue-command--keep-buffer))
                (let ((buffer (compile-queue-execution--buffer execution)))
                  (when (and (buffer-live-p buffer) compile-queue-garbage-collect-time)
                    (with-current-buffer buffer

                      (setq-local
                       compile-queue-delegate-mode--gc-timer
                       (run-at-time compile-queue-garbage-collect-time nil
                                    (lambda () (compile-queue-delegate-mode--gc-callback execution))))))))
              (unless non-zero-exit (compile-queue-execute queue)))))))))

(defun compile-queue-delegate-mode--gc-callback (execution)
  "Kill the buffer for EXECUTION if the buffer is still the same execution."
  (let ((buffer (compile-queue-execution--buffer execution)))
    (when (and (buffer-live-p buffer)
               (not (compile-queue-allows-output-p
                     (buffer-local-value 'compile-queue-delegate-mode--queue buffer)
                     execution)))
      (kill-buffer buffer))))

(defun compile-queue-delegate-mode--forward-change (beg end length)
  "Forward change to the `compile-queue-delegate-mode--queue' buffer.
Replaces the text at BEG with LENGTH with the text between BEG and END
from the execution-buffer in the compile-queue-delegate-mode--queue buffer."
  (when (compile-queue-allows-output-p compile-queue-delegate-mode--queue
                                     compile-queue-delegate-mode--execution)
    (let ((text (buffer-substring beg end))
          (inhibit-redisplay t)
          (removed-lines (or (and compile-queue-max-buffer-line-limit (- (line-number-at-pos (point-max)) compile-queue-max-buffer-line-limit))
                             0)))
      (when (> removed-lines 0)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (delete-region (point-min) (save-excursion (forward-line removed-lines) (point))))))

      (with-current-buffer (compile-queue-buffer-name compile-queue-delegate-mode--queue)
        (let* ((compile-queue-end-pt (point-max))
               (scroll-to-end (->> (get-buffer-window-list (current-buffer) nil t)
                                   (--filter (>= (window-point it) compile-queue-end-pt)))))
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char beg)
              (delete-char length)
              (insert text)
              (when (> removed-lines 0)
                (goto-char (point-min))
                (delete-region (point-min) (save-excursion (forward-line removed-lines) (point))))))
          (when scroll-to-end (--each scroll-to-end (compile-queue-delegate-mode--scroll-to-end it))))))))

(provide 'compile-queue-delegate-mode)
;;; compile-queue-delegate-mode.el ends here
