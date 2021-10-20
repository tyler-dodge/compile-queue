;;; -*- lexical-binding: t -*-

(defcustom compile-queue-view-mode-line-format
  '(" %b" " - " (:eval (compile-queue-view-mode--mode-line-command-name)) " " (:eval (compile-queue-view-mode--mode-line-scheduled)))
  "The `mode-line-format' used by the queue buffer."
  :safe t
  :type 'string
  :group 'compile-queue)

(defvar compile-queue-view-mode-map
  (--doto (make-sparse-keymap)
    (define-key it (kbd "RET")
      #'compile-queue-view-mode-display-execution-buffer)))

(define-derived-mode compile-queue-view-mode special-mode "Compile-Queue"
  "Mode for displaying the output of the current queue's execution's compile buffer."
  :group 'compile-queue
  (when compile-queue-view-mode-line-format
    (setq-local mode-line-format compile-queue-view-mode-line-format))
  (setq-local buffer-read-only t))

(defun compile-queue-view-mode--mode-line-scheduled ()
  "Return the scheduled command names for the mode line."
  (let* ((scheduled (-some-> (compile-queue-current)
                      compile-queue--scheduled)))
    (-some--> scheduled
      (--map
       (let ((command (->> it (compile-queue-promise--command))))
         (pcase command
           ((pred (compile-queue-group-p))
            (compile-queue-group--name command))
             ((pred (compile-queue-shell-command-p))
               (or
                (compile-queue-command--name command)
                (-some-> command compile-queue-shell-command-full-command)))))
       it)
      (s-join ", " it)
      (concat "[" it "]"))))

(when (boundp 'evil-motion-state-modes)
  (add-to-list 'evil-motion-state-modes 'compile-queue-view-mode))

(defun compile-queue-view-mode--mode-line-command-name ()
  "Return the command name for the current execution for the mode line."
  (let* ((execution (-some-> (compile-queue-current)
                      compile-queue--target-execution))
         (group-name (-some-> execution
           compile-queue-execution--group-execution
           compile-queue-group-execution--group
           compile-queue-group-name)))
    (concat
     (-some--> group-name (concat "(" it "|"))
     (-some-> execution
       compile-queue-execution-name)
     (when group-name ")"))))

(defun compile-queue-view-mode--update-buffer (queue)
  "Update the target view buffer of the QUEUE."
  (when-let ((execution-buffer (-> queue compile-queue--target-execution compile-queue-execution--buffer)))
    (let* ((buffer-name (compile-queue-buffer-name queue))
           (buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name))))
      (with-current-buffer buffer
        (compile-queue-view-mode)
        (setq-local compile-queue-delegate-mode--queue queue)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (goto-char (point-max))))
    (force-mode-line-update)))

(defun compile-queue-view-mode-display-execution-buffer ()
  "Display the execution buffer for the current compile-queue."
  (interactive)
  (display-buffer (compile-queue-execution-buffer (compile-queue--target-execution  (compile-queue-current)))))

(provide 'compile-queue-view-mode)
;;; compile-queue-view-mode.el ends here
