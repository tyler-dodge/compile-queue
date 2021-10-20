;;; -*- lexical-binding: t -*-
(declare-function org-runbook--validate-command "ext:org-runbook.el" (command))
(declare-function org-runbook-command-subcommands "ext:org-runbook.el" (command))
(declare-function org-runbook-subcommand-p "ext:org-runbook.el" (command))
(declare-function org-runbook-subcommand-command "ext:org-runbook.el" (command))
(declare-function org-runbook-elisp-subcommand-p "ext:org-runbook.el" (command))
(declare-function org-runbook-command-name "ext:org-runbook.el" (command))
(declare-function org-runbook-elisp-subcommand-elisp "ext:org-runbook.el" (command))

(defun compile-queue-execute-org-runbook-command (command &optional queue)
  "Schedule the `org-runbook-command' COMMAND.
The queue it is scheduled on will be either QUEUE, `compile-queue-current'
or `compile-queue-root-queue'.

Meant to be used as the action for `org-runbook-execute-command-action'."
  (org-runbook--validate-command command)

  (let ((queue (or (-some--> (org-runbook-command-get-property command "QUEUE")
                     (compile-queue--by-name it))
                   queue
                   (compile-queue-current)
                   (compile-queue--by-name compile-queue-root-queue))))
    (cl-loop
     with commands = nil
     with deferred = nil
     for subcommand in (org-runbook-command-subcommands command)
     do
     (cond ((org-runbook-subcommand-p subcommand) (push (org-runbook-subcommand-command subcommand) commands))
           ((org-runbook-elisp-subcommand-p subcommand)
            (if commands
                (let ((shell-command (compile-queue-shell-command-create
                                      :pty (org-runbook-command-pty command)
                                      :name (org-runbook-command-name command)
                                      :command (->> commands (reverse) (-non-nil) (-map #'s-trim) (s-join "; ")))))
                  (setq deferred
                        (deferred:$
                          (if deferred
                              (deferred:nextc it deferred)
                            (lambda (&rest _) (compile-queue-schedule queue shell-command))
                            (compile-queue-schedule queue shell-command))
                          (deferred:nextc it `(lambda ()
                                                (save-excursion
                                                  (when-let (buffer (compile-queue-buffer-name ,queue))
                                                    (set-buffer buffer))
                                                  ,(org-runbook-elisp-subcommand-elisp subcommand))))))
                  (setq commands nil))
              (setq deferred
                    (deferred:$
                      (if deferred
                          (deferred:nextc deferred `(lambda ()
                                                      (save-excursion
                                                        (when-let (buffer (compile-queue-buffer-name ,queue))
                                                          (set-buffer buffer))
                                                        ,(org-runbook-elisp-subcommand-elisp subcommand))))
                        (with-current-buffer (compile-queue-buffer-name queue)
                          (eval (org-runbook-elisp-subcommand-elisp subcommand) t))))))))
     finally return
     (when commands
       (let* ((host (org-runbook-command-get-property command "HOST"))
              (force (org-runbook-command-get-property command "FORCE"))
              (directory (org-runbook-command-get-property command "DIRECTORY"))
              (major-mode (-some--> (org-runbook-command-get-property command "MAJOR-MODE")
                            (intern it)))
              (vars (-some->> (org-runbook-command-get-property command "VARS")
                      (read)
                      (--map (cons (car it) (cdr it)))))
              (shell-command
               (if host
                   (compile-queue-ssh-shell-command-create
                    :name (org-runbook-command-name command)
                    :pty (org-runbook-command-pty command)
                    :host host
                    :major-mode major-mode
                    :default-directory directory
                    :vars vars
                    :command (->> commands (reverse) (-non-nil) (-map #'s-trim) (s-join "; ")))
                   (compile-queue-shell-command-create
                    :name (org-runbook-command-name command)
                    :default-directory directory
                    :major-mode major-mode
                    :vars vars
                    :pty (org-runbook-command-pty command)
                    :command (->> commands (reverse) (-non-nil) (-map #'s-trim) (s-join "; "))))))
         (prog1 (if deferred
                    (deferred:nextc deferred
                      (lambda (&rest _)
                        (when force (compile-queue-clean queue))
                        (compile-queue-schedule queue shell-command)))
                  (when force (compile-queue-clean queue))
                  (compile-queue-schedule queue shell-command))
           (setq commands nil)))))))

(defun org-babel-execute:compile-queue (body params)
  (let* ((buffer (current-buffer))
         (result-type (alist-get :result-params params))
        (placeholder (uuid-string))
        (replace-placeholder (lambda (replacement)
                               (with-current-buffer buffer
                                 (save-excursion
                                   (goto-char (point-min))
                                   (when (search-forward placeholder nil t)
                                     (delete-char (- (length placeholder)))
                                     (insert
                                      (if (string= (cadr result-type) "drawer")
                                          (s-trim replacement)
                                          (s-replace-all '(("\n" . "\n: "))
                                                         (s-trim replacement))))))))))
    (deferred:$
      (compile-queue-execute-org-runbook-command
       (org-runbook--shell-command-for-target
        (org-runbook-target-at-point)))
      (deferred:error it
        (lambda (error)
          (funcall replace-placeholder (error-message-string error))
          (deferred:fail (error-message-string error))))
      (deferred:nextc it
        (lambda (result-buffer)
          (let ((buffer-string (with-current-buffer result-buffer (buffer-string))))
            (funcall replace-placeholder buffer-string)))))
    placeholder))

(provide 'compile-queue-org)
;;; compile-queue-org.el ends here
