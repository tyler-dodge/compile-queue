;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'el-mock)
(require 'ert-async)

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'compile-queue (expand-file-name "compile-queue.el"))


(ert-deftest compile-queue-sanity-test ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'compile-queue:$)))

(ert-deftest-async compile-queue-should-run-two-commands (done)
  "Basic path for running two commands sequentially."
  (compile-queue-clean)
  (let* ((index 0)
         (done done))
    (compile-queue:$
      (shell :after-complete
             (lambda (buffer)
               (should (eq index 0))
               (setq index (1+ index))) "echo A")
      (shell :after-complete
             (lambda (buffer)
               (should (eq index 1))
               (setq index (1+ index))) "echo B")
      (deferred:nextc it
        (lambda (buffer)
          (should (eq index 2))
          (funcall done))))))

(ert-deftest-async compile-queue-should-output-buffer-in-deferred (done)
  "The buffer should be provided via a deferred object at the end of the chain."
  (compile-queue-clean)
  (let* ((index 0)
         (done done))
    (deferred:$
      (compile-queue:$
        (shell "echo A"))
      (deferred:nextc it
        (lambda (buffer)
          (should (string=
                   "A\n\nProcess echo A finished"
                   (s-trim
                    (with-current-buffer buffer
                      (buffer-string)))))
          (funcall done))))))

(ert-deftest-async compile-queue-should-set-execution-status-code (done)
  "Exit status should be set on the execution object"
  (compile-queue-clean)
  (deferred:$
    (compile-queue:$
      (shell
       :before-start
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution-status-code compile-queue--execution)
                     nil)))
       :after-complete
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution-status-code compile-queue--execution)
                     0)))
       "true")
      (shell
       :before-start
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution-status-code compile-queue--execution)
                     nil))
         )
       :after-complete
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution-status-code compile-queue--execution)
                     1)))
       "false"))
    (deferred:error it
      (lambda (buffer)
        (funcall done)))))

(ert-deftest-async compile-queue-should-reset-state (recovered done)
  "Execution state should be reset between runs"
  (compile-queue-clean)
  (deferred:$
    (compile-queue:$
      (shell
       :before-start
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution-status-code compile-queue--execution) nil)))
       :after-complete
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution-status-code compile-queue--execution) 1)))
       "false"))
    (deferred:error it
      (lambda ()
        (compile-queue:$
          (shell
           :before-start
           (lambda (buffer)
             (set-buffer buffer)
             (should (eq (compile-queue-execution-status-code compile-queue--execution) nil)))
           :after-complete
           (lambda (buffer)
             (set-buffer buffer)
             (should (eq (compile-queue-execution-status-code compile-queue--execution) 0)))
           "true"))))
    (deferred:nextc it
      (lambda ()
        (funcall recovered)
        (compile-queue:$
          (shell
           :before-start
           (lambda (buffer)
             (set-buffer buffer)
             (should (eq (compile-queue-execution-status-code compile-queue--execution) nil)))
           :after-complete
           (lambda (buffer)
             (set-buffer buffer)
             (should (eq (compile-queue-execution-status-code compile-queue--execution) 1)))
           "false"))))
    (deferred:error it
      (lambda (buffer)
        (funcall done)))))

(ert-deftest-async compile-queue-should-defer-scheduling (done)
  "deferred-shell should defer scheduling"
  (compile-queue-clean)
  (let* ((index 0))
    (deferred:$
      (compile-queue:$
        (shell
         :after-complete
         (lambda (buffer)
           (should (eq index 0))
           (setq index (1+ index)))
         "echo 1")
        (deferred-shell
          :after-complete
          (lambda (buffer)
            (should (eq index 2))
            (setq index (1+ index)))
          "echo 3")

        (shell
         :after-complete
         (lambda (buffer)
           (should (eq index 1))
           (setq index (1+ index)))
         "echo 2")
        )
      (deferred:nextc it
        (lambda (buffer)
          (funcall done))))))

(ert-deftest-async compile-queue-should-handle-setting-major-mode (done)
  "compile-queue should handle setting the major mode"
  (compile-queue-clean)
  (let* ((index 0))
    (deferred:$
      (compile-queue:$
        (shell
         :major-mode #'fundamental-mode
         "sleep 1"))
      (deferred:nextc it
        (lambda (buffer)
          (set-buffer buffer)
          (set-buffer (compile-queue--buffer-name (compile-queue-current)))
          (funcall done))))))

(provide 'compile-queue-test)
