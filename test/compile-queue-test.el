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
  (let* ((index 0)
         (done done))
    (deferred:$
      (compile-queue:$
        (shell "echo A"))
      (deferred:nextc it
        (lambda (buffer)
          (should (string=
                   "A\n\nProcess"
                   (s-trim
                    (with-current-buffer buffer
                      (buffer-string)))))
          (funcall done))))))

(provide 'compile-queue-test)
