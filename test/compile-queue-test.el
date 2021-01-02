;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'el-mock)
(require 'ert-async)

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'compile-queue (expand-file-name "compile-queue.el"))


(ert-deftest compile-queue-sanity-test ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'compile-queue-$)))

(ert-deftest-async compile-queue-should-run-two-commands (done)
  "Basic path for running two commands sequentially."
  (compile-queue-clean)
  (let* ((index 0)
         (done done))
    (compile-queue-$
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
      (compile-queue-$
        (shell :keep-buffer t "echo A"))
      (deferred:nextc it
        (lambda (buffer)
          (should (string=
                   "A"
                   (s-trim
                    (with-current-buffer buffer
                      (buffer-string)))))
          (funcall done))))))

(ert-deftest-async compile-queue-should-set-execution-status-code (done)
  "Exit status should be set on the execution object"
  (compile-queue-clean)
  (deferred:$
    (compile-queue-$
      (shell
       :before-start
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution)
                     nil)))
       :after-complete
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution)
                     0)))
       "true")
      (shell
       :before-start
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution)
                     nil))
         )
       :after-complete
       (lambda (buffer)
         (set-buffer buffer)
         (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution)
                     1)))
       "false"))
    (deferred:error it
      (lambda (buffer)
        (funcall done)))))

(ert-deftest-async compile-queue-should-reset-state (recovered done)
  "Execution state should be reset between runs"
  (compile-queue-clean)
  (deferred:$
    (compile-queue-$
     (shell
      :before-start
      (lambda (buffer)
        (set-buffer buffer)
        (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution) nil)))
      :after-complete
      (lambda (buffer)
        (set-buffer buffer)
        (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution) 1)))
      "false"))
    (deferred:error it
      (lambda ()
        (compile-queue-$
         (shell
          :before-start
          (lambda (buffer)
            (set-buffer buffer)
            (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution) nil)))
          :after-complete
          (lambda (buffer)
            (set-buffer buffer)
            (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution) 0)))
          "true"))))
    (deferred:nextc it
      (lambda ()
        (funcall recovered)
        (compile-queue-$
         (shell
          :before-start
          (lambda (buffer)
            (set-buffer buffer)
            (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution) nil)))
          :after-complete
          (lambda (buffer)
            (set-buffer buffer)
            (should (eq (compile-queue-execution--status-code compile-queue-delegate-mode--execution) 1)))
          "false"))))
    (deferred:error it
      (lambda (buffer)
        (funcall done)))))

(ert-deftest-async compile-queue-should-defer-scheduling (done)
  "deferred-shell should defer scheduling"
  (compile-queue-clean)
  (let* ((index 0))
    (deferred:$
      (compile-queue-$
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
           (setq index (1+ index))
           (funcall done))
         "echo 3")

       (shell
        :after-complete
        (lambda (buffer)
          (should (eq index 1))
          (setq index (1+ index)))
        "echo 2")))))

(ert-deftest-async compile-queue-should-handle-setting-major-mode (done)
  "compile-queue should handle setting the major mode"
  (compile-queue-clean)
  (let* ((index 0))
    (deferred:$
      (compile-queue-$
        (shell
         :major-mode #'fundamental-mode
         :keep-buffer t
         "echo test"))
      (deferred:nextc it
        (lambda (buffer)
          (set-buffer buffer)
          (set-buffer (compile-queue-buffer-name (compile-queue-current)))
          (funcall done))))))


(ert-deftest-async compile-queue-should-handle-concurrency (done)
  "compile-queue should handle concurrent executions"
  (compile-queue-clean)
  (let* ((index 0))
    (deferred:$
      (compile-queue-$
       (shell
        :after-complete
        (lambda (buffer)
          (set-buffer buffer)
          (should (eq index 3))
          (setq index (1+ index))
          (should (string= "A" (s-trim (buffer-string))))
          (funcall done))
        :major-mode #'fundamental-mode
        :matcher (lambda (_) t)
        "echo A; sleep 2")
       (deferred-shell
         :after-complete
         (lambda (buffer)
           (set-buffer buffer)
           (should (eq index 1))
           (setq index (1+ index))
           (should (string= "B" (s-trim (buffer-string)))))
         :major-mode #'fundamental-mode
         :matcher (lambda (_) t)
         "echo B; sleep 1")
       (shell
        :major-mode #'fundamental-mode
        :matcher (lambda (_) t)
        :after-complete
        (lambda (buffer)
          (set-buffer buffer)
          (should (eq index 2))
          (setq index (1+ index))
          (should (string= "C" (s-trim (buffer-string)))))
        "echo C; sleep 1.5"))
      (deferred:nextc it
        (lambda (buffer)
          (set-buffer buffer)
          (should (eq index 0))
          (setq index (1+ index)))))))

(ert-deftest-async compile-queue-clean (after-complete)
  "compile-queue-clean should work correctly"
  (let* ((queue-name "queue")
         (queue (compile-queue-current queue-name)))
    (compile-queue-$
     queue-name
     (shell
      :after-complete
      (lambda (buffer)
        (funcall after-complete))
      :major-mode #'fundamental-mode
      "echo A; sleep 2")
     (shell
      :major-mode #'fundamental-mode
      "echo A; sleep 2"))
    (should (compile-queue--target-execution queue))
    (should (compile-queue--scheduled queue))
    (compile-queue-clean queue)
    (should-not (compile-queue--target-execution queue))
    (should-not (compile-queue--scheduled queue))))

(ert-deftest-async compile-queue-trail-window (done)
  "compile-queue-clean should work correctly"
  (let* ((queue-name "queue")
         (queue (compile-queue-current queue-name)))
    (compile-queue-$
      (shell
       :after-complete
       (lambda (buffer)
         (set-buffer (window-buffer (selected-window)))
         (should (string= (s-trim (buffer-string))
                          "A"))
         (should (eq (window-point (selected-window)) (point-max)))
         (funcall done))
       :major-mode #'fundamental-mode
       "echo A; sleep 2"))
    (switch-to-buffer (compile-queue-buffer-name (compile-queue-current compile-queue-root-queue)))
    (set-window-point (selected-window) (point-max))))

(ert-deftest compile-queue-shell-env-ambiguous ()
  ""
  (should-not (compile-queue-$--shell-env-ambiguous '("test" . "value")))
  (should-not (compile-queue-$--shell-env-ambiguous '(("test" . "value"))))
  (should-not (compile-queue-$--shell-env-ambiguous '(("test" . nil))))
  (should-not (compile-queue-$--shell-env-ambiguous '(("test"))))
  (should (compile-queue-$--shell-env-ambiguous '("test" . nil)))
  (should-not (compile-queue-$--shell-env-ambiguous '((concat "A" "test") . (concat "B" "test"))))
  (should-not (compile-queue-$--shell-env-ambiguous '((concat "A" "test") . (concat "C" "test"))))
  (should-not (compile-queue-$--shell-env-ambiguous '((concat (concat "A") "test") . (concat "C" (concat "test"))))))

(ert-deftest-async compile-queue-$-simple ()
  ""
  (let ((env '("KeyA" . "KeyB")))
    (compile-queue-$ (shell :env env "echo" "TEST"))))

(ert-deftest-async compile-queue-$-major-mode ()
  ""
  (compile-queue-$ (shell :major-mode #'fundamental-mode "echo test")))

(ert-deftest-async compile-queue-$-env-literal ()
  ""
  (compile-queue-$ (shell :major-mode #'fundamental-mode "echo test"))
  (compile-queue-$ (shell "echo test"))
  (compile-queue-$ (shell :env ("KeyA" . "ValueA") "echo test"))
  (compile-queue-$ (shell :env (("KeyA" . "ValueA")) "echo test"))
  (compile-queue-$ (shell :env (("KeyA" . "ValueA")) "echo test"))
  (compile-queue-$ (shell :env (("KeyA" . "ValueA") ("KeyB" . "ValueB")) "echo test"))
  (compile-queue-$ (shell :env (("KeyA" . "ValueA") ("KeyB" . "ValueB")) "echo" "TEST")))

(ert-deftest-async compile-queue-$-env-variable ()
  ""
  (let ((env '(("KeyA" . "ValueA") ("KeyB" . "ValueB"))))
    (compile-queue-$ (shell :env env "echo test"))))

(provide 'compile-queue-test)
