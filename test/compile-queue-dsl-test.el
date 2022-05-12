;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'el-mock)
(require 'ert-async)

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'compile-queue (expand-file-name "compile-queue.el"))
(defun -compile-queue-test-extract-command-from-dsl (dsl transformer)
  (--> dsl
       (funcall transformer it)
       (plist-get it :command)
       (eval it)
       (compile-queue-shell-command-full-command it)))

(ert-deftest compile-queue-dsl-simple-expansions ()
  (should
   (equal "echo test"
          (-compile-queue-test-extract-command-from-dsl '(shell "echo test") #'compile-queue-$--shell-command)))

  (should
   (equal "echo 1 2 3"
          (-compile-queue-test-extract-command-from-dsl '(shell "echo" "1" "2" "3") #'compile-queue-$--shell-command)))

  (should
   (equal "echo 123"
          (-compile-queue-test-extract-command-from-dsl '(shell "echo" (concat "1" "2" "3")) #'compile-queue-$--shell-command)))

  (should
   (equal "echo A"
          (-compile-queue-test-extract-command-from-dsl '(! "echo" "A") #'compile-queue-$--shell-command)))
  )

(provide 'compile-queue-dsl-test)
