;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'el-mock)

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'compile-queue (expand-file-name "compile-queue.el"))


(ert-deftest compile-queue-sanity-test ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'compile-queue:$)))

(provide 'compile-queue-test)
