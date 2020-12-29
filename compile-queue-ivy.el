;;; -*- lexical-binding: t -*-
(require 'ivy)
(require 'dash)

(ivy-set-actions
 'compile-queue-ivy-running
 '(
   ("b" compile-queue-ivy-action--execution-display-buffer "Display Base Buffer")
   ("r" compile-queue-ivy-action--execution-restart "Restart Execution")
   ("o" compile-queue-ivy-action--execution-display-queue "Display Queue Buffer")
   ("K" compile-queue-ivy-action--execution-kill "Kill Execution")))

(defun compile-queue-ivy-running ()
  (interactive)
  (ivy-read
   "Running:"
   (-some->> (process-list)
     (--map (process-buffer it))
     (--map (-some--> it
              (buffer-local-value 'compile-queue-delegate-mode--execution it)
              (cons (concat  (-some-> (compile-queue-execution--queue it) compile-queue--name) ": " (compile-queue-execution-name it)) it)))
     (--filter it))
   :require-match t
   :caller 'compile-queue-ivy-running
   :action #'compile-queue-ivy-action--execution-display-queue))

(defun compile-queue-ivy-action--execution-display-buffer (ivy-response)
  (-let [(_ . execution) ivy-response]
    (or (compile-queue-execution-p execution)
        (error "Expected ivy-response to be a cons with cdr being a compile-queue-execution: %S"
               execution))
    (-some-> execution compile-queue-execution--buffer display-buffer)))

(defun compile-queue-ivy-action--execution-display-queue (ivy-response)
  (-let [(_ . execution) ivy-response]
    (or (compile-queue-execution-p execution)
        (error "Expected ivy-response to be a cons with cdr being a compile-queue-execution: %S"
               execution))
    (-some-> execution compile-queue-execution-queue
             compile-queue-buffer-name
             display-buffer)))

(defun compile-queue-ivy-action--execution-kill (ivy-response)
  (-let [(_ . execution) ivy-response]
    (or (compile-queue-execution-p execution)
        (error "Expected ivy-response to be a cons with cdr being a compile-queue-execution: %S"
               execution))
    (-some-> execution compile-queue-execution--buffer get-buffer-process kill-process)))

(defun compile-queue-ivy-action--execution-restart (ivy-response)
  (-let [(_ . execution) ivy-response]
    (or (compile-queue-execution-p execution)
        (error "Expected ivy-response to be a cons with cdr being a compile-queue-execution: %S"
               execution))
    (compile-queue-execution-restart execution)))

(provide 'compile-queue-ivy)
