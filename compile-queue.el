;;; compile-queue.el --- The package for running commands while viewing their output -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 1.0
;; Keywords: convenience, processes, terminals, files
;; Package-Requires: ((emacs "26.2") (uuid "0.0.3") (dash "2.17.0") (deferred "0.5.1") (s "1.12.0") (ht "2.3"))
;; URL: https://github.com/tyler-dodge/compile-queue
;; Git-Repository: git://github.com/tyler-dodge/compile-queeue.git
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;;;
;;; Commentary:
;; The package for running lists of commands, while viewing their output.
;; Main entrypoint is meant to be the `compile-queue-$' macro
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'pcase)
(require 'deferred)
(require 's)
(require 'rx)
(require 'comint)
(require 'uuid)
(require 'compile-queue-utils "compile-queue-utils.el")
(require 'compile-queue-structs "compile-queue-structs.el")
(require 'compile-queue-dsl "compile-queue-dsl.el")
(require 'compile-queue-view-mode "compile-queue-view-mode.el")
(require 'compile-queue-delegate-mode "compile-queue-delegate-mode.el")
(require 'compile-queue-org "compile-queue-org.el")
(require 'compile-queue-shell-command "compile-queue-shell-command.el")

(defgroup compile-queue nil
  "Customization Group for Compile Queue."
  :group 'convenience)

(defcustom compile-queue-default-process-connection-type nil
  "The default value to set for `process-connection-type' for compile-queue commands.
Can be overriden by specifying pty on the `compile-queue-shell-command'."
  :safe t
  :type 'boolean
  :group 'compile-queue)

(defcustom compile-queue-root-queue "compile-queue"
  "Name of the compile queue that is used by default if a compile queue is not specified when calling."
  :safe t
  :type 'string
  :group 'compile-queue)


(defcustom compile-queue-garbage-collect-time
  10.0
  "Time in seconds before the buffer is garbage collected automatically.
Set to nil to disable garbage collection."
  :safe t
  :type 'number
  :group 'compile-queue)


(defcustom compile-queue-shell-default-major-mode
  #'comint-mode
  "The `major-mode' used in the compile-queue buffer if the `compile-queue-command' does not specify a `major-mode'."
  :safe t
  :type 'function
  :group 'compile-queue)

(defcustom compile-queue-max-buffer-line-limit
  nil
  "The max size for a compile queue buffer. Deletes old lines to keep within limit."
  :type 'number
  :safe t
  :group 'compile-queue)

(defvar compile-queue--echo-output nil
  "Used for batch output.")

(defvar compile-queue--name-ht (ht)
  "Hash table from compile-queue name to the corresponding compile-queue struct.")


(defun compile-queue-group-execution-scheduled (group)
  "Return the scheduled executions in GROUP."
  (-some--> (compile-queue-group-execution--scheduled group) (if (not (listp it)) (list it) it)))

(defun compile-queue-item-before-start (command)
  (pcase command
    ((pred (compile-queue-group-p)
            (compile-queue-group--before-start command)))
    ((pred (compile-queue-command-p))
     (compile-queue-command--before-start command))))

(defun compile-queue-item-after-complete (command)
  (pcase command
    ((pred (compile-queue-group-p)
            (compile-queue-group--after-complete command)))
    ((pred (compile-queue-command-p))
     (
      compile-queue-command--after-complete command))))

(defun compile-queue-clean-all ()
  "Cleans every known queue."
  (interactive)
  (->> (ht-values compile-queue--name-ht)
       (-map #'compile-queue-clean)))

(defun compile-queue-skip (&optional queue)
  "Skip waiting for the current execution on QUEUE.
Start the next execution if one is available.
If nothing is scheduled, this is a no-op."
  (interactive)
  (let ((queue (or queue (compile-queue-current) (compile-queue--by-name compile-queue-root-queue))))
    (when (compile-queue--scheduled queue)
      (setf (compile-queue--target-execution queue) nil)
      (compile-queue-execute queue))))

(defun compile-queue-clean (&optional queue)
  "Cleans up QUEUE or the result of `compile-queue-current'.
Kills the current execution."
  (interactive)
  (when-let ((queue (or (-some-> queue compile-queue--by-name)
                        (compile-queue-current)
                        (compile-queue--by-name compile-queue-root-queue))))
    (let ((process (-some-> queue compile-queue--target-execution compile-queue-execution-buffer get-buffer-process)))
      (when (process-live-p process)
        (kill-process process)))
    (--each
        (->>
         (compile-queue--scheduled queue)
         (-map #'compile-queue-promise--command)
         (-map #'compile-queue-item-after-complete)
         (-non-nil))
      ;; ignoring errors because they don't matter while cleaning
      (ignore-errors (funcall it nil)))
    (setf (compile-queue--target-execution queue) nil)
    (setf (compile-queue--scheduled queue) nil))
  (force-mode-line-update t)
  t)

(defun compile-queue-accept-process-output (compile-queue)
  "Waits until the compile queue is finished."
  (let ((compile-queue--echo-output t))
    (while (compile-queue--target-execution 
            (compile-queue-current compile-queue))
      (accept-process-output nil 1 nil nil)
      (redisplay t))))

(defun compile-queue--by-name (name)
  "Find or create and register the queue with NAME.
Returns a `compile-queue' struct"
  (if (compile-queue-p name) name
    (or (ht-get compile-queue--name-ht name)
        (--doto (compile-queue-create :name name)
          (ht-set! compile-queue--name-ht name it)))))

(defun compile-queue--callback (execution)
  "Return a callback for the EXECUTION.
The callback expects a function or a symbol that
takes the execution's buffer as an argument."
  (lambda (callback)
    (save-selected-window
      (save-mark-and-excursion
        (funcall callback (compile-queue-execution--buffer execution))))))



(defun compile-queue-group-create-execution (group &optional parent)
  (compile-queue-group-execution-create
   :group group
   :scheduled (->> (compile-queue-group--commands group)
                   (--map (compile-queue-promise-create
                           :command it
                           :deferred (deferred:new))))
   :parent-group-execution
   parent))


(defun compile-queue-execute (queue)
  "Execute the next command in commands on QUEUE."
  (let* ((target-group
          (-some--> queue
            (compile-queue--target-execution it)
            (pcase it
              ((pred compile-queue-group-execution-p) it)
              ((pred compile-queue-execution-p)
               (compile-queue-execution--group-execution it))
              (_ nil)))))
    (cond
     (target-group
      (if-let (parent (cl-loop
                       for parent = target-group
                       then (compile-queue-group-execution--parent-group-execution parent)
                       until (or (not parent) (compile-queue-group-execution--scheduled parent))
                       finally return parent))
          (let ((next-command (pop (compile-queue-group-execution--scheduled parent))))
            (pcase (compile-queue-promise--command next-command)
              ((pred (compile-queue-group-p))
               (setf (compile-queue--target-execution queue)
                     (compile-queue-group-create-execution (compile-queue-promise--command next-command) parent))
               (compile-queue-execute queue))
              (_
               (setf (compile-queue--target-execution queue)
                     (compile-queue-command--execute (compile-queue-promise--command next-command) next-command queue parent)))))
        ;; else
        (setf (compile-queue--target-execution queue) nil)
        (compile-queue-execute queue)))
     (t
      (-let (((next-command . rest) (-some--> (compile-queue--scheduled queue) (if (not (listp it)) (list it) it))))
        (if (not next-command)
            (setf (compile-queue--target-execution queue) nil)
          (cond
           ((-some-> next-command compile-queue-promise--command compile-queue-group-p)
            (setf (compile-queue--scheduled queue) rest)
            (setf (compile-queue--target-execution queue)
                  (compile-queue-group-create-execution (compile-queue-promise--command next-command)))
            (compile-queue-execute queue))
           ((-some-> next-command compile-queue-promise--command compile-queue-command-p)
            (setf (compile-queue--scheduled queue) rest)
            (setf (compile-queue--target-execution queue)
                  (compile-queue-command--execute (compile-queue-promise--command next-command) next-command queue nil)))
           (t (error "Unexpected type attempted to be executed: %S" next-command)))))))))

(defun compile-queue-schedule (queue command)
  "Append the COMMAND to the commands on QUEUE."
  (let ((promise (deferred:new)))
    (setf
     (compile-queue--scheduled queue)
     (append (--> (compile-queue--scheduled queue) (if (listp it) it (list it)))
             (list (compile-queue-promise-create
                    :command command
                    :deferred promise)) nil))
    (if (not (compile-queue--target-execution queue))
        (progn
          (compile-queue-execute queue)))
    promise))

(defun compile-queue-buffer-name (queue)
  "Return the compile-queue-view-mode buffer name of the QUEUE.
Uses the `compile-queue--name' if `compile-queue--buffer-name' is unset."
  (or (compile-queue--buffer-name queue)
      (-some--> (compile-queue--name queue) (concat "*" it "*"))
      (user-error "Broken queue with no name: %S" queue)))

(defun compile-queue-current (&optional object)
  "Return the compile queue for the given OBJECT.
OBJECT can be either a process, buffer, or window.

If OBJECT is nil, return the value of `compile-queue-delegate-mode--queue'
for the current buffer."
  (pcase object
    ((pred compile-queue-p) object)
    ((pred processp) (buffer-local-value 'compile-queue-delegate-mode--queue (process-buffer object)))
    ((pred bufferp) (buffer-local-value 'compile-queue-delegate-mode--queue object))
    ((pred windowp) (buffer-local-value 'compile-queue-delegate-mode--queue object))
    ((pred stringp) (compile-queue--by-name object))
    ('nil (buffer-local-value 'compile-queue-delegate-mode--queue (current-buffer)))
    (_ (error "Unexpected type passed to compile-queue-current %s" object))))

(defun compile-queue-allows-output-p (compile-queue execution)
  (-contains-p
   (compile-queue--outputting-executions compile-queue)
   (compile-queue-execution--id execution)))

(defun compile-queue-limit-output-to-target (compile-queue)
  (setf (compile-queue--outputting-executions compile-queue)
        (list (compile-queue-execution--id (compile-queue--target-execution compile-queue)))))

(defun compile-queue-update-status (compile-queue new-status)
  (pcase new-status
    ((or 'error 'success) new-status)
    (_ (error "Unexpected status passed to compile-queue %S" new-status))))

(provide 'compile-queue)
;;; compile-queue.el ends here
