;;; compile-queue.el --- Org mode for runbooks -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 0.1
;; Keywords: convenience, processes, terminals, files
;; Package-Requires: ((emacs "25.1") (seq "2.3") (f "0.20.0") (s "1.12.0") (dash "2.17.0") (mustache "0.24") (ht "0.9"))
;; URL: https://github.com/tyler-dodge/compile-queue
;; Git-Repository: git://github.com/tyler-dodge/compile-queeue.git
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;;;
;;; Commentary:
;;; Code:

(require 'uuid)
(require 'dash)
(require 'ht)
(require 'cl-lib)
(require 'deferred)

(defgroup compile-queue nil
  "Customization Group for Compile Queue.")

(defcustom compile-queue-root-queue
  "*compile-queue*"
  "Name of the compile queue that is used by default if a compile queue is not specified when calling."
  :safe t
  :type 'stringp
  :group 'compile-queue)

(defcustom compile-queue-shell-default-major-mode
  #'fundamental-mode
  "The major-mode used in the compile-queue buffer if the `compile-queue-command' does not specify a queue."
  :safe t
  :type 'functionp
  :group 'compile-queue)

(defvar compile-queue--name-map (ht)
  "Map from compile-queue name to the corresponding compile-queue struct")

(defvar-local compile-queue-shell-command--process-filter-delegate nil
  "The process filter that compile-queue delegates to while running.")

(defvar-local compile-queue-shell-command--process-sentinel-delegate nil
  "The process sentinal that compile-queue delegates to while running.")

(defvar-local compile-queue nil
  "The compile-queue that the current buffer is referencing.")

(cl-defstruct (compile-queue (:constructor compile-queue-create))
  (id (uuid-string))
  name
  buffer-name
  execution
  scheduled)

(cl-defstruct (compile-queue-command (:constructor compile-queue-command-create))
  (id (uuid-string))
  name
  before-start
  deferred
  after-complete)

(cl-defstruct (compile-queue-execution (:constructor compile-queue-execution-create))
  (id (uuid-string))
  promise
  queue
  exit-status)

(cl-defstruct (compile-queue-promise (:constructor compile-queue-promise-create))
  (id (uuid-string))
  command
  deferred)

(cl-defstruct (compile-queue-shell-command (:constructor compile-queue-shell-command-create )
                                           (:include compile-queue-command))
  command
  env
  major-mode
  default-directory
  buffer-name)


(defvar compile-queue--converters
  '(
    compile-queue:$--deferred-shell-command
    compile-queue:$--shell-command)
  "Converters for `compile-queue:$'. Each are applied sequentially until one returns non-nil.")

(cl-defun compile-queue:$-convert (list)
  (cl-loop for converter in compile-queue--converters
           do
           (when-let (result (funcall converter list))
             (cl-return-from compile-queue:$-convert result))))

(cl-defun compile-queue:$-command (queue-var command)
  (if-let ((output-command (compile-queue:$-convert command)))
      (if  (plist-get output-command :deferred)
          `(setq it (deferred:nextc it
                      (lambda (&rest arg)
                        (compile-queue-schedule
                         ,queue-var
                         ,(plist-get output-command :command)))))
        `(setq it (compile-queue-schedule ,queue-var ,output-command)))
    `(setq it ,command)))

(defmacro compile-queue:$ (queue-name &rest commands)
  "The main entrypoint to defining compile-queue-commands."
  (declare (debug t)
           (indent 0))
  (let* ((queue-name-is-queue (stringp queue-name))
         (queue-var (make-symbol "queue"))
         (commands (->> (if queue-name-is-queue commands (append (list queue-name) commands))
                        (--map (compile-queue:$-command queue-var it)))))
    `(let* ((,queue-var
             (compile-queue--by-name ,(if queue-name-is-queue queue-name compile-queue-root-queue)))
            (it nil))
       ,@commands)))

(defun compile-queue:$--deferred-shell-command (list)
  (when (memq (car list) '(deferred-shell !deferred))
    (let* ((plist-end (1+ (--find-last-index (symbolp it) list)))
           (plist (-slice list 1 plist-end))
           (command-rest (-slice list plist-end)))
      (list
       :deferred t
       :command
       `(compile-queue-shell-command-create ,@plist
                                            :command
                                            (concat ,@command-rest))))))

(defun compile-queue:$--shell-command (list)
  (when (memq (car list) (list 'shell '!))
    (let* ((plist-end (1+ (--find-last-index (symbolp it) list)))
           (plist (-slice list 1 plist-end))
           (command-rest (-slice list plist-end)))
      `(compile-queue-shell-command-create ,@plist
                                           :command
                                           (concat ,@command-rest)))))
(defun compile-queue--by-name (name)
  (if (compile-queue-p name) name
    (or (ht-get compile-queue--name-map name)
        (--doto (compile-queue-create :name name)
          (ht-set! compile-queue--name-map name it)))))

(defun compile-queue--callback (execution)
  (lambda (callback)
    (save-selected-window
      (save-mark-and-excursion
        (funcall callback execution)))))

(defun compile-queue-command--execute (promise queue)
  "Executes COMMAND in a buffer related to QUEUE."
  (let ((command (compile-queue-promise-command promise)))
    (compile-queue-shell-command--init-buffer command)
    (setq-local compile-queue queue)
    (let ((execution (compile-queue-execution-create
                      :promise promise
                      :queue queue)))
      (prog1 execution
        (setf (compile-queue-execution queue) execution)
        (-some-> (compile-queue-command-before-start command)
          (compile-queue--callback execution))
        (start-process-shell-command
         (compile-queue-shell-command--name command)
         (compile-queue-shell-command--buffer-name command)
         (compile-queue-shell-command-command command))
        (--doto (get-buffer-process (current-buffer))
          (when it
            (setq-local compile-queue-shell-command--process-filter-delegate (process-filter it))
            (setq-local compile-queue-shell-command--process-sentinal-delegate (process-sentinel it))
            (set-process-sentinel it #'compile-queue-shell-command--process-sentinel)
            (set-process-filter it #'compile-queue-shell-command--process-filter)))))))

(defun compile-queue-execute (queue)
  "Execute the next command in commands on QUEUE."
  (-let (((next-command . rest) (-some--> (compile-queue-scheduled queue) (if (not (listp it)) (list it) it))))
    (if (not next-command)
        (setf (compile-queue-execution queue) nil)
      (setf (compile-queue-scheduled queue) rest)
      (setf (compile-queue-execution queue)
            (compile-queue-command--execute next-command queue)))))

(defun compile-queue-schedule (compile-queue command)
  "Appends the COMMAND to the commands on QUEUE."
  (let ((promise (deferred:new)))
    (setf
     (compile-queue-scheduled compile-queue)
     (append (--> (compile-queue-scheduled compile-queue) (if (listp it) it (list it)))
             (list (compile-queue-promise-create
                    :command command
                    :deferred promise)) nil))

    (when (not (compile-queue-execution compile-queue))
      (compile-queue-execute compile-queue))
    promise))

(defun compile-queue-current (&optional object)
  (pcase object
    ((pred processp) (buffer-local-value 'compile-queue (process-buffer object)))
    ((pred bufferp) (buffer-local-value 'compile-queue object))
    ((pred windowp) (buffer-local-value 'compile-queue object))
    ('nil (buffer-local-value 'compile-queue (current-buffer)))
    (_ (error "Unexpected type passed to compile-queue-current %s" object))))

(defun compile-queue-shell-command--buffer-name (command)
  (or (compile-queue-shell-command-buffer-name command)
      (-some--> (compile-queue-shell-command-name command) (concat " *" it "*"))
      (concat " *" (s-truncate 10 (compile-queue-shell-command-command command)) "*")))

(defun compile-queue-shell-command--name (command)
  (or (compile-queue-shell-command-buffer-name command)
      (-some--> (compile-queue-shell-command-name command) it)
      (concat (s-truncate 10 (compile-queue-shell-command-command command)))))

(defun compile-queue-shell-command--init-buffer (command)
  "Initialize the buffer for the execution. Replaces the buffer if it already exists."
  (pcase-let* (((cl-struct compile-queue-shell-command (default-directory directory)) command)
               (buffer-name (compile-queue-shell-command--buffer-name command)))
    (unless (get-buffer buffer-name) (generate-new-buffer buffer-name))
    (set-buffer buffer-name)

    (-some--> (get-buffer-process (current-buffer))
      (when (process-live-p it) (kill-process it)))
    (buffer-disable-undo)
    (setq-local inhibit-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer))

    (funcall
     (or (compile-queue-shell-command-major-mode command)
         compile-queue-shell-default-major-mode))

    (prog1 (current-buffer)
      (when directory (setq-local default-directory directory))
      (compile-queue-view-mode 1)
      (goto-char (point-max)))))

(defun compile-queue-shell-command--process-filter (process output)
  (-some--> compile-queue-shell-command--process-filter-delegate
    (funcall it process output)))

(defun compile-queue-shell-command--process-sentinel (process status)
  "Delegating sentinel for compile-queue. Handles notifying compile queue on process completion."
  (-some--> compile-queue-shell-command--process-sentinel-delegate
    (funcall it process status))
  (when-let ((queue (compile-queue-current process)))
    (let ((execution (-some-> queue compile-queue-execution)))
      (-some-> execution
        compile-queue-execution-promise
        compile-queue-promise-command
        compile-queue-command-after-complete
        (compile-queue--callback execution))
      (-some--> execution
        (compile-queue-execution-promise it)
        (compile-queue-promise-deferred it)
        (deferred:callback it execution)))
    (compile-queue-execute queue)))

(define-minor-mode compile-queue-view-mode "Mode for viewing the output of the current execution of a compile-queue" nil)

(provide 'compile-queue)
