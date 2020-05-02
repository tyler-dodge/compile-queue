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
;; GNU General Public License for more details

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;;;
;;; Commentary:
;; Main entrypoint is meant to be the compile-queue:$ macro
;;
;;
;;
;;; Code:

(require 'uuid)
(require 'dash)
(require 'ht)
(require 'cl-lib)
(require 'deferred)

(defgroup compile-queue nil
  "Customization Group for Compile Queue.")

(defcustom compile-queue-root-queue
  "compile-queue"
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

(defvar-local compile-queue--execution nil
  "The execution that is related to the current buffer's process.")

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

(defalias 'c:$ 'compile-queue:$)
(defmacro compile-queue:$ (queue-name &rest commands)
  "Small DSL for chaining commands on the compile-queue.
Fully compatible with deferred.el's deferred:$


QUEUE-NAME is optional.

Currently there are 2 special types:

(shell &rest COMMAND) - run the command specified by joining the list of COMMAND with spaces
(! &rest COMMAND)

(deferred-shell &rest COMMAND) - waits to schedule the command until the deferred chain before this has already completed.
(!deferred &rest COMMAND)

The deferred promise receives the output buffer as the argument once the execution completes.

Example:
(compile-queue:$
(shell \"echo A\")
(deferred-shell \"echo C\")
(shell \"echo B\"))

This example will end up displaying C because the command \"echo C\" is not scheduled until after \"echo A\" finishes execution
whereas \"echo B\" is scheduled before \"echo B\" starts executing.

Example:
(compile-queue:$
(shell \"echo A\")
(deferred:nextc it (lambda (buffer) (set-buffer buffer) (message \"%s\" (s-trim (buffer-string))))))

This example shows how compile-queue can be chained with deferred.el.
"
(declare (debug t)
         (indent 0))
(let* ((queue-name-is-queue (stringp queue-name))
       (queue-var (make-symbol "queue"))
       (commands (->> (if queue-name-is-queue commands (append (list queue-name) commands))
                      (--map (compile-queue:$-command queue-var it)))))
  `(let* ((,queue-var
           (compile-queue--by-name ,(if queue-name-is-queue queue-name compile-queue-root-queue)))
          (it
           ;; Might be a better way of doing this, but need to check if it is lexically bound.
           (ignore-errors it)))
     (if (deferred-p it)
         (progn
           (deferred:nextc it (lambda () ,@commands)))
       ,@commands))))

(defvar compile-queue--converters
  '(compile-queue:$--deferred-shell-command
    compile-queue:$--shell-command)
  "Converters for `compile-queue:$'. Each are applied sequentially until one returns non-nil.")

(cl-defun compile-queue:$-convert (list)
  "Runs each of the converters in `compile-queue--converters' against LIST. Returns the first match."
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
                                            (s-join " " (list ,@command-rest)))))))

(defun compile-queue:$--shell-command (list)
  (when (memq (car list) (list 'shell '!))
    (let* ((plist-end (1+ (--find-last-index (symbolp it) list)))
           (plist (-slice list 1 plist-end))
           (command-rest (-slice list plist-end)))
      `(compile-queue-shell-command-create ,@plist
                                           :command
                                           (s-join " " (list ,@command-rest))))))
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
    (set-buffer (compile-queue-shell-command--init-buffer command))
    (when (not (memq #'compile-queue--forward-change after-change-functions))
      (setq-local after-change-functions (append (-some->> after-change-functions (-drop-last 1)) '(compile-queue--forward-change t))))
    (setq-local compile-queue queue)
    (let ((execution (compile-queue-execution-create
                      :promise promise
                      :queue queue)))
      (setq-local compile-queue--execution execution)
      (prog1 execution
        (setf (compile-queue-execution queue) execution)
        (-some-> (compile-queue-command-before-start command)
          (compile-queue--callback execution))
        (let ((process (start-process-shell-command
                        (compile-queue-shell-command--name command)
                        (compile-queue-shell-command--buffer-name command)
                        (compile-queue-shell-command-command command))))
          (--doto process
            (when it
              (setq-local compile-queue-shell-command--process-filter-delegate (process-filter it))
              (setq-local compile-queue-shell-command--process-sentinal-delegate (process-sentinel it))
              (set-process-sentinel it #'compile-queue-shell-command--process-sentinel)
              (set-process-filter it #'compile-queue-shell-command--process-filter)))
          (compile-queue--update-buffer queue))))))

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

(defun compile-queue--buffer-name (compile-queue)
  (or (compile-queue-buffer-name compile-queue)
      (-some--> (compile-queue-name compile-queue) (concat "*" it "*"))
      (user-error "Broken queue with no name: %S" compile-queue)))

(defun compile-queue-execution--buffer-name (execution)
  (let ((command (compile-queue-promise-command
                  (compile-queue-execution-promise execution))))
    (pcase command
      ((cl-struct compile-queue-shell-command)
       (compile-queue-shell-command--buffer-name command)))))


(defun compile-queue--update-buffer (compile-queue)
  (when-let ((execution-buffer-name (-> compile-queue compile-queue-execution compile-queue-execution--buffer-name)))
    (let* ((buffer-name (compile-queue--buffer-name compile-queue))
           (execution-buffer (get-buffer execution-buffer-name))
           (old-buffer (get-buffer buffer-name))
           (windows (get-buffer-window-list old-buffer))
           (new-buffer (generate-new-buffer " *temp-queue*")))
      (when old-buffer
        (--each windows
          (set-window-buffer it new-buffer))
        (kill-buffer old-buffer))
      (set-buffer new-buffer)
      (setq-local buffer-read-only t)
      (unless (string= buffer-name (buffer-name))
        (rename-buffer buffer-name))
      (goto-char (point-max)))))

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
  (let* ((execution-buffer (process-buffer process))
         (compile-queue (compile-queue-current process))
         (compile-queue-buffer (get-buffer (compile-queue--buffer-name compile-queue)))
         (execution (buffer-local-value 'compile-queue--execution execution-buffer))
         (delegate
          (-some->> execution-buffer
            (buffer-local-value 'compile-queue-shell-command--process-filter-delegate))))
    (if (and execution (eq (-> execution compile-queue-execution-id)
                           (-> compile-queue compile-queue-execution compile-queue-execution-id)))
        (progn (let* ((compile-queue-end-pt (with-current-buffer (process-buffer process) (point-max)))
                      (scroll-to-end (->> (get-buffer-window-list compile-queue-buffer)
                                          (--filter (eq (window-point it) compile-queue-end-pt))))
                      (mark (1+ (- (process-mark process) (with-current-buffer (process-buffer process) (point-min))))))
                 (-some--> delegate (funcall it process output))
                 (set-buffer (compile-queue--buffer-name (compile-queue-current)))

                 (let ((pt-max (point-max)))
                   (--each scroll-to-end
                     (set-window-point it pt-max)))))
      (-some--> delegate (funcall it process output)))))

(defun compile-queue-shell-command--process-sentinel (process status)
  "Delegating sentinel for compile-queue. Handles notifying compile queue on process completion."
  (let ((delegate
         (-some->> process
           (compile-queue-current)
           (compile-queue-execution)
           (compile-queue-execution--buffer-name)
           (get-buffer)
           (buffer-local-value 'compile-queue-shell-command--process-sentinel-delegate))))
    (-some--> delegate (funcall it process status))
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
          (deferred:callback it (get-buffer (compile-queue-execution--buffer-name execution)))))
      (compile-queue-execute queue))))

(define-minor-mode compile-queue-view-mode "Mode for viewing the output of the current execution of a compile-queue" nil)

(defun compile-queue--forward-change (beg end length)
  (when (eq compile-queue--execution (compile-queue-execution compile-queue))
    (if (< length 0)
        (let* ((length (abs length))
               (lhs (buffer-substring (- beg length) beg))
               (rhs (buffer-substring beg (+ beg length))))
          (set-buffer (compile-queue--buffer-name compile-queue))
          (goto-char beg)
          (cond
           ((string= (buffer-substring beg (+ beg length)))
            (let ((inhibit-read-only t))
              (delete-char (- length))!))
           ((string= (buffer-substring (- beg length) beg) lhs)
            (let ((inhibit-read-only t))
              (delete-char length))))))
    (let ((text (buffer-substring beg end)))
      (set-buffer (compile-queue--buffer-name compile-queue))
      (goto-char beg)
      (let ((inhibit-read-only t))
        (insert text)))))

(provide 'compile-queue)
