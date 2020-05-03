;;; compile-queue.el --- Org mode for runbooks -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 0.1
;; Keywords: convenience, processes, terminals, files
;; Package-Requires: ((emacs "25.1"))
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
(require 'pcase)
(require 'cl-lib)
(require 'deferred)
(require 's)
(require 'rx)

(defgroup compile-queue nil
  "Customization Group for Compile Queue."
  :group 'convenience)

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
  buffer
  queue
  status-code)

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
          (it nil))
     ,@(-drop-last 1 commands)
     (prog1 ,@(last commands)))))

(defun compile-queue-clean (&optional queue skip-execution)
  "Cleans up QUEUE or the result of `compile-queue-current'.
Kills the current execution.
"
  (interactive)
  (when-let ((queue (or queue (or queue (compile-queue-current)
                                  (compile-queue--by-name compile-queue-root-queue)))))
    (let ((process (-some-> queue compile-queue-execution compile-queue-execution-buffer get-buffer-process)))
      (when (process-live-p process)
        (kill-process process)))
    (when skip-execution
      (-some--> (-some-> queue compile-queue-execution
                         compile-queue-execution-promise
                         compile-queue-promise-deferred)
        (deferred:errorback it "Queue Killed")))
    (--each
        (->>
         (compile-queue-scheduled queue)
         (-map #'compile-queue-promise-command)
         (-map #'compile-queue-command-after-complete)
         (-non-nil))
      (funcall it nil))
    (setf (compile-queue-execution queue) nil)
    (setf (compile-queue-scheduled queue) nil))
  t)

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
                         (let ((it nil))
                           ,(plist-get output-command :command))))))
        `(setq it (let ((it nil)) (compile-queue-schedule ,queue-var ,output-command))))
    `(setq it ,command)))



(defun compile-queue:$--deferred-shell-command (list)
  (when (memq (car list) '(deferred-shell !deferred))
    (list
     :deferred t
     :command
     (compile-queue:$--shell-command (cons 'shell (cdr list))))))

(defun compile-queue:$--shell-command (list)
  (when (memq (car list) (list 'shell '!))
    (-let [(plist . command-rest) (compile-queue--split-plist (cdr list))]
      `(compile-queue-shell-command-create ,@plist
                                           :command
                                           (s-join " " (list ,@command-rest))))))


(defun compile-queue--split-plist (plist)
  "Split PLIST into a cons cell (SUBPLIST . REST)
where SUBPLIST is the valid prefix plist of PLIST and REST is the remainder of the PLIST excluding SUBPLIST."
  (-let* ((plist-end
           (or
            (-some--> (->> plist (--find-last-index (and (symbolp it) (s-starts-with-p ":" (symbol-name it)))))
              (+ it 2))
            0))
          (output-plist (-slice plist 0 plist-end))
          (command-rest (-slice plist plist-end)))
    (--each-indexed output-plist
      (pcase (if (eq it-index 0) 0 (% it-index 2))
        (1 (when (and (symbolp it)
                      (s-starts-with-p ":" (symbol-name it)))
             (error "Unexpected key in value slot %s, slot: %s, plist %s" it
                    (nth (1- it-index) output-plist)
                    output-plist)))
        (0
         (unless (and (symbolp it)
                      (s-starts-with-p ":" (symbol-name it)))
           (error "Unexpected Value %s at %d when expecting key. plist %s"
                  it
                  it-index
                  output-plist)))))
    (cons output-plist command-rest)))

(defun compile-queue--by-name (name)
  (if (compile-queue-p name) name
    (or (ht-get compile-queue--name-map name)
        (--doto (compile-queue-create :name name)
          (ht-set! compile-queue--name-map name it)))))

(defun compile-queue--callback (execution)
  (lambda (callback)
    (save-selected-window
      (save-mark-and-excursion
        (funcall callback (compile-queue-execution-buffer execution))))))

(defmacro compile-queue--save-var-excursion (var-names &rest body)
  (declare (indent 1))
  (let* ((var-names (if (symbolp var-names) (list var-names) var-names))
         (vars
          (->> var-names
               (--map (append
                       (list
                        (make-symbol
                         (concat "old-"
                                 (-> (pcase it
                                       ((pred listp) (car it))
                                       ((pred symbolp) it)
                                       (_ (user-error "Unexpected type: %s" it)))
                                     symbol-name))))
                       (if (listp it)
                           (list
                            (car it)
                            (cadr it))
                         (list it)))))))
    `(let ,(append
            (->> vars (--map (list (car it) (cadr it))))
            (->> vars (--map (when (caddr it) (list (cadr it) (caddr it))))
                 (--filter it)))
       (unwind-protect
           (progn ,@body)
         (progn
           ,@(->> vars (--map `(setq ,(cadr it) ,(car it)))))))))

(defun compile-queue-command--execute (promise queue)
  "Executes COMMAND in a buffer related to QUEUE."
  (let* ((command (compile-queue-promise-command promise))
         (buffer (compile-queue-shell-command--init-buffer command)))
    (set-buffer buffer)
    (when (not (memq #'compile-queue--forward-change after-change-functions))
      (setq-local after-change-functions (append (-some->> after-change-functions (-drop-last 1)) '(compile-queue--forward-change t))))
    (setq-local compile-queue queue)
    (let ((execution (compile-queue-execution-create
                      :buffer buffer
                      :status-code nil
                      :promise promise
                      :queue queue)))
      (setq-local compile-queue--execution execution)
      (prog1 execution
        (setf (compile-queue-execution queue) execution)
        (-some--> (compile-queue-command-before-start command)
          (funcall (compile-queue--callback execution) it))
        (let ((process (start-process-shell-command
                        (compile-queue-shell-command--name command)
                        (compile-queue-shell-command--buffer-name command)
                        (compile-queue-shell-command-command command))))
          (setf (compile-queue-execution-buffer execution) buffer)
          (--doto process
            (when it
              (setq-local compile-queue-shell-command--process-filter-delegate (process-filter it))
              (setq-local compile-queue-shell-command--process-sentinel-delegate (process-sentinel it))
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

(defun compile-queue-schedule (queue command)
  "Appends the COMMAND to the commands on QUEUE."
  (let ((promise (deferred:new)))
    (setf
     (compile-queue-scheduled queue)
     (append (--> (compile-queue-scheduled queue) (if (listp it) it (list it)))
             (list (compile-queue-promise-create
                    :command command
                    :deferred promise)) nil))

    (if (not (compile-queue-execution queue))
        (progn
          (compile-queue-execute queue)))
    promise))

(defun compile-queue--buffer-name (queue)
  (or (compile-queue-buffer-name queue)
      (-some--> (compile-queue-name queue) (concat "*" it "*"))
      (user-error "Broken queue with no name: %S" queue)))

(defun compile-queue-execution--buffer-name (execution)
  (let ((command (compile-queue-promise-command
                  (compile-queue-execution-promise execution))))
    (pcase command
      ((cl-struct compile-queue-shell-command)
       (compile-queue-shell-command--buffer-name command)))))


(defun compile-queue--update-buffer (queue)
  (when-let ((execution-buffer (-> queue compile-queue-execution compile-queue-execution-buffer)))
    (let* ((buffer-name (compile-queue--buffer-name queue))
           (old-buffer (get-buffer buffer-name))
           (windows (get-buffer-window-list old-buffer))
           (new-buffer (generate-new-buffer " *temp-queue*")))
      (when old-buffer
        (--each windows
          (set-window-buffer it new-buffer))
        (kill-buffer old-buffer))
      (set-buffer new-buffer)
      (compile-queue-mode)
      (unless (string= buffer-name (buffer-name new-buffer))
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
      (compile-queue-shell-command-name command)
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

    (prog1 (get-buffer buffer-name)
      (when directory (setq-local default-directory directory))
      (goto-char (point-max)))))

(defun compile-queue-shell-command--process-filter (process output)
  (let* ((execution-buffer (process-buffer process))
         (delegate
          (-some->> execution-buffer
            (buffer-local-value 'compile-queue-shell-command--process-filter-delegate))))
    (-some--> delegate (funcall it process output))

    (let* ((compile-queue (compile-queue-current process))
           (compile-queue-buffer (get-buffer (compile-queue--buffer-name compile-queue)))
           (execution (buffer-local-value 'compile-queue--execution execution-buffer)))
      (if (and execution (eq (-some-> execution compile-queue-execution-id)
                             (-some-> compile-queue compile-queue-execution compile-queue-execution-id)))
          (progn (let* ((compile-queue-end-pt (with-current-buffer (process-buffer process) (point-max)))
                        (scroll-to-end (->> (get-buffer-window-list compile-queue-buffer)
                                            (--filter (eq (window-point it) compile-queue-end-pt)))))
                   (set-buffer (compile-queue--buffer-name (compile-queue-current)))

                   (let ((pt-max (point-max)))
                     (--each scroll-to-end
                       (set-window-point it pt-max)))))))))

(defun compile-queue-shell-command--process-sentinel (process status)
  "Delegating sentinel for compile-queue. Handles notifying compile queue on process completion."
  (let* ((buffer (-some-> process
                   compile-queue-current
                   compile-queue-execution
                   compile-queue-execution-buffer))
         (delegate
          (-some->> buffer
            (buffer-local-value 'compile-queue-shell-command--process-sentinel-delegate))))
    (compile-queue--save-var-excursion ((inhibit-read-only t))
      (-some--> delegate (funcall it process status)))

    (unless (process-live-p process)
      (when-let ((queue (compile-queue-current process)))
        (let* ((execution (-some-> queue compile-queue-execution))
               (status-code (compile-queue--status-code-for-process process status))
               (killed (not (eq status-code 0))))
          (when execution (setf (compile-queue-execution-status-code execution) status-code))
          (setf (compile-queue-execution queue) nil)
          (if killed (compile-queue-clean queue t))
          (-some-->
              (-some-> execution
                compile-queue-execution-promise
                compile-queue-promise-command
                compile-queue-command-after-complete)
            (funcall (compile-queue--callback execution) it))

          (if killed
              (-some--> execution
                (compile-queue-execution-promise it)
                (compile-queue-promise-deferred it)
                (deferred:errorback it status))
            (-some--> execution
              (compile-queue-execution-promise it)
              (compile-queue-promise-deferred it)
              (deferred:callback it (-some-> execution compile-queue-execution-buffer))))
          (when (not killed) (compile-queue-execute queue)))))))

(defun compile-queue--forward-change (beg end length)
  (when (eq compile-queue--execution (compile-queue-execution compile-queue))
    (if (< length 0)
        (let* ((length (abs length))
               (lhs (buffer-substring (- beg length) beg))
               (rhs (buffer-substring beg (+ beg length))))
          (set-buffer (compile-queue--buffer-name compile-queue))
          (goto-char beg)
          (cond
           ((string= (buffer-substring beg (+ beg length)) rhs)
            (let ((inhibit-read-only t))
              (delete-char length)))
           ((string= (buffer-substring (- beg length) beg) lhs)
            (let ((inhibit-read-only t))
              (delete-char (- length)))))))
    (let ((text (buffer-substring beg end)))
      (set-buffer (compile-queue--buffer-name compile-queue))
      (goto-char beg)
      (let ((inhibit-read-only t))
        (insert text)))))

(defun compile-queue--status-code-for-process (process status)
  (pcase (process-status process)
    ((or 'exit 'signal 'failed)
     (pcase (s-trim status)
       ("finished" 0)
       ((rx line-start "exited abnormally with code" (* whitespace) (let code (* digit)))
        (string-to-number code))
       (_ nil)))
    (_ nil)))

(define-derived-mode compile-queue-mode fundamental-mode "Compile-Queue"
  "Mode for mirroring the output of the current queue's execution's compile buffer."
  :group 'compile-queue
  (setq-local buffer-read-only t))

(provide 'compile-queue)
;;; compile-queue.el ends here
