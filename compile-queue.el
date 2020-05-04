;;; compile-queue.el --- Org mode for runbooks -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 0.1
;; Keywords: convenience, processes, terminals, files
;; Package-Requires: ((emacs "26.2"))
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
  "Use `compile-queue-current' to get an instance of this type."
  (id (uuid-string))
  name
  buffer-name
  execution
  scheduled)

(cl-defstruct (compile-queue-command (:constructor compile-queue-command-create))
  "Base struct for Compile Commands.
:before-start is a symbol or function that is called before an execution of this command
It is called with the buffer to which the execution will output as the argument.

:after-complete is a symbol or function that is called after the execution of this command completes.
It is called with the buffer to which the execution outputted as the argument

:name The name of the compile command

:matcher is an anaphoric function wrapped with a lambda providing it.
Example:
(:matcher (search-forward-regexp \"REGEXP\" nil t))

expands to
(:matcher (lambda (it) (search-forward-regexp \"REGEXP\" nil t)))

:matcher can also either by a symbol, or a list that starts with lambda.
so (:matcher #'match-function) and (:matcher (lambda (text) t)) are valid matchers.

It is called with a string containing the text being added to the buffer.
The current buffer is also set to the execution buffer of the command,
with the point set to the start of the current modification.


:deferred The deferred object for the compile command.
Represents the point in time after after-complete is called.
If the compile command throws an error, an error is passed up the deferred chain.
 "
  (id (uuid-string))
  name
  before-start
  matcher
  deferred
  after-complete)

(cl-defstruct (compile-queue-execution (:constructor compile-queue-execution-create))
  "Represents an individual execution of a `compile-queue-command'.
`compile-queue-execution-buffer' is the buffer that the execution to which the execution outputs.
`compile-queue-execution-queue' is the queue on which the buffer is being executed.
"
  (id (uuid-string))
  promise
  buffer
  queue
  status-code)

(cl-defstruct (compile-queue-promise (:constructor compile-queue-promise-create))
  "Represents a command and a deferred object that represents the completion of an execution of the command"
  (id (uuid-string))
  command
  deferred)

(cl-defstruct (compile-queue-shell-command (:constructor compile-queue-shell-command-create)
                                           (:include compile-queue-command))
  "Represents a shell command.

:command The shell command as an already escaped string
:env A cons list of environment variables like ((\"KEY2\" . \"VALUE2\") (\"KEY2\" . \"VALUE2\"))
:major-mode The major mode to use for the buffer that handles the output from the shell command
:default-directory The default-directory to use when executing the shell command
:buffer-name The name of the buffer that will handle the output from the shell command.
If nil, it defaults to a truncated version of the command."
  command
  env
  major-mode
  default-directory
  buffer-name)

;; Do not want the symbolp validation that is created by default
(put 'compile-queue-shell-command-create 'compiler-macro nil)

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
  (let* ((queue-name-is-queue (or (stringp queue-name) (symbolp queue-name)))
         (queue-var (make-symbol "queue"))
         (commands (->> (if queue-name-is-queue commands (append (list queue-name) commands))
                        (--map (compile-queue:$-command queue-var it)))))
    `(let* ((,queue-var
             (compile-queue--by-name ,(if queue-name-is-queue queue-name compile-queue-root-queue)))
            (it nil))
       ,@(->> (-drop-last 1 commands) (-map #'macroexpand))
       (prog1 ,@(->> (last commands) (-map #'macroexpand))))))

(defun compile-queue-clean (&optional queue)
  "Cleans up QUEUE or the result of `compile-queue-current'.
Kills the current execution.
"
  (interactive)
  (when-let ((queue (or queue (or queue (compile-queue-current)
                                  (compile-queue--by-name compile-queue-root-queue)))))
    (let ((process (-some-> queue compile-queue-execution compile-queue-execution-buffer get-buffer-process)))
      (when (process-live-p process)
        (kill-process process)))
    (--each
        (->>
         (compile-queue-scheduled queue)
         (-map #'compile-queue-promise-command)
         (-map #'compile-queue-command-after-complete)
         (-non-nil))
      ;; ignoring errors because they don't matter while cleaning
      (ignore-errors (funcall it nil)))
    (setf (compile-queue-execution queue) nil)
    (setf (compile-queue-scheduled queue) nil))
  t)

(defvar compile-queue--converters
  '(compile-queue:$--deferred-shell-command
    compile-queue:$--shell-command)
  "Converters for `compile-queue:$'. A converter receives a list and if it matches returns
either a form that'll be evaluated to create a `compile-queue-command' or a list of the form (:deferred t :command FORM)
where FORM is the form that would've been evaluated
Each are applied sequentially until one returns non-nil.")

(cl-defun compile-queue:$-convert (list)
  "Runs each of the converters in `compile-queue--converters' against LIST. Returns the first match."
  (cl-loop for converter in compile-queue--converters
           do
           (when-let (result (funcall converter list))
             (cl-return-from compile-queue:$-convert result))))

(cl-defun compile-queue:$-command (queue-var command)
  "Macro help for function for generating a link in the compile-queue:$ chain for COMMAND.
Handles deferring if the converter returns a form (:deferred t :command).
QUEUE-VAR is the symbol of a variable that points the queue name.
"
  (if-let ((output-command (compile-queue:$-convert command)))
      (if (plist-member output-command :deferred)
          (if (plist-get output-command :deferred)
              `(setq it (deferred:nextc it
                          (lambda (&rest arg)
                            (compile-queue-schedule
                             ,queue-var
                             (let ((it nil))
                               ,(plist-get output-command :command))))))
            `(setq it (compile-queue-schedule
                       ,queue-var
                       (let ((it nil))
                         ,(plist-get output-command :command)))))
        `(setq it (let ((it nil)) (compile-queue-schedule ,queue-var ,output-command))))
    `(setq it ,command)))



(defun compile-queue:$--deferred-shell-command (list)
  "The converter for `deferred-shell'"
  (when (memq (car list) '(deferred-shell !deferred))
    (list
     :deferred t
     :command
     (compile-queue:$--shell-command (cons 'shell (cdr list))))))

(defun compile-queue:$--shell-command (list)
  "The converter for `shell'"
  (when (memq (car list) (list 'shell '!))
    (-let* (((plist . command-rest) (compile-queue--split-plist (cdr list)))
            (plist-ht (ht<-plist plist))
            (matcher (plist-get plist :matcher)))
      (when matcher (ht-remove! plist-ht :matcher))
      `(compile-queue-shell-command-create
        ,@(ht->plist plist-ht)
        :matcher ,(cond
                   ((or (eq matcher nil) (symbolp matcher)
                        (and (listp matcher) (eq (car matcher) 'lambda)))
                    matcher)
                   (t `(lambda (it) ,matcher)))
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
  "Finds or creates and registers the queue with NAME.
Returns a `compile-queue' struct"
  (if (compile-queue-p name) name
    (or (ht-get compile-queue--name-map name)
        (--doto (compile-queue-create :name name)
          (ht-set! compile-queue--name-map name it)))))

(defun compile-queue--callback (execution)
  "Returns a callback that expects a function or a symbol that
takes the execution's buffer as an argument."
  (lambda (callback)
    (save-selected-window
      (save-mark-and-excursion
        (funcall callback (compile-queue-execution-buffer execution))))))

(defmacro compile-queue--save-var-excursion (var-names &rest body)
  "Binds the variables globally in VAR-NAMES and resets them to their original values
once BODY returns.
Accepts initial values for the var-names as well similar to `let' bindings.
"
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
            )
       ,@(->> vars (--map (when (caddr it) (list (cadr it) (caddr it))))
              (--filter it)
              (--map (cons (intern "setq") it)))
       (unwind-protect
           (progn ,@body)
         (progn
           ,@(->> vars (--map `(setq ,(cadr it) ,(car it)))))))))

(defun compile-queue-command--execute (promise queue)
  "Executes COMMAND in a buffer related to QUEUE."
  (let* ((command (compile-queue-promise-command promise))
         (buffer (compile-queue-shell-command--init-buffer command)))
    (set-buffer buffer)
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
  "Return the buffer name of the QUEUE.
Uses the `compile-queue-name' if `compile-queue-buffer-name' is unset."
  (or (compile-queue-buffer-name queue)
      (-some--> (compile-queue-name queue) (concat "*" it "*"))
      (user-error "Broken queue with no name: %S" queue)))

(defun compile-queue--update-buffer (queue)
  "Update the target view buffer of the QUEUE."
  (when-let ((execution-buffer (-> queue compile-queue-execution compile-queue-execution-buffer)))
    (let* ((buffer-name (compile-queue--buffer-name queue))
           (buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name))))
      (set-buffer buffer)
      (compile-queue-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (goto-char (point-max)))))

(defun compile-queue-current (&optional object)
  "Return the compile queue for the given OBJECT.
OBJECT can be either a process, buffer, or window.
If OBJECT is nil, return the value of `compile-queue' for the current buffer.
"
  (pcase object
    ((pred processp) (buffer-local-value 'compile-queue (process-buffer object)))
    ((pred bufferp) (buffer-local-value 'compile-queue object))
    ((pred windowp) (buffer-local-value 'compile-queue object))
    ((pred stringp) (compile-queue--by-name object))
    ('nil (buffer-local-value 'compile-queue (current-buffer)))
    (_ (error "Unexpected type passed to compile-queue-current %s" object))))

(defun compile-queue-shell-command--buffer-name (command)
  "Return the buffer name of the execution buffer for COMMAND.
return `compile-queue-shell-command-buffer-name' if available.
If `compile-queue-shell-command-name' is set, return *`compile-queue-shell-command-name'*.
Otherwise return the command's string truncated.
"
  (or (compile-queue-shell-command-buffer-name command)
      (-some--> (compile-queue-shell-command-name command) (concat " *" it "*"))
      (concat " *" (s-truncate 10 (compile-queue-shell-command-command command)) "*")))

(defun compile-queue-shell-command--name (command)
  "Return the name of COMMAND.
return `compile-queue-shell-command-buffer-name' if available.
If `compile-queue-shell-command-buffer-name' is set, return
`compile-queue-shell-command-buffer-name' instead.

Otherwise return the command's string truncated.
"
  (or (compile-queue-shell-command-name command)
      (compile-queue-shell-command-buffer-name command)
      (concat (s-truncate 10 (compile-queue-shell-command-command command)))))

(defun compile-queue-shell-command--init-buffer (command)
  "Initialize the buffer COMMAND which is a `compile-queue-shell-command'.
Replaces the buffer if it already exists."
  (pcase-let* (((cl-struct compile-queue-shell-command (default-directory directory)) command)
               (buffer-name (compile-queue-shell-command--buffer-name command)))
    (unless (get-buffer buffer-name) (generate-new-buffer buffer-name))
    (set-buffer buffer-name)

    (-some--> (get-buffer-process (current-buffer))
      (when (process-live-p it) (kill-process it)))
    (buffer-disable-undo)
    (let ((inhibit-read-only t))
      (erase-buffer))

    (funcall
     (or (compile-queue-shell-command-major-mode command)
         compile-queue-shell-default-major-mode))

    (when (not (memq #'compile-queue--forward-change after-change-functions))
      (setq-local after-change-functions (append (-some->> after-change-functions (-drop-last 1)) '(compile-queue--forward-change t))))

    (prog1 (get-buffer buffer-name)
      (when directory (setq-local default-directory directory))
      (goto-char (point-max)))))

(defun compile-queue-shell-command--process-filter (process output)
  "Process filter for compile queue that handles delegating to the original process filter.
Also, manages scolling windows to the end if the point is currently set at point-max.
"
  (let* ((process-buffer (process-buffer process))
         (queue (compile-queue-current process))
         (compile-queue-buffer (-some-> queue compile-queue--buffer-name get-buffer))
         (delegate
          (-some->> process-buffer
            (buffer-local-value 'compile-queue-shell-command--process-filter-delegate)))
         (compile-queue-end-pt (with-current-buffer (process-buffer process) (point-max)))
         (scroll-to-end (->> (get-buffer-window-list compile-queue-buffer)
                             (--filter (eq (window-point it) compile-queue-end-pt))))
         (execution (buffer-local-value 'compile-queue--execution process-buffer))
         (start (marker-position (process-mark process))))
    (-some--> delegate (funcall it process output))
    (when-let ((matcher (-some-> execution
                          compile-queue-execution-promise
                          compile-queue-promise-command
                          compile-queue-command-matcher)))
      (with-current-buffer process-buffer
        (goto-char start)
        (when (funcall matcher output)
          (deferred:callback
            (-> execution
                compile-queue-execution-promise
                compile-queue-promise-deferred)
            (-> execution compile-queue-execution-buffer))
          (compile-queue-execute queue))))

    (let* ((execution (buffer-local-value 'compile-queue--execution process-buffer)))
      (if (and execution (compile-queue-execution-eq-id
                          execution
                          (-some-> queue compile-queue-execution)))
          (progn
            (set-buffer (compile-queue--buffer-name queue))
            (let ((pt-max (point-max)))
              (--each scroll-to-end
                (set-window-point it pt-max)
                (set-window-start
                 it
                 (save-excursion
                   (goto-char pt-max)
                   (forward-line (floor (- (- (window-height it 'floor) 3))))
                   (point))))))))))

(defun compile-queue-shell-command--process-sentinel (process status)
  "Delegating sentinel for compile-queue.
Delegates to the `process-sentinel' except when the `process-sentinel' is the default.
Handles notifying compile queue on process completion."
  (let* ((buffer (process-buffer process))
         (delegate
          (-some->> buffer
            (buffer-local-value 'compile-queue-shell-command--process-sentinel-delegate))))
    (unless (eq delegate #'internal-default-process-sentinel)
      (compile-queue--save-var-excursion ((inhibit-read-only t))
        (-some--> delegate (funcall it process status))))

    (unless (process-live-p process)
      (let* ((execution (buffer-local-value 'compile-queue--execution buffer))
             (status-code (compile-queue--status-code-for-process process status))
             (queue (-some-> (compile-queue-current process)))
             (is-target (compile-queue-execution-eq-id
                         execution
                         (-some-> queue compile-queue-execution)))
             (killed (not (eq status-code 0))))
        (when execution (setf (compile-queue-execution-status-code execution) status-code))
        (when is-target
          (setf (compile-queue-execution queue) nil)
          (when killed (compile-queue-clean queue)))
        (-some-->
            (-some-> execution
              compile-queue-execution-promise
              compile-queue-promise-command
              compile-queue-command-after-complete)
          (funcall (compile-queue--callback execution) it))

        (when is-target
          (progn
            (if killed
                (-some--> execution
                  (compile-queue-execution-promise it)
                  (compile-queue-promise-deferred it)
                  (deferred:errorback it status))
              (-some--> execution
                (compile-queue-execution-promise it)
                (compile-queue-promise-deferred it)
                (deferred:callback it (-some-> execution compile-queue-execution-buffer))))
            (when (not killed) (compile-queue-execute queue))))))))

(defun compile-queue--forward-change (beg end length)
  "After change function that handles forwarding an execution buffer to its corresponding compile-queue buffer."
  (when (compile-queue-execution-eq-id compile-queue--execution (compile-queue-execution compile-queue))
    (let ((text (buffer-substring beg end)))
      (set-buffer (compile-queue--buffer-name compile-queue))
      (goto-char beg)
      (delete-char length)
      (let ((inhibit-read-only t))
        (insert text)))))

(defun compile-queue--status-code-for-process (process status)
  "Return the STATUS from PROCESS as a status code."
  (pcase (process-status process)
    ((or 'exit 'signal 'failed)
     (pcase (s-trim status)
       ("finished" 0)
       ((rx line-start "exited abnormally with code" (* whitespace) (let code (* digit)))
        (string-to-number code))
       (_ nil)))
    (_ nil)))

(defun compile-queue-execution-eq-id (lhs rhs)
  "Return true if LHS id == rhs id."
  (string= (-some-> lhs compile-queue-execution-id)
           (-some-> rhs compile-queue-execution-id)))

(define-derived-mode compile-queue-mode fundamental-mode "Compile-Queue"
  "Mode for mirroring the output of the current queue's execution's compile buffer."
  :group 'compile-queue
  (read-only-mode 1))

(when (boundp 'evil-motion-state-modes)
  (add-to-list 'evil-motion-state-modes 'compile-queue-mode))

(provide 'compile-queue)
;;; compile-queue.el ends here
