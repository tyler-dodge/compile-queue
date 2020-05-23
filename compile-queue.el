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

(require 'uuid)
(require 'dash)
(require 'ht)
(require 'pcase)
(require 'cl-lib)
(require 'deferred)
(require 's)
(require 'rx)
(require 'comint)
(declare-function org-runbook--validate-command "ext:org-runbook.el" (command))
(declare-function org-runbook-command-name "ext:org-runbook.el" (command))
(declare-function org-runbook-command-full-command "ext:org-runbook.el" (command))

(defgroup compile-queue nil
  "Customization Group for Compile Queue."
  :group 'convenience)

(defcustom compile-queue-root-queue
  "compile-queue"
  "Name of the compile queue that is used by default if a compile queue is not specified when calling."
  :safe t
  :type 'stringp
  :group 'compile-queue)

(defcustom compile-queue-mode-line-format
  '(" %b" " - " (:eval (or (compile-queue-mode--mode-line-command-name) "")) " " (:eval (compile-queue-mode--mode-line-scheduled)))
  "The `mode-line-format' used by the queue buffer."
  :safe t
  :type 'stringp
  :group 'compile-queue)


(defcustom compile-queue-garbage-collect-time
  10.0
  "Time in seconds before the buffer is garbage collected automatically.
Set to nil to disable garbage collection."
  :safe t
  :type 'numberp
  :group 'compile-queue)


(defcustom compile-queue-shell-default-major-mode
  #'comint-mode
  "The `major-mode' used in the compile-queue buffer if the `compile-queue-command' does not specify a `major-mode'."
  :safe t
  :type 'functionp
  :group 'compile-queue)

(defvar compile-queue--name-ht (ht)
  "Hash table from compile-queue name to the corresponding compile-queue struct.")

(defvar-local compile-queue-delegate-mode--process-filter-delegate nil
  "The process filter that compile-queue delegates to while running.")

(defvar-local compile-queue-delegate-mode--process-sentinel-delegate nil
  "The process sentinal that compile-queue delegates to while running.")

(defvar-local compile-queue-delegate-mode--queue nil
  "The compile-queue that the current buffer is referencing.")

(defvar-local compile-queue-delegate-mode--gc-timer nil
  "The timer for cleaning up the current buffer after the process completes.")

(defvar-local compile-queue-delegate-mode--execution nil
  "The execution that is related to the current buffer's process.")

(defvar-local compile-queue-delegate-mode--matcher-disabled nil
  "When non-nil, disable the current execution's matcher.
Set automatically if the matcher throws an error.")


(cl-defstruct (compile-queue (:constructor compile-queue-create)
                             (:conc-name compile-queue--)
                             (:copier compile-queue-copy))
  "Use `compile-queue-current' to get an instance of this type."
  (id (uuid-string))
  name
  buffer-name
  target-execution
  scheduled)

(cl-defstruct (compile-queue-command (:constructor compile-queue-command-create)
                                     (:conc-name compile-queue-command--)
                                     (:copier compile-queue-command-copy))
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

:keep-buffer can either be nil or non-nil.
If non-nil: No garbage collection happens.
If nil: The buffer gets removed after completion.

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
  keep-buffer
  deferred
  after-complete)

(cl-defstruct (compile-queue-execution (:constructor compile-queue-execution-create)
                                       (:conc-name compile-queue-execution--)
                                       (:copier compile-queue-execution-copy))
  "Represents an individual execution of a `compile-queue-command'.
`compile-queue-execution--buffer' is the buffer that the execution to which the execution outputs.
`compile-queue-execution-queue' is the queue on which the buffer is being executed.
"
  (id (uuid-string))
  promise
  buffer
  process
  queue
  status-code)

(cl-defstruct (compile-queue-promise (:constructor compile-queue-promise-create)
                                     (:conc-name compile-queue-promise--)
                                     (:copier compile-queue-promise-copy))
  "Represents a command and a deferred object that represents the completion of an execution of the command"
  (id (uuid-string))
  command
  deferred)

(cl-defstruct (compile-queue-shell-command (:constructor compile-queue-shell-command-create)
                                           (:conc-name compile-queue-shell-command--)
                                           (:copier compile-queue-shell-command-copy)
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

;;;###autoload
(defmacro compile-queue-$ (queue-name &rest commands)
  "Small DSL for chaining COMMANDS on the compile-queue.
Fully compatible with deferred.el's `deferred-$'


QUEUE-NAME is optional.

Currently there are 2 special types

`(shell &rest COMMAND)'
See `compile-queue-shell-command' for keywords.

`(! &rest COMMAND)' - run the command specified by joining
the list of COMMAND with spaces


`(deferred-shell &rest COMMAND)'
`(!deferred &rest COMMAND)' - waits to schedule the command
until the deferred chain before this has already completed.
See `compile-queue-shell-command' for keywords

`(deferred-org-runbook &rest COMMAND)'
`(>deferred &rest COMMAND)'
`(org-runbook &rest COMMAND)'
`(> &rest COMMAND)' - run the command by matching the first
org-runbook command that matches COMMAND concatenated with >>.
For instance, given

```
* A
** A1
#+BEGIN_SRC compile-queue
echo A
#+END_SRC
* B
* B2
```


`(org-runbook \"A >> A1\")'

`(org-runbook \"A\" \"A1\")'


both resolve to the command

```
echo A
```



The deferred promise receives the output buffer as the argument
once the execution completes.


Example:
`(compile-queue-$
   (shell \"echo A\")
   (deferred-shell \"echo C\")
   (shell \"echo B\"))'

This example will end up displaying C because the command \"echo C\"
is not scheduled until after \"echo A\" finishes execution
whereas \"echo B\" is scheduled before \"echo B\" starts executing.

Example:
`(compile-queue-$
   (shell \"echo A\")
   (deferred:nextc it
     (lambda (buffer)
       (set-buffer buffer)
       (message \"%s\" (string-trim (buffer-string))))))'

This example shows how compile-queue can be chained with deferred.el."
  (declare (debug t)
           (indent 0))
  (let* ((queue-name-is-queue (or (stringp queue-name) (symbolp queue-name)))
         (queue-var (make-symbol "queue"))
         (commands (->> (if queue-name-is-queue commands (append (list queue-name) commands))
                        (--map (compile-queue-$-command queue-var it)))))
    `(let* ((,queue-var
             (compile-queue--by-name ,(if queue-name-is-queue queue-name compile-queue-root-queue)))
            (it nil))
       ,@(->> (-drop-last 1 commands) (-map #'macroexpand))
       (prog1 ,@(->> (last commands) (-map #'macroexpand))))))

;;;###autoload
(defmacro compile-queue-force-$ (queue-name &rest rest)
  "Clean the queue before scheduling on it.
Unlike `compile-queue-$' this requires QUEUE-NAME to be set.
See `compile-queue-$' for how to format REST."
  (declare (debug t)
           (indent 1))
  `(progn
     (compile-queue-clean ,queue-name)
     (compile-queue-$
       ,queue-name
       ,@rest)))

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
    (let ((process (-some-> queue compile-queue--target-execution compile-queue-execution--buffer get-buffer-process)))
      (when (process-live-p process)
        (kill-process process)))
    (--each
        (->>
         (compile-queue--scheduled queue)
         (-map #'compile-queue-promise--command)
         (-map #'compile-queue-command--after-complete)
         (-non-nil))
      ;; ignoring errors because they don't matter while cleaning
      (ignore-errors (funcall it nil)))
    (setf (compile-queue--target-execution queue) nil)
    (setf (compile-queue--scheduled queue) nil))
  (force-mode-line-update t)
  t)

(defvar compile-queue--converters
  '(compile-queue-$--deferred-shell-command
    compile-queue-$--shell-command
    compile-queue-$--deferred-org-runbook-command
    compile-queue-$--org-runbook-command)
  "Converters for `compile-queue-$'.
A converter receives a list and if it matches return either a form
that'll be evaluated to create a `compile-queue-command' or a list
of the form (:deferred t :command FORM)
where FORM is the form that would've been evaluated.

Each are applied sequentially until one returns non-nil.")

(cl-defun compile-queue-$-convert (list)
  "Runs each of the converters in `compile-queue--converters' against LIST. Returns the first match."
  (cl-loop for converter in compile-queue--converters
           do
           (when-let (result (funcall converter list))
             (cl-return-from compile-queue-$-convert result))))

(cl-defun compile-queue-$-command (queue-var command)
  "Macro help for function for generating a link in the compile-queue-$ chain for COMMAND.
Handles deferring if the converter returns a form (:deferred t :command).
QUEUE-VAR is the symbol of a variable that points the queue name.
"
  (if-let ((output-command (compile-queue-$-convert command)))
      (if (plist-member output-command :deferred)
          (if (plist-get output-command :deferred)
              `(setq it (deferred:nextc it
                          (lexical-let ((,queue-var ,queue-var))
                            (lambda (&rest arg)
                              (compile-queue-schedule
                               ,queue-var
                               (let ((it nil))
                                 ,(plist-get output-command :command)))))))
            `(setq it (compile-queue-schedule
                       ,queue-var
                       (let ((it nil))
                         ,(plist-get output-command :command)))))
        `(setq it (let ((it nil)) (compile-queue-schedule ,queue-var ,output-command))))
    `(setq it ,command)))



(defun compile-queue-$--deferred-shell-command (list)
  "Return `deferred-shell' if LIST match."
  (when (memq (car list) '(deferred-shell !deferred))
    (list
     :deferred t
     :command
     (compile-queue-$--shell-command (cons 'shell (cdr list))))))

(defun compile-queue-$--deferred-org-runbook-command (list)
  "Return `deferred-shell' if LIST match."
  (when (memq (car list) '(deferred-org-runbook >deferred))
    (list
     :deferred t
     :command
     (compile-queue-$--org-runbook-command (cons 'org-runbook (cdr list))))))

(defun compile-queue-$--shell-env-ambiguous (env)
  "Return non-nil if ENV is ambiguous."
  (pcase env
    (`(((,_ . ,_) . ,_)) nil)
    (`((,_ ,_ . ,_)) t)
    (`((,_ . ,_)) nil)
    (`(,_ . nil) t)))

(defun compile-queue-$--shell-wrap-env (env)
  "Return wrapped ENV if it is not a list."
  (pcase env
    ('nil nil)
    (`((,_ . ,_) (,_ . ,_)) env)
    (`(,_ . nil) env)
    (`(,_ . ,_) (list env))
    (_ env)))

(defun compile-queue-$--shell-command (list)
  "Return `shell' if LIST match."
  (when (memq (car list) (list 'shell '!))
    (-let* (((plist . command-rest) (compile-queue--split-plist (cdr list)))
            (plist-ht (ht<-plist plist))
            (env (ht-get plist-ht :env))
            (matcher (ht-get plist-ht :matcher)))
      (when matcher (ht-remove plist-ht :matcher))
      (when env (ht-remove plist-ht :env))
      `(compile-queue-shell-command-create
        :env ,(let ((env (if (and (consp env) (not (cdr env))) env env)))
                `(->>
                  ,(cond
                    ((not env) nil)
                    ((symbolp env)
                     `(let ((result ,env))
                        (compile-queue-$--shell-wrap-env result)))
                    ((compile-queue-$--shell-env-ambiguous env)
                     (error "Current structure is ambiguous.  Use full syntax to clear the ambiguity.
Ex: `(shell :env ((KEY . nil))'.  %S" env))
                    (t
                     `(quote ,(compile-queue-$--shell-wrap-env env))))
                  (--map (cons (eval (car it)) (eval (cdr it))))
                  (-non-nil)
                  (--map
                   (prog1 it
                     (unless (consp it)
                       (error "Env should be either an alist or a cons.  Found: %s.  List %s" it (list ,list)))
                     (unless (stringp (car it))
                       (error "Env Key should resolve to a string .  Found: %s.  Cons: %s.  List %s" (car it) it (quote ,list)))
                     (unless (or (stringp (cdr it)) (not (cdr it)))
                       (error "Env Value should resolve to a string .  Found: %s.  Cons: %s.  List %s" (cdr it) it (quote ,list)))))))
        ,@(unless (ht-get plist-ht :default-directory) (list :default-directory default-directory))
        ,@(ht->plist plist-ht)
        :matcher ,(cond
                   ((or (not matcher)
                        (symbolp matcher)
                        (and (listp matcher) (eq (car matcher) 'lambda)))
                    matcher)
                   (t `(lambda (it) ,matcher)))
        :command
        (s-join " " (list ,@command-rest))))))


(defun compile-queue-$--org-runbook-command (list)
  "Schedule a runbook command if LIST is of the form (org-runbook &rest arg)."
  (when (memq (car list) (list 'org-runbook '>))
    (-let* (((plist . rest) (compile-queue--split-plist (cdr list))))
      `(let* ((command-name (s-join " >> " (list ,@rest)))
              (commands (->>
                         (org-runbook-targets)
                         (-map #'org-runbook-file-targets)
                         (-flatten)
                         (-map #'org-runbook--shell-command-for-target))))
         (--> commands
              (or (--first (string= command-name (org-runbook-command-name it)) it)
                  (user-error "Unable to find runbook command named: %s.  Available Commands: %s"
                              command-name
                              (-some--> commands
                                (-map #'org-runbook-command-name it)
                                (s-join ", " it)
                                (concat "[" it "]"))))
              (org-runbook-command-full-command it)
              (compile-queue-shell-command-create
               ,@(unless (plist-get plist :name) (list :name 'command-name))
               ,@plist
               :command it))))))

(defun compile-queue--split-plist (plist)
  "Split PLIST into a cons cell (SUBPLIST . REST)
where SUBPLIST is the valid prefix plist of PLIST and REST is the remainder of the PLIST excluding SUBPLIST."
  (-let* ((plist-end
           (or
            (-some--> (->> plist (--find-last-index (and (symbolp it) (string-prefix-p ":" (symbol-name it)))))
              (+ it 2))
            0))
          (output-plist (-slice plist 0 plist-end))
          (command-rest (-slice plist plist-end)))
    (--each-indexed output-plist
      (pcase (if (eq it-index 0) 0 (% it-index 2))
        (1 (when (and (symbolp it)
                      (string-prefix-p ":" (symbol-name it)))
             (error "Unexpected key in value slot %s, slot: %s, plist %s" it
                    (nth (1- it-index) output-plist)
                    output-plist)))
        (0
         (unless (and (symbolp it)
                      (string-prefix-p ":" (symbol-name it)))
           (error "Unexpected Value %s at %d when expecting key.  plist %s"
                  it
                  it-index
                  output-plist)))))
    (cons output-plist command-rest)))

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

(defmacro compile-queue--save-var-excursion (var-names &rest body)
  "Bind the variables globally in VAR-NAMES and reset them after BODY.
Accepts initial values for the var-names as well similar to `let' bindings."
  (declare (indent 1))
  (let* ((var-names (if (symbolp var-names) (list var-names) var-names))
         (vars
          (->> var-names
               (--map (append
                       (list (make-symbol (concat
                                           "old-"
                                           (-> (pcase it
                                                 ((pred listp) (car it))
                                                 ((pred symbolp) it)
                                                 (_ (user-error "Unexpected type: %s" it)))
                                               symbol-name))))
                       (if (listp it) (list (car it) (cadr it))
                         (list it)))))))
    `(let ,(append (->> vars (--map (list (car it) (cadr it)))))
       ,@(->> vars (--map (when (caddr it) (list (cadr it) (caddr it))))
              (--filter it)
              (--map (cons (intern "setq") it)))
       (unwind-protect
           (progn ,@body)
         (progn ,@(->> vars (--map `(setq ,(cadr it) ,(car it)))))))))

(defun compile-queue-shell-command-command (command)
  "Return the full shell command for COMMAND."
  (compile-queue-shell-command--command command))

(defun compile-queue-command--execute (promise queue)
  "Execute PROMISE in a buffer related to QUEUE."
  (let* ((command (compile-queue-promise--command promise))
         (buffer (compile-queue-shell-command--init-buffer command)))
    (with-current-buffer buffer
      (setq-local compile-queue-delegate-mode--queue queue)
      (let ((execution (compile-queue-execution-create
                        :buffer buffer
                        :status-code nil
                        :promise promise
                        :queue queue)))
        (setq-local compile-queue-delegate-mode--execution execution)
        (prog1 execution
          (setf (compile-queue--target-execution queue) execution)
          (-some--> (compile-queue-command--before-start command)
            (funcall (compile-queue--callback execution) it))
          (let* ((buffer-name (compile-queue-shell-command-buffer-name command))
                 (process
                  (compile-queue--save-var-excursion (process-environment)
                    (--each (compile-queue-shell-command--env command)
                      (add-to-list 'process-environment (concat (car it) "="
                                                                (cdr it))))
                    (start-process-shell-command
                     (compile-queue-shell-command-name command)
                     buffer-name
                     (compile-queue-shell-command-command  command)))))

            (when (derived-mode-p #'comint-mode)
              (goto-char (point-min))
              (set-marker (process-mark process) (point))
              (setq-local comint-last-output-start (make-marker))
              (set-marker comint-last-output-start (point))
              (set-process-filter process #'comint-output-filter))
            (setf (compile-queue-execution--process execution) process)
            (setf (compile-queue-execution--buffer execution) buffer)
            (--doto process
              (when it
                (setq-local compile-queue-delegate-mode--process-filter-delegate (process-filter it))
                (setq-local compile-queue-delegate-mode--process-sentinel-delegate (process-sentinel it))
                (set-process-sentinel it #'compile-queue-delegate-mode--process-sentinel)
                (set-process-filter it #'compile-queue-delegate-mode--process-filter)))
            (compile-queue--update-buffer queue)))))))

(defun compile-queue-execute (queue)
  "Execute the next command in commands on QUEUE."
  (-let (((next-command . rest) (-some--> (compile-queue--scheduled queue) (if (not (listp it)) (list it) it))))
    (if (not next-command)
        (setf (compile-queue--target-execution queue) nil)
      (setf (compile-queue--scheduled queue) rest)
      (setf (compile-queue--target-execution queue)
            (compile-queue-command--execute next-command queue)))))

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
  "Return the buffer name of the QUEUE.
Uses the `compile-queue--name' if `compile-queue--buffer-name' is unset."
  (or (compile-queue--buffer-name queue)
      (-some--> (compile-queue--name queue) (concat "*" it "*"))
      (user-error "Broken queue with no name: %S" queue)))

(defun compile-queue--update-buffer (queue)
  "Update the target view buffer of the QUEUE."
  (when-let ((execution-buffer (-> queue compile-queue--target-execution compile-queue-execution--buffer)))
    (let* ((buffer-name (compile-queue-buffer-name queue))
           (buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name))))
      (with-current-buffer buffer
        (compile-queue-mode)
        (setq-local compile-queue-delegate-mode--queue queue)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (goto-char (point-max))))
    (force-mode-line-update)))


(defun compile-queue-current (&optional object)
  "Return the compile queue for the given OBJECT.
OBJECT can be either a process, buffer, or window.

If OBJECT is nil, return the value of `compile-queue-delegate-mode--queue'
for the current buffer."
  (pcase object
    ((pred processp) (buffer-local-value 'compile-queue-delegate-mode--queue (process-buffer object)))
    ((pred bufferp) (buffer-local-value 'compile-queue-delegate-mode--queue object))
    ((pred windowp) (buffer-local-value 'compile-queue-delegate-mode--queue object))
    ((pred stringp) (compile-queue--by-name object))
    ('nil (buffer-local-value 'compile-queue-delegate-mode--queue (current-buffer)))
    (_ (error "Unexpected type passed to compile-queue-current %s" object))))

(defun compile-queue-shell-command-buffer-name (command)
  "Return the buffer name of the execution buffer for COMMAND.
return `compile-queue-shell-command-buffer-name' if available.

If `compile-queue-shell-command-name' is set,
return *`compile-queue-shell-command-name'*.

Otherwise return the command's string truncated."
  (or (compile-queue-shell-command--buffer-name command)
      (-some--> (compile-queue-shell-command--name command) (concat " *" it "*"))
      (concat " *" (s-truncate 20 (compile-queue-shell-command-command command)) "*")))

(defun compile-queue-shell-command-name (command)
  "Return the name of COMMAND.
return `compile-queue-shell-command-buffer-name' if available.
If `compile-queue-shell-command-buffer-name' is set, return
`compile-queue-shell-command-buffer-name' instead.

Otherwise return the command's string truncated."
  (or (compile-queue-shell-command--name command)
      (compile-queue-shell-command-buffer-name command)
      (concat (s-truncate 10 (compile-queue-shell-command-command command)))))

(defun compile-queue-shell-command--init-buffer (command)
  "Initialize the buffer COMMAND which is a `compile-queue-shell-command'.
Replaces the buffer if it already exists."
  (pcase-let* (((cl-struct compile-queue-shell-command (default-directory directory)) command)
               (buffer-name (compile-queue-shell-command-buffer-name command)))
    (unless (get-buffer buffer-name) (generate-new-buffer buffer-name))
    (with-current-buffer buffer-name

      (-some--> (get-buffer-process (current-buffer))
        (when (process-live-p it) (kill-process it)))
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (-some-> compile-queue-delegate-mode--gc-timer cancel-timer)
      (setq-local compile-queue-delegate-mode--gc-timer nil)
      (funcall
       (or (compile-queue-shell-command--major-mode command)
           compile-queue-shell-default-major-mode))

      (compile-queue-delegate-mode 1)

      (prog1 (get-buffer buffer-name)
        (when directory (setq-local default-directory directory))
        (goto-char (point-max))))))

(defun compile-queue-delegate-mode--forward-change (beg end length)
  "Forward change to the `compile-queue-delegate-mode--queue' buffer.
Replaces the text at BEG with LENGTH with the text between BEG and END
from the execution-buffer in the compile-queue-delegate-mode--queue buffer."
  (when (compile-queue-execution-eq-id compile-queue-delegate-mode--execution (compile-queue--target-execution compile-queue-delegate-mode--queue))
    (let ((text (buffer-substring beg end)))
      (with-current-buffer (compile-queue-buffer-name compile-queue-delegate-mode--queue)
        (goto-char beg)
        (delete-char length)
        (let ((inhibit-read-only t))
          (insert text))))))

(defun compile-queue-delegate-mode--status-code-for-process (process status)
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
  "Return non-nil if LHS id == RHS id."
  (when (and lhs rhs)
    (string= (-> lhs compile-queue-execution--id)
             (-> rhs compile-queue-execution--id))))

(define-derived-mode compile-queue-mode special-mode "Compile-Queue"
  "Mode for mirroring the output of the current queue's execution's compile buffer."
  :group 'compile-queue
  (when compile-queue-mode-line-format
    (setq-local mode-line-format compile-queue-mode-line-format))
  (setq-local buffer-read-only t))

(defun compile-queue-mode--mode-line-command-name ()
  "Return the command name for the current execution for the mode line."
  (let* ((execution (-some-> (compile-queue-current)
                      compile-queue--target-execution))
         (command  (-some-> execution compile-queue-execution--promise
                            compile-queue-promise--command)))
    (or
     (-some-> command compile-queue-command--name)
     (-some-> execution compile-queue-execution--buffer buffer-name)
     (when (compile-queue-shell-command-p command) (-some-> command compile-queue-shell-command-command)))))

(defun compile-queue-mode--mode-line-scheduled ()
  "Return the scheduled command names for the mode line."
  (let* ((scheduled (-some-> (compile-queue-current)
                      compile-queue--scheduled)))

    (-some--> scheduled
      (--map
       (let ((command (->> it (compile-queue-promise--command))))
         (or (compile-queue-command--name command)
             (when (compile-queue-shell-command-p command) (-some-> command compile-queue-shell-command-command))))
       it)
      (s-join ", " it)
      (concat "[" it "]"))))

(defun compile-queue-execute-org-runbook-command (command &optional queue)
  "Schedule the `org-runbook-command' COMMAND.
The queue it is scheduled on will be either QUEUE, `compile-queue-current'
or `compile-queue-root-queue'.

Meant to be used as the action for `org-runbook-execute-command-action'."
  (org-runbook--validate-command command)

  (let ((queue (or queue
                   (compile-queue-current)
                   (compile-queue--by-name compile-queue-root-queue))))
    (cl-loop
     with commands = nil
     with deferred = nil
     for subcommand in (org-runbook-command-subcommands command)
     do
     (cond ((org-runbook-subcommand-p subcommand) (push (org-runbook-subcommand-command subcommand) commands))
           ((org-runbook-elisp-subcommand-p subcommand)
            (if commands
                (let ((shell-command (compile-queue-shell-command-create
                                      :name (org-runbook-command-name command)
                                      :command (->> commands (reverse) (-non-nil) (-map #'s-trim) (s-join "; ")))))
                  (setq deferred
                        (deferred:$
                          (if deferred
                              (deferred:nextc deferred)
                            (lambda (&rest _) (compile-queue-schedule queue shell-command))
                            (compile-queue-schedule queue shell-command))
                          (deferred:nextc it `(lambda () ,(org-runbook-elisp-subcommand-elisp subcommand)))))
                  (setq commands nil))
              (setq deferred
                    (deferred:$
                      (if deferred
                          (deferred:nextc deferred `(lambda ()
                                                      (when-let (buffer (compile-queue-buffer-name ,queue))
                                                        (set-buffer buffer))
                                                      ,(org-runbook-elisp-subcommand-elisp subcommand)))
                        (eval (org-runbook-elisp-subcommand-elisp subcommand) t)))))))
     finally
     (when commands
       (let ((shell-command (compile-queue-shell-command-create
                             :name (org-runbook-command-name command)
                             :command (->> commands (reverse) (-non-nil) (-map #'s-trim) (s-join "; ")))))
         (if deferred
             (deferred:nextc deferred
               (lambda (&rest _) (compile-queue-schedule queue shell-command)))
           (compile-queue-schedule queue shell-command))
         (setq commands nil))))))

(define-minor-mode compile-queue-delegate-mode
  "Minor mode for buffers that delegate to the compile-queue."
  :init-value nil
  :lighter " compile-queue-delegate"
  (if compile-queue-delegate-mode
      (progn
        (unless (memq 'compile-queue-delegate-mode--forward-change after-change-functions)
          (setq-local after-change-functions
                      (append (-some->> after-change-functions (-drop-last 1)) '(compile-queue-delegate-mode--forward-change t)))))
    (setq-local after-change-functions (--filter (not (equal it #'compile-queue-delegate-mode--forward-change)) after-change-functions))))

(defun compile-queue-delegate-mode--process-filter (process output)
  "Process filter that delegates OUTPUT to the original PROCESS filter.
Also, manages scolling windows to the end if the point
is currently set at `point-max'."
  (let* ((process-buffer (process-buffer process))
         (queue (compile-queue-current process))
         (compile-queue-buffer (-some-> queue compile-queue-buffer-name get-buffer))
         (delegate
          (-some->> process-buffer
            (buffer-local-value 'compile-queue-delegate-mode--process-filter-delegate)))
         (compile-queue-end-pt (with-current-buffer (process-buffer process) (point-max)))
         (scroll-to-end (->> (get-buffer-window-list compile-queue-buffer)
                             (--filter (eq (window-point it) compile-queue-end-pt))))
         (execution (buffer-local-value 'compile-queue-delegate-mode--execution process-buffer))
         (start (marker-position (process-mark process))))
    (-some--> delegate (progn
                         (funcall it process output)))
    (when-let ((matcher (-some-> execution
                          compile-queue-execution--promise
                          compile-queue-promise--command
                          compile-queue-command--matcher)))
      (with-current-buffer process-buffer
        (goto-char start)
        (when (and (not compile-queue-delegate-mode--matcher-disabled)
                   (condition-case error
                       (if (functionp matcher)
                           (funcall matcher output)
                         output)
                     (error
                      (setq-local compile-queue-delegate-mode--matcher-disabled t)
                      (error "%s" (error-message-string error)))
                     (user-error
                      (setq-local compile-queue-delegate-mode--matcher-disabled t)
                      (error "%s" (error-message-string error)))))
          (deferred:callback
            (-> execution
                compile-queue-execution--promise
                compile-queue-promise--deferred)
            (-> execution compile-queue-execution--buffer))
          (compile-queue-execute queue))))

    (let* ((execution (buffer-local-value 'compile-queue-delegate-mode--execution process-buffer)))
      (if (and execution (compile-queue-execution-eq-id
                          execution
                          (-some-> queue compile-queue--target-execution)))
          (progn
            (with-current-buffer (compile-queue-buffer-name queue)
              (let ((pt-max (point-max)))
                (--each scroll-to-end
                  (set-window-point it pt-max)
                  (set-window-start
                   it
                   (save-excursion
                     (goto-char pt-max)
                     (forward-line (floor (- (- (window-height it 'floor) 3))))
                     (point)))))))))))

(defun compile-queue-delegate-mode--process-sentinel (process status)
  "Delegating sentinel for compile-queue.
Delegates to the original `process-sentinel' for PROCESS except
when the `process-sentinel' is the default.
Handles notifying compile queue the process STATUS on completion."
  (let* ((buffer (process-buffer process))
         (delegate
          (-some->> buffer
            (buffer-local-value 'compile-queue-delegate-mode--process-sentinel-delegate))))
    (unless (eq delegate #'internal-default-process-sentinel)
      (compile-queue--save-var-excursion ((inhibit-read-only t))
        (-some--> delegate (funcall it process status))))

    (unless (process-live-p process)
      (let* ((execution (-some--> (buffer-local-value 'compile-queue-delegate-mode--execution buffer)
                          (when (eq (compile-queue-execution--process it) process) it)))
             (command (-some-> execution compile-queue-execution--promise compile-queue-promise--command))
             (status-code (compile-queue-delegate-mode--status-code-for-process process status))
             (queue (-some-> (compile-queue-current process)))
             (is-target (when execution
                          (compile-queue-execution-eq-id
                           execution
                           (-some-> queue compile-queue--target-execution))))
             (non-zero-exit (not (eq status-code 0))))
        (when execution (setf (compile-queue-execution--status-code execution) status-code))
        (when is-target
          (setf (compile-queue--target-execution queue) nil)
          (when non-zero-exit (compile-queue-clean queue)))
        (-some->> command
          (compile-queue-command--after-complete)
          (funcall (compile-queue--callback execution)))

        (when is-target
          (progn
            (if non-zero-exit
                (-some--> execution
                  (compile-queue-execution--promise it)
                  (compile-queue-promise--deferred it)
                  (unless (deferred:status it) (deferred:errorback it (string-trim status))))
              (-some--> execution
                (compile-queue-execution--promise it)
                (compile-queue-promise--deferred it)
                (deferred:callback it (-some-> execution compile-queue-execution--buffer))))
            (unless (and (-some-> command compile-queue-command--keep-buffer))
              (let ((buffer (compile-queue-execution--buffer execution)))
                (when (and (buffer-live-p buffer) compile-queue-garbage-collect-time)
                  (with-current-buffer buffer
                    (setq-local
                     compile-queue-delegate-mode--gc-timer
                     (run-at-time compile-queue-garbage-collect-time nil
                                  (lambda () (compile-queue-delegate-mode--gc-callback execution))))))))
            (unless non-zero-exit (compile-queue-execute queue))))))))

(defun compile-queue-delegate-mode--gc-callback (execution)
  "Kill the buffer for EXECUTION if the buffer is still the same execution."
  (let ((buffer (compile-queue-execution--buffer execution)))
    (when (and (buffer-live-p buffer)
               (compile-queue-execution-eq-id
                (buffer-local-value 'compile-queue-delegate-mode--execution buffer)
                execution))
      (kill-buffer buffer))))


(when (boundp 'evil-motion-state-modes)
  (add-to-list 'evil-motion-state-modes 'compile-queue-mode))

(provide 'compile-queue)
;;; compile-queue.el ends here
