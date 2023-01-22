;;; -*- lexical-binding: t -*-

(defmacro compile-queue-defstruct (name &rest rest)
  "Helper function for adding the usual defaults for compile-queue structs such as
using -create and -copy for 
See `cl-defstruct'"
  (declare (indent 1))
  (let ((struct-name (symbol-name (if (listp name) (car name) name))))
    `(cl-defstruct (,(intern struct-name)
                    (:conc-name ,(intern (concat struct-name "--")))
                    (:constructor ,(intern (concat struct-name "-create")))
                    (:copier ,(intern (concat struct-name "-copy")))
                    ,@(when (consp name) (cdr name)))
       ,@rest)))

(compile-queue-defstruct compile-queue
  "Use `compile-queue-current' to get an instance of this type."
  (id (uuid-string))
  name
  buffer-name
  status
  target-execution
  scheduled
  outputting-executions)

(compile-queue-defstruct compile-queue-command
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
  after-complete
  group)

(cl-defgeneric compile-queue-command--start-process (command))
(cl-defgeneric compile-queue-command--init-buffer (command))
(cl-defgeneric compile-queue-command--execute (command promise queue group))

(cl-defgeneric compile-queue-command-restart (command execution))
(cl-defmethod compile-queue-command-restart (command execution)
  (error "Unknown command type command: %S, execution: %S." command execution))

(compile-queue-defstruct compile-queue-group
  "Compile-Queue commands can belong to a group.
Groups are mainly used for managing commands that do not make sense to run in isolation."
  (id (uuid-string))
  name
  commands
  before-start
  deferred
  after-complete
  parent-group)

(defun compile-queue-group-name (group)
  (s-join ":"
          (cl-loop
           for parent = group 
           then (compile-queue-group--parent-group parent)
           while parent
           collect (compile-queue-group--name parent))))

(compile-queue-defstruct compile-queue-execution
  "Represents an individual execution of a `compile-queue-command'.
`compile-queue-execution--buffer' is the buffer that the execution to which the execution outputs.
`compile-queue-execution-queue' is the queue on which the buffer is being executed.
"
  (id (uuid-string))
  promise
  buffer
  process
  queue
  status-code
  group-execution)

(defun compile-queue-execution-restart (execution)
  "Restart the EXECUTION without triggering any deferred callbacks."
  (or (compile-queue-execution-p execution) (error "Unexpected type passed: %S" execution))
  (let ((command (compile-queue-execution-command execution)))
    (compile-queue-command-restart command execution)))

(defun compile-queue-execution-command (command)
  (-some-> command compile-queue-execution--promise compile-queue-promise--command))

(defun compile-queue-execution-queue (command)
  (-some-> (compile-queue-execution--queue command) compile-queue-current))

(defun compile-queue-execution-buffer (execution)
  (pcase execution
    ((pred (compile-queue-execution-p))
      (compile-queue-execution--buffer execution))))

(defun compile-queue-execution-name (execution)
  (or (compile-queue-execution-p execution)
      (error "Unexpected type for execution %S" execution))
  (let* ((command  (-some-> execution compile-queue-execution--promise
                            compile-queue-promise--command)))
    (or
     (-some-> command compile-queue-command--name)
     (-some-> execution compile-queue-execution--buffer buffer-name)
     (when (compile-queue-shell-command-p command) (-some-> command compile-queue-shell-command-full-command)))))

(defun compile-queue-execution-eq-id (lhs rhs)
  "Return non-nil if LHS id == RHS id."
  (when (and lhs rhs
             (compile-queue-execution-p lhs)
             (compile-queue-execution-p rhs))
    (string= (-> lhs compile-queue-execution--id)
             (-> rhs compile-queue-execution--id))))

(compile-queue-defstruct compile-queue-group-execution
  "Represents an individual execution of a compile-queue-group.
`compile-queue-group-execution--group' is the group definition of the group.
`compile-queue-group-execution--scheduled' is a list of `compile-queue-command' and `compile-queue-group'"
  (id (uuid-string))
  parent-group-execution
  group
  scheduled)

(defun compile-queue-group-restart (execution)
  "Restart the EXECUTION without triggering any deferred callbacks."
  (or (compile-queue-execution-p execution) (error "Unexpected type passed: %S" execution))
  (let ((command (compile-queue-execution-command execution)))
    (compile-queue-command-restart command execution)))

(compile-queue-defstruct compile-queue-promise
  "Represents a command and a deferred object that represents the completion of an execution of the command"
  (id (uuid-string))
  command
  deferred)

(compile-queue-defstruct (compile-queue-shell-command
                          (:include compile-queue-command))
  "Represents a shell command.

:command The shell command as an already escaped string
:env A cons list of environment variables like ((\"KEY2\" . \"VALUE2\") (\"KEY2\" . \"VALUE2\"))
:major-mode The major mode to use for the buffer that handles the output from the shell command
:vars Lisp Variables to set after the major mode in the buffer that handles the output from the shell command
:default-directory The default-directory to use when executing the shell command
:buffer-name The name of the buffer that will handle the output from the shell command.
:pty t if this requires a pty
If nil, it defaults to a truncated version of the command."
  command
  env
  major-mode
  vars
  default-directory
  buffer-name
  pty)

(compile-queue-defstruct (compile-queue-ssh-shell-command
                          (:include compile-queue-shell-command))
  "Represents a ssh shell command.
:host The host to run the command on.
See `compile-queue-shell-command'
"
  host)

;; Do not want the symbolp validation that is created by default
(put 'compile-queue-shell-command-create 'compiler-macro nil)
(put 'compile-queue-ssh-shell-command-create 'compiler-macro nil)

(provide 'compile-queue-structs)
;;; compile-queue-structs.el ends here
