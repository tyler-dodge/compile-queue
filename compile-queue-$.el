;;; -*- lexical-binding: t -*-

(defvar compile-queue-$--converters
  '(compile-queue-$--deferred-shell-command
    compile-queue-$--shell-command
    compile-queue-$--ssh-shell-command
    compile-queue-$--comint-command
    compile-queue-$--deferred-org-runbook-command
    compile-queue-$--org-runbook-command)
  "Converters for `compile-queue-$'.
A converter receives a list and if it matches return either a form
that'll be evaluated to create a `compile-queue-command' or a list
of the form (:deferred t :command FORM)
where FORM is the form that would've been evaluated.
Each are applied sequentially until one returns non-nil.")

(defmacro compile-queue-$--push-end (val place)
  "Appends `val' to store at `place'"
  (declare (debug t))
  `(setf ,place (append ,place (list ,val))))

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
         (group-bound-p-var (make-symbol "group-bound-p"))
         (commands (->> (if queue-name-is-queue commands (append (list queue-name) commands))
                        (--map (compile-queue-$--expand-group queue-var it))
                        (--map `(setq it ,it)))))
    `(let* ((it (if (boundp 'it) it nil))
            (,group-bound-p-var compile-queue-$--group)
            (,queue-var
             (compile-queue--by-name ,(if queue-name-is-queue queue-name compile-queue-root-queue))))
       (unwind-protect
           (progn
             (unless (or (not (boundp 'it)) (null it) (deferred-p it))
               (error "`it' is an unexpected type %S. `it' should be nil or a deferred object 
before `compile-queue'" it))
             ,@(->> (-drop-last 1 commands))
             (prog1 ,@(->> (last commands))))
         (unless ,group-bound-p-var (setq compile-queue-$--group nil))))))

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

(defvar compile-queue-$--group nil)

(defun compile-queue-$--expand-group (queue-var command)
  (pcase command
    (`(group ,name . ,rest)
     (let ((group-var (make-symbol "group")))
       `(progn
          ;; push the new group to the end of the scheduled for the old group
          (setq ,group-var compile-queue-$--group)
          (setq compile-queue-$--group
                (compile-queue-group-create
                 :parent-group compile-queue-$--group
                 :name  (quote ,name)))
          (unwind-protect
              (progn
                ,@(->> rest (--map (compile-queue-$--expand-group queue-var it)))
                (if ,group-var
                    (compile-queue-$--push-end compile-queue-$--group (compile-queue-group--commands ,group-var))
                  (compile-queue-schedule ,queue-var compile-queue-$--group)))
            (setq compile-queue-$--group
                  ,group-var)))))
    
    (_
     (or
      (compile-queue-$-command queue-var command)
      command))))

(cl-defun compile-queue-$-convert (list)
  "Runs each of the converters in `compile-queue-$--converters' against LIST. Returns the first match."
  (cl-loop for converter in compile-queue-$--converters
           do
           (when-let (result (funcall converter list))
             (cl-return-from compile-queue-$-convert result))))

(cl-defun compile-queue-$-command (queue-var command)
  "Macro help for function for generating a link in the compile-queue-$ chain for COMMAND.
Handles deferring if the converter returns a form (:deferred t :command).
QUEUE-VAR is the symbol of a variable that points the queue name.
"
  (if-let ((output-command (compile-queue-$-convert command)))
      (let ((protocol (or (plist-get output-command :protocol)
                          (when (or (plist-member output-command :deferred)
                                    (plist-member output-command :command))
                            'v1)
                          'unknown)))
        (pcase protocol
          ('v1
           (cond
            ((plist-get output-command :deferred)
             `(setq it (deferred:nextc it
                         (lexical-let ((,queue-var ,queue-var))
                           (lambda (&rest arg)
                             (compile-queue-schedule
                              ,queue-var
                              (let ((it nil))
                                ,(plist-get output-command :command))))))))
            (t
             (let ((command (or (plist-get output-command :command) output-command)))
               `(if compile-queue-$--group
                    (compile-queue-$--push-end
                     ,command
                     (compile-queue-group--commands compile-queue-$--group))
                    (compile-queue-schedule ,queue-var ,command))))))
          ('unknown `(setq it ,output-command))
          (_ (error "Unknown protocol %S" protocol))))))
(defun compile-queue-$--deferred-shell-command (list)
  "Return `deferred-shell' if LIST match."
  (when (memq (car list) '(deferred-shell !deferred))
    (list
     :protocol 'v1
     :deferred t
     :command
     (plist-get 
      (compile-queue-$--shell-command (cons 'shell
                                            (cdr list)))
      :command))))

(defun compile-queue-$--deferred-org-runbook-command (list)
  "Return `deferred-shell' if LIST match."
  (when (memq (car list) '(deferred-org-runbook >deferred))
    (list
     :deferred t
     :protocol 'v1
     :command
     (compile-queue-$--org-runbook-command (cons 'org-runbook (cdr list))))))

(defun compile-queue-$--shell-env-ambiguous (env)
  "Return non-nil if ENV is ambiguous."
  (pcase env
    (`(((,_ . ,_) . ,_)) nil)
    (`((,_ ,_ . ,_)) nil)
    (`((,_ . ,_)) nil)
    (`(,_ . nil) t)))

(defun compile-queue-$--shell-wrap-env (env)
  "Return wrapped ENV if it is not a list."
  (pcase env
    ('nil nil)
    (`((,_ . ,_)) env)
    (`((,_ . ,_) (,_ . ,_)) env)
    (`(,_ . nil) env)
    (`(,_ . ,_) (list env))
    (_ env)))

(defun compile-queue-$--comint-command (list)
  "Return `shell' if LIST match."
  (when (memq (car list) (list 'comint '$))
    (compile-queue-$--shell-command
     (append (list 'shell :major-mode ''comint-mode :pty t) (cdr list)))))

(defun compile-queue-$--shell-command (list)
  "Return `shell' if LIST match."
  (when (memq (car list) (list 'shell '!))
    (-let* (((plist . command-rest) (compile-queue-$--split-plist (cdr list)))
            (plist-ht (ht<-plist plist))
            (env (ht-get plist-ht :env))
            (matcher (ht-get plist-ht :matcher)))
      (when matcher (ht-remove plist-ht :matcher))
      (when env (ht-remove plist-ht :env))
      (list
       :protocol 'v1
       :command `(compile-queue-shell-command-create
                  :group compile-queue-$--group
                  :env ,(let ((env (if (and (consp env) (not (cdr env))) env env)))
                          (compile-queue-$--shell-command-expand-env env))
                  ,@(unless (ht-get plist-ht :default-directory) (list :default-directory 'default-directory))
                  ,@(ht->plist plist-ht)
                  :pty ,(plist-get plist :pty)
                  :matcher ,(cond
                             ((or (not matcher)
                                  (symbolp matcher)
                                  (and (listp matcher) (eq (car matcher) 'lambda)))
                              matcher)
                             (t `(lambda (it) ,matcher)))
                  :command
                  (s-join " " (list ,@command-rest)))))))

(defun compile-queue-$--ssh-shell-command (list)
  "Return `shell' if LIST match."
  (when (memq (car list) (list 'ssh '!!))
    (-let* ((host (cadr list))
            ((plist . command-rest) (compile-queue-$--split-plist (cddr list)))
            (plist-ht (ht<-plist plist))
            (env (ht-get plist-ht :env))
            (matcher (ht-get plist-ht :matcher)))
      (when matcher (ht-remove plist-ht :matcher))
      (when env (ht-remove plist-ht :env))
      (list
       :protocol 'v1
       :command `(compile-queue-ssh-shell-command-create
                  :group compile-queue-$--group
                  :host ,host
                  :env ,(let ((env (if (and (consp env) (not (cdr env))) env env)))
                          (compile-queue-$--shell-command-expand-env env))
                  ,@(unless (ht-get plist-ht :default-directory) (list :default-directory 'default-directory))
                  ,@(ht->plist plist-ht)
                  :pty ,(plist-get plist :pty)
                  :matcher ,(cond
                             ((or (not matcher)
                                  (symbolp matcher)
                                  (and (listp matcher) (eq (car matcher) 'lambda)))
                              matcher)
                             (t `(lambda (it) ,matcher)))
                  :command
                  (s-join " " (list ,@command-rest)))))))

(defun compile-queue-$--shell-command-expand-env (env)
  (cond
   ((not env) nil)
   ((symbolp env)
    `(let ((result ,env))
       (compile-queue-$--shell-wrap-env result)))
   ((compile-queue-$--shell-env-ambiguous env)
    (error "Current structure is ambiguous.  Use full syntax to clear the ambiguity.
Ex: `(shell :env ((KEY . nil))'.  %S" env))
   (t
    `(list ,@(->> (compile-queue-$--shell-wrap-env env) (-non-nil)
           (--map
            (prog1 `(cons ,(car it) ,(cdr it))
              (unless (consp it)
                (error "Env should be either an alist or a cons.  Found: %s." it)))))))))

(defun compile-queue-$--org-runbook-command (list)
  "Schedule a runbook command if LIST is of the form (org-runbook &rest arg)."
  (when (memq (car list) (list 'org-runbook '>))
    (-let* (((split-plist . rest) (compile-queue-$--split-plist (cdr list)))
            (plist-ht (ht<-plist split-plist)))
      (list
       :protocol 'v1
       :command
       `(let* ((command-name (s-join " >> " (list ,@rest)))
               (name ,(ht-get plist-ht :name))
               (commands
                (compile-queue--save-var-excursion ((default-directory ,(ht-get plist-ht :default-directory)))
                  (->>
                   (if-let (file ,(ht-get plist-ht :file))
                       (org-runbook-targets-from-file-by-name file)
                     (->> (org-runbook-targets)
                          (-map #'org-runbook-file-targets)))
                   (-flatten)
                   (-map #'org-runbook--shell-command-for-target)))))
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
                ,@(unless (ht-get plist-ht :name) (list :name 'command-name))
                ,@(-some--> plist-ht (prog1 it (ht-remove it :file)) (ht->plist it))
                :command it)))))))

(defun compile-queue-$--split-plist (plist)
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

(provide-me)
