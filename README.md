# compile-queue.el
[![License](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Version](https://img.shields.io/github/v/tag/tyler-dodge/compile-queue)](https://github.com/tyler-dodge/compile-queue/releases)
[![Build Status](https://travis-ci.com/tyler-dodge/compile-queue.svg?branch=master)](https://travis-ci.com/github/tyler-dodge/compile-queue)
[![Coverage Status](https://coveralls.io/repos/github/tyler-dodge/compile-queue/badge.svg)](https://coveralls.io/github/tyler-dodge/compile-queue)
---

The package for running lists of commands, while viewing their output.

## Installation

Coming soon to MELPA!

## Features

* Combine shell commands sequentially interactively.
* No blocking user input for process output.
* Match against process output as it streams, Ex: regexp search for Server listening on Port.
* Named buffers for output buffers.
If a matcher returns true, the output buffer stays alive until the process dies,
while the queue continues executing the remaining scheduled commands.
* Callbacks based on the process's lifecycle
* [org-runbook.el](https://github.com/tyler-dodge/org-runbook) integration
* [deferred.el](https://github.com/kiwanami/emacs-deferred) integration

## Usage

```
(compile-queue-$ QUEUE-NAME &rest COMMANDS)
```
compile-queue-$ is a macro for chaining COMMANDS on the compile-queue.
Fully compatible with [deferred.el's](https://github.com/kiwanami/emacs-deferred) `deferred:$`

QUEUE-NAME is optional.

Currently there are 2 special types

### (shell &rest COMMAND)
---

```
(shell &rest COMMAND)
(! &rest COMMAND)
```

Run the command specified by joining
the list of COMMAND with spaces


```
(deferred-shell &rest COMMAND)
(!deferred &rest COMMAND)
```

Waits to schedule the command until the deferred chain before
this has already completed.


### (org-runbook &rest COMMAND)
---

```
(org-runbook &rest COMMAND)

(> &rest COMMAND)

(deferred-org-runbook &rest COMMAND)

(>deferred &rest COMMAND)
```

Run the command by matching the first org-runbook command that matches
COMMAND concatenated with >>.

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

```
(org-runbook "A >> A1")
(org-runbook "A" "A1")
```

Both resolve to the command

```
echo A
```

### Common Keywords
---

All types take the following keywords:


#### :env
A cons list of environment variables like (("KEY2" . "VALUE2") ("KEY2" . "VALUE2"))

#### :major-mode
The major mode to use for the buffer that handles the output from the shell command

#### :default-directory
The default-directory to use when executing the shell command

#### :buffer-name
The name of the buffer that will handle the output from the shell command.

## org-runbook.el Integration

compile-queue.el can be used as a target for [org-runbook.el](https://github.com/tyler-dodge/org-runbook).

<kbd>M-x</kbd> `customize-variable` <kbd>[RET]</kbd> `org-runbook-execute-command-action` <kbd>[RET]</kbd>

Set value to `#'compile-queue-execute-org-runbook-command`

## Examples

### Running Commands in Order

```
(compile-queue-$
(shell "echo Command 1")
(shell "echo Command 2"))
```

This will run
```
echo Command 1
```

and then

```
echo Command 2
```

in the buffer specified by compile-queue-root-queue.


### Running Commands After Match

```
(compile-queue-$
(shell
    :buffer-name "*long*"
    :matcher (re-search-regexp "Command 2" nil t)
    "echo Command 1;sleep 1; echo Command 2; sleep 1; echo Command 3")
(shell "echo Next"))
```

This will run

```
echo Command 1
sleep 1
echo Command 2
```

At which point the matcher will match the string Command 2,

which will trigger the next command in the compile-queue to execute,
which will be

```
echo Next
```

in the buffer specified by compile-queue-root-queue.


```
echo Command 1
sleep 1
echo Command 2
sleep 1
echo Command 3
```

The block will still finish running even though the compile-queue is no longer displaying it. The output is accessible by viewing "*long*" since that was spceified as the buffer's name.


### Running Commands on Multiple Queues

```
(compile-queue-$ "queue-1"
  (shell "echo Queue 1;sleep 1; echo DONE"))

(compile-queue-$ "queue-2"
  (shell "echo Queue 2;sleep 2; echo DONE"))
```

### Deferring until after a compile-queue execution

```
(compile-queue-$
    (shell "echo TEST")
    (deferred:nextc it (lambda (buffer)
        (set-buffer buffer)
        (message "%s" (string-trim (buffer-string))))))
```

This will print the string "TEST" in the minibuffer.

### Resuming the queue after deferring
```
(compile-queue-$
    (shell "echo TEST; sleep 1")
     ;; `it' is set similarly to how `deferred:$' handles `it'.
    (deferred:nextc it (lambda (buffer)
        (set-buffer buffer)
        (message "%s" (string-trim (buffer-string)))))
    (deferred-shell "echo DONE"))
```

This will defer scheduling "echo DONE" until after

```
(lambda (buffer)
        (set-buffer buffer)
        (message "%s" (string-trim (buffer-string))))
```

Completes.

## Contributing

Contributions welcome, but forking preferred.
I plan to actively maintain this, but I will be prioritizing features that impact me first.

I'll look at most pull requests eventually, but there is no SLA on those being accepted.

Also, I will only respond to pull requests on a case by case basis.
I have no obligation to comment on, justify not accepting, or accept any given pull request.
Feel free to start a fork that has more support in that area.

If there's a great pull request that I'm slow on accepting, feel free to fork and rename the project.
