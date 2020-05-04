# compile-queue.el
[![License](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Version](https://img.shields.io/github/v/tag/tyler-dodge/compile-queue)](https://github.com/tyler-dodge/compile-queue/releases)
[![Build Status](https://travis-ci.org/tyler-dodge/compile-queue.svg?branch=master)](https://travis-ci.org/github/tyler-dodge/compile-queue)
[![Coverage Status](https://coveralls.io/repos/github/tyler-dodge/compile-queue/badge.svg?branch=master)](https://coveralls.io/github/tyler-dodge/compile-queue)

---

The package for running lists of commands, while viewing their output.

## Installation

Coming soon to MELPA!

## Usage

```
(compile-queue-$ QUEUE-NAME &rest COMMANDS)
```
compile-queue-$ is a macro for chaining COMMANDS on the compile-queue.
Fully compatible with [deferred.el's](https://github.com/kiwanami/emacs-deferred) deferred:$

QUEUE-NAME is optional.

Currently there are 2 special types

(shell &rest COMMAND)

(! &rest COMMAND) - run the command specified by joining
the list of COMMAND with spaces


(deferred-shell &rest COMMAND)

(!deferred &rest COMMAND) - waits to schedule the command
until the deferred chain before this has already completed.

Both shell and deferred-shell take the following as keywords

```
:env A cons list of environment variables like ((\"KEY2\" . \"VALUE2\") (\"KEY2\" . \"VALUE2\"))
```

```
:major-mode The major mode to use for the buffer that handles the output from the shell command
```

```
:default-directory The default-directory to use when executing the shell command
```


```
:buffer-name The name of the buffer that will handle the output from the shell command.
```

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
(shell :buffer-name "*long*" :matcher (re-search-regexp "Command 2" nil t) "echo Command 1;sleep 1; echo Command 2; sleep 1; echo Command 3")
(shell "echo Next"))
```

This will run
```
echo Command 1
sleep 1
echo Command 2
```

at which point the matcher will match the string Command 2,

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

## Contributing

Contributions welcome, but forking preferred.
I plan to actively maintain this, but I will be prioritizing features that impact me first.

I'll look at most pull requests eventually, but there is no SLA on those being accepted.

Also, I will only respond to pull requests on a case by case basis.
I have no obligation to comment on, justify not accepting, or accept any given pull request.
Feel free to start a fork that has more support in that area.

If there's a great pull request that I'm slow on accepting, feel free to fork and rename the project.
