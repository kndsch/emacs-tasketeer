# Tasketeer

Simple project-aware task runner completion for emacs.

## Features

This package provides completions for various task runners as a drop-in replacement for `compile` via `tasketeer-compile`.

The following project runners are currently supported out-of-the-box:
- [Taskfile](https://taskfile.dev/) (Task)
- [Justfile](https://github.com/casey/just) (Just)
- [Gnumake](https://www.gnu.org/software/make/) (Make)

## Installation

Tasketeer is currently available on GitHub only.

### Using package.el

```elisp
;; Add to your init.el
(unless (package-installed-p 'tasketeer)
  (package-vc-install "https://github.com/kndsch/emacs-tasketeer"))

(require 'tasketeer)

;; Optional: Set up key binding
(global-set-key (kbd "C-c c") 'tasketeer-compile)
```

### Using use-package with :vc (Emacs 29+)

```elisp
(use-package tasketeer
  :vc (:url "https://github.com/kndsch/emacs-tasketeer"))
```

### Using straight.el with use-package

```elisp
(use-package tasketeer
  :straight (:host github :repo "kndsch/emacs-tasketeer")
  :bind ("C-c c" . tasketeer-compile))
```

### Using elpaca with use-package

```elisp
(use-package tasketeer
  :ensure (:host github :repo "kndsch/emacs-tasketeer"))
```

## Adding your own runner configuration

Tasketeer makes it easy to add support for new task runners using the `tasketeer-register` function. This allows you to extend Tasketeer to work with any build system or task runner.

### Basic Registration

```elisp
(tasketeer-register
 :name "MyRunner"           ; Display name shown in completion
 :executable "my-build-tool" ; Required executable (optional)
 :detect "my-build-file"    ; How to detect this runner in projects
 :targets '("build" "test" "deploy")) ; Available targets
```

### Configuration Options

#### `:name` (required)
The display name shown in the completion interface (e.g., "Make", "Task", "Just").

#### `:executable` (optional)
The command-line executable for this runner. Serves two purposes:

1. **Detection requirement**: If specified, the runner will only be detected in projects where this executable is available in PATH
2. **Default command**: If no `:command` is specified, defaults to `"EXECUTABLE TARGET"`

```elisp
:executable "cargo"  ; Requires 'cargo' in PATH, default command: "cargo TARGET"
:executable nil      ; No executable requirement, default command: "TARGET"
```

#### `:detect` (required)
How to detect if this runner is available in a project directory. Can be:

**String (file pattern):** Checks for files matching a regex pattern
```elisp
:detect "Cargo.toml"           ; Exact filename
:detect "\\(?:[Mm]akefile\\)"  ; Regex pattern for Makefile variations
:detect "\\.?[Jj]ustfile"      ; Pattern for justfile/Justfile/.justfile
```

**Function:** Custom detection logic taking project root as argument
```elisp
:detect (lambda (root)
          (and (file-exists-p (expand-file-name "package.json" root))
               (file-exists-p (expand-file-name "scripts/" root))))
```

#### `:targets` (required)
How to get the list of available targets. Can be:

**List:** Static list of targets
```elisp
:targets '("build" "test" "clean" "install")
```

**Function:** Dynamic target discovery taking project root as argument
```elisp
:targets (lambda (root)
           (with-temp-buffer
             (call-process "my-tool" nil t nil "--list-targets")
             (split-string (buffer-string) "\n" t)))
```

#### `:command` (optional)
How to generate the command string. Defaults to `"EXECUTABLE TARGET"`.

**String template:** Use `%s` as placeholder for target
```elisp
:command "my-tool run %s --verbose"
```

**Function:** Custom command generation taking root and target as arguments
```elisp
:command (lambda (root target)
           (format "cd %s && my-tool %s --config=production" root target))
```

## Similar projects
- [helm-make](https://github.com/abo-abo/helm-make): The main inspiration for this project. The makefile support is mostly borrowed from helm-make.
- [emacs-taskrunner](https://github.com/emacs-taskrunner/emacs-taskrunner): A similar project with much broader support for various task and build systems. Sadly it does not seem to be actively maintained.
