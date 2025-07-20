;;; tasketeer.el --- Compilation with task runner completion -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: kndsch
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes, convenience
;; URL: https://github.com/kndsch/emacs-tasketeer

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; This package provides a new compilation command with
;; intelligent completion for various task runners (Make, Taskfile, Justfile).
;; It integrates with project.el to provide project-aware compilation history
;; and automatic detection of available task runners.

;;; Code:

(require 'compile)
(require 'project)
(require 'cl-lib)
(require 'json)

(defgroup tasketeer nil
  "Enhanced compilation with task runner completion."
  :group 'tools
  :prefix "tasketeer-")

(defcustom tasketeer-history-file
  (expand-file-name "tasketeer-history" user-emacs-directory)
  "File to save project compilation history."
  :type 'file
  :group 'tasketeer)

(defcustom tasketeer-history-size 50
  "Maximum number of commands to save per project."
  :type 'integer
  :group 'tasketeer)

(defcustom tasketeer-runners '()
  "List of registered runners."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'tasketeer)

(defvar tasketeer--history-table (make-hash-table :test 'equal)
  "Hash table mapping project roots to compilation history.")

(defvar tasketeer--loaded-history nil
  "Whether history has been loaded from file.")

;;; Core runner abstraction

(defun tasketeer-register (&rest args)
  "Register a new task runner with ARGS.
ARGS should be a plist with keys:
:name - Name of the runner (string)
:executable - Required executable (string, optional)
:detect - Detection method (string path or function)
:targets - Target source (function or list)
:command - Command template (string, defaults to 'EXECUTABLE TARGET')"
  (let ((runner (cl-loop for (key value) on args by #'cddr
                         collect (cons key value))))
    (add-to-list 'tasketeer-runners runner)))

(defun tasketeer--get-property (runner prop)
  "Get property PROP from RUNNER alist."
  (cdr (assq prop runner)))

(defun tasketeer-detect (runner root)
  "Check if RUNNER is available in project ROOT."
  (let ((detect (tasketeer--get-property runner :detect))
        (executable (tasketeer--get-property runner :executable)))
    (and (or (not executable) (executable-find executable))
         (cond
          ((stringp detect)
           (tasketeer--detect-by-regexp detect root))
          ((functionp detect)
           (funcall detect root))
          (t nil)))))

(defun tasketeer--detect-by-regexp (regexp root)
  "Check if any file in ROOT matches REGEXP."
  (let ((files (directory-files root nil regexp)))
    (cl-some (lambda (file)
               (file-exists-p (expand-file-name file root)))
             files)))

(defun tasketeer-get-targets (runner root)
  "Get available targets for RUNNER in project ROOT."
  (let ((targets (tasketeer--get-property runner :targets)))
    (cond
     ((functionp targets)
      (funcall targets root))
     ((listp targets)
      targets)
     (t '()))))

(defun tasketeer-make-command (runner root target)
  "Create command string for RUNNER in ROOT with TARGET."
  (let ((command-template (tasketeer--get-property runner :command))
        (executable (tasketeer--get-property runner :executable)))
    (cond
     (command-template
      (if (functionp command-template)
          (funcall command-template root target)
        (format command-template target)))
     (executable
      (concat executable " " target))
     (t target))))

(defun tasketeer-get-name (runner)
  "Get name of RUNNER."
  (tasketeer--get-property runner :name))

(defun tasketeer--find-available-runners (root)
  "Find all available runners for project at ROOT."
  (cl-loop for runner in tasketeer-runners
           when (tasketeer-detect runner root)
           collect runner))

;;; History management

(defun tasketeer--project-key (project)
  "Get a unique key for PROJECT."
  (if project
      (project-root project)
    "global"))

(defun tasketeer--load-history ()
  "Load history from file."
  (when (and (not tasketeer--loaded-history)
             (file-exists-p tasketeer-history-file))
    (with-temp-buffer
      (insert-file-contents tasketeer-history-file)
      (when (> (buffer-size) 0)
        (let ((data (read (current-buffer))))
          (when (hash-table-p data)
            (setq tasketeer--history-table data)))))
    (setq tasketeer--loaded-history t)))

(defun tasketeer--save-history ()
  "Save history to file."
  (with-temp-file tasketeer-history-file
    (prin1 tasketeer--history-table (current-buffer))))

(defun tasketeer--get-history (project)
  "Get compilation history for PROJECT."
  (tasketeer--load-history)
  (let ((key (tasketeer--project-key project)))
    (or (gethash key tasketeer--history-table)
        '())))

(defun tasketeer--add-to-history (project command)
  "Add COMMAND to compilation history for PROJECT."
  (tasketeer--load-history)
  (let* ((key (tasketeer--project-key project))
         (history (tasketeer--get-history project))
         (history (delete command history))
         (history (cons command history)))
    (when (> (length history) tasketeer-history-size)
      (setq history (seq-take history tasketeer-history-size)))
    (puthash key history tasketeer--history-table)
    (tasketeer--save-history)))

;;; Built-in runners

                                        ; Adapted from helm-make
(defun tasketeer--make-targets (root)
  "Get Make targets from Makefile in ROOT."
  (let ((default-directory root)
        (process-environment (cons "LC_ALL=C" process-environment))
        targets)
    (with-temp-buffer
      (call-process "make" nil t nil "-nqp")
      (when (> (buffer-size) 0)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^%$:#\n\t ]+\\):\\([^=]\\|$\\)" nil t)
          (let ((target (match-string 1)))
            (unless (or (save-excursion
                          (goto-char (match-beginning 0))
                          (forward-line -1)
                          (looking-at "^# Not a target:"))
                        (string-match "^\\([/a-zA-Z0-9_. -]+/\\)?\\." target))
              (push target targets))))))
    (delete-dups (nreverse targets))))


(defun tasketeer--taskfile-targets (root)
  "Get Task targets from Taskfile in ROOT using JSON output."
  (let ((default-directory root)
        targets)
    (with-temp-buffer
      (when (zerop (call-process "task" nil t nil "--list" "--json"))
        (goto-char (point-min))
        (condition-case nil
            (let* ((json-data (json-read))
                   (tasks (cdr (assoc 'tasks json-data))))
              (when (arrayp tasks)
                (dotimes (i (length tasks))
                  (let* ((task (aref tasks i))
                         (name (cdr (assoc 'name task))))
                    (when name
                      (push name targets))))))
          (error nil))))
    (nreverse targets)))

(defun tasketeer--justfile-targets (root)
  "Get Just recipes from Justfile in ROOT."
  (let ((default-directory root)
        targets)
    (with-temp-buffer
      (when (zerop (call-process "just" nil t nil "--list"))
        (goto-char (point-min))
        (while (re-search-forward "^    \\([a-zA-Z0-9][a-zA-Z0-9_-]*\\)" nil t)
          (push (match-string 1) targets))))
    (nreverse targets)))

;; Register built-in runners
(tasketeer-register
 :name "Make"
 :executable "make"
 :detect "\\(?:GNU\\)?[Mm]akefile"
 :targets #'tasketeer--make-targets)

(tasketeer-register
 :name "Task"
 :executable "task"
 :detect "\\(?:[Tt]askfile\\)\\.ya?ml"
 :targets #'tasketeer--taskfile-targets)

(tasketeer-register
 :name "Just"
 :executable "just"
 :detect "\\.?[Jj]ustfile"
 :targets #'tasketeer--justfile-targets)

;;; Main UI

(defun tasketeer--get-all-targets (project)
  "Get all available targets for PROJECT."
  (let* ((root (if project (project-root project) default-directory))
         (runners (tasketeer--find-available-runners root))
         all-targets)
    (dolist (runner runners)
      (let ((targets (tasketeer-get-targets runner root)))
        (dolist (target targets)
          (push (cons (format "[%s] %s" (tasketeer-get-name runner) target)
                      (cons runner target))
                all-targets))))
    (nreverse all-targets)))

(defun tasketeer--read-command (project)
  "Read a compilation command for PROJECT with completion."
  (let* ((history (tasketeer--get-history project))
         (targets (tasketeer--get-all-targets project))
         (all-choices (append (mapcar (lambda (cmd) (cons cmd cmd)) history)
                              targets))
         (choice (completing-read "Compile command: " all-choices nil nil)))
    (let ((runner-target (cdr (assoc choice all-choices))))
      (if runner-target
          ;; Choice is a formatted target entry - generate command
          (let* ((runner (car runner-target))
                 (target (cdr runner-target))
                 (root (if project (project-root project) default-directory)))
            (tasketeer-make-command runner root target))
        ;; Choice is a direct command string (from history)
        choice))))

;;;###autoload
(defun tasketeer-compile ()
  "Run compilation with task runner completion."
  (interactive)
  (let* ((project (project-current))
         (command (tasketeer--read-command project))
         (default-directory (if project (project-root project) default-directory)))
    (tasketeer--add-to-history project command)
    (compile command)))

;;;###autoload
(defun tasketeer-list-runners ()
  "List available task runners for current project."
  (interactive)
  (let* ((project (project-current))
         (root (if project (project-root project) default-directory))
         (runners (tasketeer--find-available-runners root)))
    (if runners
        (message "Available runners: %s"
                 (mapconcat #'tasketeer-get-name runners ", "))
      (message "No task runners found in %s" root))))

;;;###autoload
(defun tasketeer-clear-history ()
  "Clear compilation history for current project."
  (interactive)
  (let* ((project (project-current))
         (key (tasketeer--project-key project)))
    (tasketeer--load-history)
    (remhash key tasketeer--history-table)
    (tasketeer--save-history)
    (message "Cleared history for %s" (or (and project (project-root project)) "global"))))

(provide 'tasketeer)
;;; tasketeer.el ends here
