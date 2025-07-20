;;; integration-taskfile-test.el --- Integration tests for Taskfile parsing -*- lexical-binding: t; -*-

;; Integration tests for Taskfile target parsing functionality

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(require 'tasketeer)

(defun tasketeer-integration-test--task-available-p ()
  "Check if task executable is available."
  (executable-find "task"))

(ert-deftest tasketeer-integration-test-taskfile-parsing ()
  "Integration test for Taskfile target parsing."
  :tags '(integration task)
  (skip-unless (tasketeer-integration-test--task-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (targets (tasketeer--taskfile-targets test-project-dir))
         (expected-targets '("build" "clean" "test" "install" "dev" "lint" "format" "deploy" "benchmark")))
    (should (listp targets))
    (should (> (length targets) 0))
    ;; Check that we found the expected core targets
    (dolist (expected expected-targets)
      (should (member expected targets)))
    (message "All expected targets found successfully")
    (message "Found Taskfile targets: %S" targets)
    (message "Successfully verified %d expected targets" (length expected-targets))))

(ert-deftest tasketeer-integration-test-taskfile-command-generation ()
  "Integration test for Taskfile command generation."
  :tags '(integration task command)
  (skip-unless (tasketeer-integration-test--task-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (runners (tasketeer--find-available-runners test-project-dir))
         (taskfile-runner (seq-find (lambda (r) (string= (tasketeer-get-name r) "Task")) runners)))
    (should taskfile-runner)
    (let ((command (tasketeer-make-command taskfile-runner test-project-dir "build")))
      (should (string-match-p "^task build" command))
      (should-not (string-match-p "\\[Task\\]" command))
      (message "Generated Taskfile command: %s" command))))

(ert-deftest tasketeer-integration-test-taskfile-choice-parsing ()
  "Integration test for parsing formatted choice strings."
  :tags '(integration task command)
  (skip-unless (tasketeer-integration-test--task-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (project (cons 'transient test-project-dir))
         (targets (tasketeer--get-all-targets project))
         (task-target (seq-find (lambda (t) (string-match-p "\\[Task\\] build" (car t))) targets))
         (choice-text (car task-target)))
    (should task-target)
    (should (string= choice-text "[Task] build"))
    ;; Test that when this formatted choice is processed, it produces the right command
    (let* ((runner-target (cdr task-target))
           (runner (car runner-target))
           (target (cdr runner-target))
           (command (tasketeer-make-command runner test-project-dir target)))
      (should (string= target "build")) ; Target should be just "build", not "[Task] build"
      (should (string-match-p "^task build" command))
      (should-not (string-match-p "\\[Task\\]" command))
      (message "Parsed target: %s, Generated command: %s" target command))))

(ert-deftest tasketeer-integration-test-read-command-logic ()
  "Integration test for read command logic bug."
  :tags '(integration task command)
  (skip-unless (tasketeer-integration-test--task-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (project (cons 'transient test-project-dir)))
    ;; Mock completing-read to return the formatted choice
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt choices &rest _)
                 "[Task] build"))) ; Simulate user selecting formatted target
      (let ((command (tasketeer--read-command project)))
        ;; This should fail with current code - command will be "[Task] build" instead of "task build"
        (should-not (string= command "[Task] build"))
        (should (string-match-p "^task build" command))
        (message "Read command result: %s" command)))))

(provide 'integration-taskfile-test)

;;; integration-taskfile-test.el ends here
