;;; integration-just-test.el --- Integration tests for Just parsing -*- lexical-binding: t; -*-

;; Integration tests for Just target parsing functionality

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(require 'tasketeer)

(defun tasketeer-integration-test--just-available-p ()
  "Check if just executable is available."
  (executable-find "just"))

(ert-deftest tasketeer-integration-test-just-parsing ()
  "Integration test for Just target parsing."
  :tags '(integration just)
  (skip-unless (tasketeer-integration-test--just-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (targets (tasketeer--justfile-targets test-project-dir))
         (expected-targets '("build" "clean" "test" "install" "dev" "lint" "format" "docs" "help" "benchmark")))
    (should (listp targets))
    (should (> (length targets) 0))
    ;; Check that we found the expected core targets
    (dolist (expected expected-targets)
      (should (member expected targets)))
    (message "All expected targets found successfully")
    (message "Found Just targets: %S" targets)
    (message "Successfully verified %d expected targets" (length expected-targets))))

(ert-deftest tasketeer-integration-test-just-command-generation ()
  "Integration test for Just command generation."
  :tags '(integration just command)
  (skip-unless (tasketeer-integration-test--just-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (runners (tasketeer--find-available-runners test-project-dir))
         (just-runner (seq-find (lambda (r) (string= (tasketeer-get-name r) "Just")) runners)))
    (should just-runner)
    (let ((command (tasketeer-make-command just-runner test-project-dir "build")))
      (should (string-match-p "^just build" command))
      (should-not (string-match-p "\\[Just\\]" command))
      (message "Generated Just command: %s" command))))

(provide 'integration-just-test)

;;; integration-just-test.el ends here
