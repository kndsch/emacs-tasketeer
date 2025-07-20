;;; integration-make-test.el --- Integration tests for Make parsing -*- lexical-binding: t; -*-

;; Integration tests for Make target parsing functionality

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(require 'tasketeer)

(defun tasketeer-integration-test--make-available-p ()
  "Check if make executable is available."
  (or (executable-find "make")
      (executable-find "gmake")
      (executable-find "gnumake")))

(ert-deftest tasketeer-integration-test-make-parsing ()
  "Integration test for Make target parsing."
  :tags '(integration make)
  (skip-unless (tasketeer-integration-test--make-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (targets (tasketeer--make-targets test-project-dir))
         (expected-targets '("all" "build" "clean" "test" "install" "help" "dev" "lint" "format" "docs")))
    (should (listp targets))
    (should (> (length targets) 0))
    ;; Check that we found the expected core targets
    (dolist (expected expected-targets)
      (should (member expected targets)))
    (message "All expected targets found successfully")
    (message "Found Make targets: %S" targets)
    (message "Successfully verified %d expected targets" (length expected-targets))))

(ert-deftest tasketeer-integration-test-make-command-generation ()
  "Integration test for Make command generation."
  :tags '(integration make command)
  (skip-unless (tasketeer-integration-test--make-available-p))
  (let* ((test-project-dir (expand-file-name "test-project" (file-name-directory load-file-name)))
         (runners (tasketeer--find-available-runners test-project-dir))
         (make-runner (seq-find (lambda (r) (string= (tasketeer-get-name r) "Make")) runners)))
    (should make-runner)
    (let ((command (tasketeer-make-command make-runner test-project-dir "build")))
      (should (string-match-p "^make build" command))
      (should-not (string-match-p "\\[Make\\]" command))
      (message "Generated Make command: %s" command))))

(provide 'integration-make-test)

;;; integration-make-test.el ends here
