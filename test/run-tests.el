;;; run-tests.el --- Test runner script -*- lexical-binding: t; -*-

;; Script to run all tasketeer tests

;;; Code:

(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'tasketeer)
(require 'tasketeer-test)

;; Load integration tests
(require 'integration-just-test)
(require 'integration-make-test)
(require 'integration-taskfile-test)

(defun run-all-tests ()
  "Run all tasketeer unit tests and report results."
  (interactive)
  (let ((ert-batch-backtrace-right-margin 80))
    (ert-run-tests-batch-and-exit "^tasketeer-test-")))

(defun run-integration-tests ()
  "Run all tasketeer integration tests and report results."
  (interactive)
  (let ((ert-batch-backtrace-right-margin 80))
    (ert-run-tests-batch-and-exit '(tag integration))))

(defun run-all-tests-including-integration ()
  "Run all tasketeer tests including integration tests."
  (interactive)
  (let ((ert-batch-backtrace-right-margin 80))
    (ert-run-tests-batch-and-exit t)))

(when noninteractive
  (cond
   ((member "--integration" command-line-args)
    (run-integration-tests))
   ((member "--all" command-line-args)
    (run-all-tests-including-integration))
   (t
    (run-all-tests))))

;;; run-tests.el ends here
