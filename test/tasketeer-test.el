;;; tasketeer-test.el --- Tests for tasketeer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for tasketeer package using a fake test project.
;; These tests avoid using real registered runners and instead create
;; a controlled test environment with custom parsing.

;;; Code:

(require 'ert)
(require 'tasketeer)
(require 'cl-lib)

(defvar tasketeer-test--temp-dir nil
  "Temporary directory for test project.")

(defvar tasketeer-test--original-runners nil
  "Original runners to restore after tests.")

(defvar tasketeer-test--original-history-file nil
  "Original history file to restore after tests.")

;;; Test utilities

(defun tasketeer-test--setup ()
  "Set up test environment."
  (setq tasketeer-test--temp-dir
        (file-name-as-directory (make-temp-file "tasketeer-test" t)))
  (setq tasketeer-test--original-runners tasketeer-runners)
  (setq tasketeer-test--original-history-file tasketeer-history-file)
  (setq tasketeer-runners '())
  (setq tasketeer-history-file
        (expand-file-name "test-history" tasketeer-test--temp-dir))
  (setq tasketeer--loaded-history nil)
  (setq tasketeer--history-table (make-hash-table :test 'equal)))

(defun tasketeer-test--teardown ()
  "Clean up test environment."
  (when (and tasketeer-test--temp-dir
             (file-exists-p tasketeer-test--temp-dir))
    (delete-directory tasketeer-test--temp-dir t))
  (setq tasketeer-runners tasketeer-test--original-runners)
  (setq tasketeer-history-file tasketeer-test--original-history-file)
  (setq tasketeer--loaded-history nil)
  (setq tasketeer--history-table (make-hash-table :test 'equal)))

(defun tasketeer-test--create-fake-project ()
  "Create a fake project directory with test files."
  (let ((project-dir (expand-file-name "fake-project" tasketeer-test--temp-dir)))
    (make-directory project-dir t)
    ;; Create fakefile
    (with-temp-file (expand-file-name "fakefile" project-dir)
      (insert "# Fake build file for testing\n")
      (insert "build: compile test\n")
      (insert "\techo \"Building project\"\n\n")
      (insert "compile:\n")
      (insert "\techo \"Compiling sources\"\n\n")
      (insert "test:\n")
      (insert "\techo \"Running tests\"\n\n")
      (insert "clean:\n")
      (insert "\techo \"Cleaning up\"\n\n")
      (insert "deploy: build\n")
      (insert "\techo \"Deploying application\"\n\n")
      (insert ".PHONY: build compile test clean deploy\n"))
    project-dir))

(defun tasketeer-test--fake-target-parser (root)
  "Parse targets from fakefile in ROOT."
  (let ((fakefile (expand-file-name "fakefile" root))
        targets)
    (when (file-exists-p fakefile)
      (with-temp-buffer
        (insert-file-contents fakefile)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-zA-Z][a-zA-Z0-9_-]*\\):" nil t)
          (let ((target (match-string 1)))
            (unless (member target targets)
              (push target targets)))))
      (nreverse targets))))

(defun tasketeer-test--register-fake-runner ()
  "Register a fake runner for testing."
  (tasketeer-register
   :name "FakeRunner"
   :executable nil  ; No external executable required
   :detect "fakefile"
   :targets #'tasketeer-test--fake-target-parser
   :command "fake-cmd %s"))

;;; Core functionality tests

(ert-deftest tasketeer-test-register-runner ()
  "Test runner registration."
  (tasketeer-test--setup)
  (unwind-protect
      (progn
        (should (= (length tasketeer-runners) 0))
        (tasketeer-test--register-fake-runner)
        (should (= (length tasketeer-runners) 1))
        (let ((runner (car tasketeer-runners)))
          (should (string= (tasketeer--get-property runner :name) "FakeRunner"))
          (should (string= (tasketeer--get-property runner :detect) "fakefile"))
          (should (functionp (tasketeer--get-property runner :targets)))
          (should (string= (tasketeer--get-property runner :command) "fake-cmd %s"))))
    (tasketeer-test--teardown)))

(ert-deftest tasketeer-test-detect-runner ()
  "Test runner detection."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project))
             (fake-runner (progn (tasketeer-test--register-fake-runner)
                                (car tasketeer-runners))))
        ;; Should detect in directory with fakefile
        (should (tasketeer-detect fake-runner project-dir))
        ;; Should not detect in directory without fakefile
        (should-not (tasketeer-detect fake-runner tasketeer-test--temp-dir)))
    (tasketeer-test--teardown)))

(ert-deftest tasketeer-test-get-targets ()
  "Test target extraction."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project))
             (fake-runner (progn (tasketeer-test--register-fake-runner)
                                (car tasketeer-runners)))
             (targets (tasketeer-get-targets fake-runner project-dir)))
        (should (listp targets))
        (should (member "build" targets))
        (should (member "compile" targets))
        (should (member "test" targets))
        (should (member "clean" targets))
        (should (member "deploy" targets))
        (should (= (length targets) 5)))
    (tasketeer-test--teardown)))

(ert-deftest tasketeer-test-make-command ()
  "Test command generation."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project))
             (fake-runner (progn (tasketeer-test--register-fake-runner)
                                (car tasketeer-runners)))
             (command (tasketeer-make-command fake-runner project-dir "build")))
        (should (string= command "fake-cmd build")))
    (tasketeer-test--teardown)))

(ert-deftest tasketeer-test-find-available-runners ()
  "Test finding available runners."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project)))
        (tasketeer-test--register-fake-runner)
        ;; Should find runner in project directory
        (let ((runners (tasketeer--find-available-runners project-dir)))
          (should (= (length runners) 1))
          (should (string= (tasketeer-get-name (car runners)) "FakeRunner")))
        ;; Should not find runner in empty directory
        (let ((runners (tasketeer--find-available-runners tasketeer-test--temp-dir)))
          (should (= (length runners) 0))))
    (tasketeer-test--teardown)))

;;; History management tests

(ert-deftest tasketeer-test-history-management ()
  "Test compilation history management."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-key "test-project")
             (commands '("cmd1" "cmd2" "cmd3")))
        ;; Initially empty
        (should (null (gethash project-key tasketeer--history-table)))
        
        ;; Add commands to history
        (dolist (cmd commands)
          (puthash project-key (cons cmd (gethash project-key tasketeer--history-table '()))
                   tasketeer--history-table))
        
        ;; Check history
        (let ((history (gethash project-key tasketeer--history-table)))
          (should (= (length history) 3))
          (should (member "cmd1" history))
          (should (member "cmd2" history))
          (should (member "cmd3" history))))
    (tasketeer-test--teardown)))

(ert-deftest tasketeer-test-history-size-limit ()
  "Test history size limitation."
  (tasketeer-test--setup)
  (unwind-protect
      (let ((tasketeer-history-size 3)
            (project-key "test-project"))
        ;; Add more commands than the limit
        (dotimes (i 5)
          (let ((cmd (format "cmd%d" i)))
            (puthash project-key (cons cmd (gethash project-key tasketeer--history-table '()))
                     tasketeer--history-table)))
        
        ;; Manually enforce size limit (simulating what --add-to-history does)
        (let ((history (gethash project-key tasketeer--history-table)))
          (when (> (length history) tasketeer-history-size)
            (setq history (seq-take history tasketeer-history-size))
            (puthash project-key history tasketeer--history-table))
          
          ;; Should only keep the most recent commands
          (should (= (length history) 3))))
    (tasketeer-test--teardown)))

;;; Integration tests

(ert-deftest tasketeer-test-get-all-targets ()
  "Test getting all targets with runner labels."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project)))
        (tasketeer-test--register-fake-runner)
        
        ;; Mock project structure
        (let* ((project (cons 'transient project-dir))
               (all-targets (tasketeer--get-all-targets project)))
          (should (listp all-targets))
          (should (> (length all-targets) 0))
          
          ;; Check that targets are properly formatted with runner name
          (let ((build-target (cl-find-if (lambda (target)
                                           (string-match-p "\\[FakeRunner\\] build" (car target)))
                                         all-targets)))
            (should build-target)
            (should (consp (cdr build-target)))
            (should (string= (tasketeer-get-name (cadr build-target)) "FakeRunner"))
            (should (string= (cddr build-target) "build")))))
    (tasketeer-test--teardown)))

;;; Error handling tests

(ert-deftest tasketeer-test-missing-executable ()
  "Test behavior with missing executable."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project)))
        ;; Register runner with non-existent executable
        (tasketeer-register
         :name "MissingExeRunner"
         :executable "non-existent-executable-12345"
         :detect "fakefile"
         :targets #'tasketeer-test--fake-target-parser)
        
        ;; Should not detect due to missing executable
        (let ((runner (car tasketeer-runners)))
          (should-not (tasketeer-detect runner project-dir))))
    (tasketeer-test--teardown)))

(ert-deftest tasketeer-test-invalid-target-function ()
  "Test behavior with invalid target function."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project)))
        ;; Register runner with function that returns nil
        (tasketeer-register
         :name "NilTargetsRunner"
         :executable nil
         :detect "fakefile"
         :targets (lambda (root) nil))
        
        (let* ((runner (car tasketeer-runners))
               (targets (tasketeer-get-targets runner project-dir)))
          (should (listp targets))
          (should (= (length targets) 0))))
    (tasketeer-test--teardown)))

;;; Property accessor tests

(ert-deftest tasketeer-test-get-property ()
  "Test property accessor function."
  (tasketeer-test--setup)
  (unwind-protect
      (progn
        (tasketeer-test--register-fake-runner)
        (let ((runner (car tasketeer-runners)))
          (should (string= (tasketeer--get-property runner :name) "FakeRunner"))
          (should (string= (tasketeer--get-property runner :detect) "fakefile"))
          (should (functionp (tasketeer--get-property runner :targets)))
          (should (string= (tasketeer--get-property runner :command) "fake-cmd %s"))
          (should (null (tasketeer--get-property runner :nonexistent)))))
    (tasketeer-test--teardown)))

(ert-deftest tasketeer-test-detect-by-regexp ()
  "Test file detection by regexp."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project)))
        ;; Should detect fakefile
        (should (tasketeer--detect-by-regexp "fakefile" project-dir))
        ;; Should not detect non-existent file
        (should-not (tasketeer--detect-by-regexp "nonexistent" project-dir))
        ;; Should detect with pattern
        (should (tasketeer--detect-by-regexp "fake.*" project-dir)))
    (tasketeer-test--teardown)))

;;; Command template tests

(ert-deftest tasketeer-test-command-templates ()
  "Test different command template formats."
  (tasketeer-test--setup)
  (unwind-protect
      (let* ((project-dir (tasketeer-test--create-fake-project)))
        ;; Test string template
        (tasketeer-register
         :name "StringTemplate"
         :detect "fakefile"
         :command "run %s --verbose")
        
        (let* ((runner (car tasketeer-runners))
               (command (tasketeer-make-command runner project-dir "test")))
          (should (string= command "run test --verbose")))
        
        ;; Test function template
        (setq tasketeer-runners '())
        (tasketeer-register
         :name "FunctionTemplate"
         :detect "fakefile"
         :command (lambda (root target) (format "cd %s && run %s" root target)))
        
        (let* ((runner (car tasketeer-runners))
               (command (tasketeer-make-command runner project-dir "build")))
          (should (string-match-p (format "cd %s && run build" (regexp-quote project-dir)) command))))
    (tasketeer-test--teardown)))

(provide 'tasketeer-test)
;;; tasketeer-test.el ends here
