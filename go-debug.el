;;; go-debug.el --- Mojer mode for the Go debugging

;; Copyright 2016 The go-debug Authors. All rights reserved.
;; Use of this source code is governed by a MIT-style
;; license that can be found in the LICENSE file.

;; Author: The go-debug Authors
;; Version: 0.1.0
;; Keywords: debug go
;; Url: https://github.com/avatar29A/go-debug/
;;
;; This file is not part of GNU Emacs.

;;; Code

(require 'cl-lib)
(require 'go-mode)
<<<<<<< HEAD
(require 'gdg-dlv)
=======
(require 'gdg-ui)

(load-file "")
>>>>>>> b850241... Added internal structure for store breakpoints

;;; Variables

;; Current package version
(defvar gdg-version "0.1.0")

;; Active breakpoints
(defvar gdg--breakpoints ())

(defvar gdg--debuggers (make-hash-table :test 'equal))

;; Active breakpoints
(defvar gdg--breakpoints ())

;;; Internal Functions & Macros


(defun gdg--set-breakpoint (line filename)
  "Set a new breakpoint"
  (add-to-list 'gdg--breakpoints (list line filename)))

(defun gdg--remove-breakpoint (line filename)
  "Remove breakpoint"
  (setq gdg--breakpoints (remove-if (lambda (el) (and
                                                  (equal (first el) line)
                                                  (equal (second el) filename)))
                                    gdg--breakpoints)))

(defun gdg--remove-all-breakpoints ()
  (setq gdg--breakpoints ()))

(defun gdg--is-go-source-p (filename)
  (when (and
         (not (string-empty-p filename))
         (not (null filename)))
    (string-match-p ".+\\.go" filename)))

(defun gdg--check-remote-dlv-addr-p (address)
  (when (not (string-empty-p address))
    (string-match-p ".+:[0-9]" address)))

(defun gdg--set-breakpoint (line filename)
  "Set a new breakpoint"
  (add-to-list 'gdg--breakpoints (list line filename)))

(defun gdg--remove-breakpoint (line filename)
  "Remove breakpoint"
  (setq gdg--breakpoints (remove-if (lambda (el) (and
                                                  (equal (first el) line)
                                                  (equal (second el) filename)))
                                    gdg--breakpoints)))

(defun gdg--remove-all-breakpoints ()
  (setq gdg--breakpoints ()))

;;; External Functions

(defun gdg-make-debugger
    "Make a new debugger instance"
    (interactive))

(defun gdg-debug (filename)
  (interactive "fSelect file with main package: ")
  (if (gdg--is-go-source-p filename)
      (message "file was loaded.")
    (message "Expected go source file, but got %s" filename))
  "Compile and begin debugging main package in current directory, or the package specified.")

(defun gdg-attach (pid)
  "Attach to running process and begin debugging"
  (interactive "nPID: ")
  (message (gdg--make-debugger :attach  pid)))

(defun gdg-connect (address)
  (interactive "sAddress to remote dlv: ")
  (if (gdg--check-remote-dlv-addr-p address)
      (message "address: %s" address)
    (message "Expected address like 'localhost:0', but got '%s'" address))
  "Connect to a headless debug server")

(defun gdg-test (filename)
  (interactive "sSelect file with your tests: ")
  (message "This function is not implemented yet :(")
  "Compile test binary and begin debugging program.")

(defun gdg-trace (filename)
  (interactive "sSelect file with main package: ")
  (if (gdg--is-go-source-p filename)
      (message "file was loaded.")
    (message "Expected go source file, but got %s" filename))
  "Compile and begin tracing program")

(defun gdg-version ()
  "Show dlv and plugin's version"
  (interactive)
  (dlv
   (let* (
          (out (shell-command-to-string (format "%s version" path-to-dlv)))
          (lines (split-string out "\n"))
          (dlv-version (second lines)))
     (message "dlv (%s), go-debug (%s)" dlv-version gdg-version))))

<<<<<<< HEAD
=======



>>>>>>> b850241... Added internal structure for store breakpoints
(defun call-dlv ()
  (interactive)
  (call-process-shell-command "dlv" nil t nil))

<<<<<<< HEAD
;;; Tests

(defmacro test-cleaner(&rest body)
  `(progn
     (gdg--remove-all-breakpoints)
     ,@body
     (gdg--remove-all-breakpoints)))


(ert-deftest test-set-breakpoint ()
  "Tests that add breakpoint is work"
  (test-cleaner
   (should (equal (length (gdg--set-breakpoint "main.go" 30)) 1))))

(ert-deftest test-remove-breakpoint ()
  "Tets that remove breakpoint is work"
  (test-cleaner
   (gdg--set-breakpoint "main.go" 30)
   (should (equal (length (gdg-remove-breakpoint "main.go" 30)) 0))))

(ert-deftest test-is-go-source ()
  ""
  (should (gdg--is-go-source-p "main.go"))
  (should (gdg--is-go-source-p "m.go"))
  (should (not (gdg--is-go-source-p ".go")))
  (should (not (gdg--is-go-source-p "")))
  (should (not (gdg--is-go-source-p nil))))

(ert-deftest test-check-remote-dlv-address-p ()
  (should (gdg--check-remote-dlv-addr-p "localhost:0"))
  (should (gdg--check-remote-dlv-addr-p "localhost:27000"))
  (should (gdg--check-remote-dlv-addr-p "192.168.243.20:27000"))
  (should (not (gdg--check-remote-dlv-addr-p "192.168.243.20")))
  (should (not (gdg--check-remote-dlv-addr-p ""))))

=======



>>>>>>> b850241... Added internal structure for store breakpoints
(provide 'gdg)
