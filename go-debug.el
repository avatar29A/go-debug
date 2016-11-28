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
(require 'gdg-ui)

(load-file "")

;;; Variables

;;Full path to delve debugger
(defvar path-to-dlv "/home/warlock/workspace/bin/dlv")

;; Current package version
(defvar gdg-version "0.1.0")

;; Address to remote dlv server
(defvar gdg--dlv-host "localhost")

;; Port to remote dlv server
(defvar gdg--dlv-port 27000)

;; Delve process' PID
(defvar gdg--dlv-pid nil)

;; Active breakpoints
(defvar gdg--breakpoints ())

;;; Internal Functions & Macros

(defun gdg--dlv-exists-p ()
  "Check that path to dlv is exists"
  (cond
   ((< 0 (length (shell-command-to-string "which dlv")))
    (setq path-to-dlv "dlv")
    t)
   ((file-exists-p path-to-dlv)
    t)))

(defmacro dlv (&rest body)
  "This macros wrap all code to check if dlv is exists in system."
  (if (gdg--dlv-exists-p)
      `(progn ,@body)
    `(message "dlv is not found. Please check path-to-dlv (%s) variable." path-to-dlv)))

(defun gdg--which-dlv ()
  "Search path to dlv on computer and print into new buffer."
  (interactive)
  (let ((command (format "which %s" path-to-dlv)))
    (with-output-to-temp-buffer bufname
      (call-process-shell-command command nil bufname nil))))

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

(defun gdg-start (path-to-package)
  "Start dlv as headless instance. Wait to connect to localhost:0")

(defun gdg-debug (path)
  "Compile and begin debugging main package in current directory, or the package specified.")

(defun gdg-attach (pid)
  "Attach to running process and begin debugging")

(defun gdg-connect (host port)
  "Connect to a headless debug server")

(defun gdg-test (path-to-tests)
  "Compile test binary and begin debugging program.")

(defun gdg-trace (path-to-package)
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




(defun call-dlv ()
  (interactive)
  (call-process-shell-command "dlv" nil t nil))




(provide 'gdg)
