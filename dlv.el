;;; dlv.el --- Provides function for work with delve.

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

(eval-when-compile (require 'cl))

;;Full path to delve debugger
(defvar path-to-dlv "/home/warlock/workspace/bin/dlv")

;; Address to remote dlv server
(defvar gdg--dlv-host "localhost")

;; Port to remote dlv server
(defvar gdg--dlv-port 27000)


;;; Debugger

(defclass breakpoint ()
  ((file :initarg :file)
   (line :initarg :line))
  "Presents breakpoint instance")

(defclass debugger ()
  ((breakpoints :initarg :breakpoints :default nil)
   (pid :initarg :pid)
   (projectfile :initarg :projectfile))
  "Presents debugger instance")

;;; Macros

(defmacro dlv (&rest body)
  "This macros wrap all code to check if dlv is exists in system."
  (if (gdg--dlv-exists-p)
      `(progn ,@body)
    `(message "dlv is not found. Please check path-to-dlv (%s) variable." path-to-dlv)))

;;; Internal Function

(defun gdg--dlv-exists-p ()
  "Check that path to dlv is exists"
  (cond
   ((< 0 (length (shell-command-to-string "which dlv")))
    (setq path-to-dlv "dlv")
    t)
   ((file-exists-p path-to-dlv)
    t)))

(defun gdg--which-dlv ()
  "Search path to dlv on computer and print into new buffer."
  (let ((command (format "which %s" path-to-dlv)))
    (with-output-to-temp-buffer bufname
      (call-process-shell-command command nil bufname nil))))

(cl-defun gdg--make-debugger (&key attach connect debug test trace)
  "Run dlv with command and arguments, where arguments is value optional parameters."
  (cond
   ((not (null attach))
    (gdg--run-dlv "attach" attach))
   ((not (null connect))
    (gdg--run-dlv "connect" connect))
   ((not (null debug))
    (gdg--run-dlv "debug" debug))
   (t
    (message "Unknown command. Expected one from (attach, connect, debug)."))))

(defun gdg--run-dlv (command args)
  "Run dlv with other arguments."
  (dlv
   (shell-command-to-string (format "dlv %s %s" command args))))

(provide 'gdg-dlv)
