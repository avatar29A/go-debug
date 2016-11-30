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

;; Address to remote dlv server
(defvar gdg--dlv-host "localhost")

;; Port to remote dlv server
(defvar gdg--dlv-port 27000)

;;; Debugger

(defclass breakpoint ()
  ((name :initarg :name)
   (id :initarg :id)
   (file :initarg :file)
   (line :initarg :line))
  "Presents breakpoint instance")

(defclass debugger ()
  ((breakpoints :initarg :breakpoints :initform nil)
   (pid :initarg :pid)
   (process :initarg :process :initform nil)
   (process-buffer :initarg :process-buffer :initform nil)
   (projectfile :initarg :projectfile))
  "Presents debugger instance")

(defmethod break ((d debugger) position)
  "break (alias: b) ------------ Sets a breakpoint."
  (invoke d (format "b %s" position)))

(defmethod continue ((d debugger))
  "continue (alias: c) --------- Run until breakpoint or program termination."
  (invoke d (format "c")))

(defmethod clear ((d debugger) bp)
  "clear ----------------------- Deletes breakpoint."
  (invoke d
          (format "clear %s"
                  (cond ((slot-boundp d id)
                         (oref d id))
                        ((slot-boundp d name)
                         (oref d name))
                        (t
                         (error "%s" "Invalid breakpoint. Breakpoint have to has id or name."))))))

(defmethod stop ((d debugger))
  (when (not (null (oref d process)))
    (delete-process (oref d process))))

(defmethod invoke ((d debugger) command)
  (when (not (null (oref d process)))
    (process-send-string (oref d process) command)))

;;; Macros

(defmacro dlv (&rest body)
  "This macros wrap all code to check if dlv is exists in system."
  (if (gdg--dlv-exists-p)
      `(progn ,@body)
    `(message "dlv is not found. Please check path-to-dlv (%s) variable." path-to-dlv)))

;;; Internal Function

(defun gdg--dlv-exists-p ()
  "Check that path to dlv is exists"
  (< 0 (length (shell-command-to-string "which dlv"))))

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
    (dlv
     (let* ((pbuffer
             (get-buffer-create
              (format "dlv [%s]" debug)))
            (p
             (start-process-shell-command pbuffer
                                          (fomrat "dlv debug %s" debug))))
       (make-instance 'debugger :process p :process-buffer pbuffer :projectfile debug))))
   (t
    (message "Unknown command. Expected one from (attach, connect, debug)."))))

(defun gdg--run-dlv (command args)
  "Run dlv with other arguments."
  (dlv
   (shell-command-to-string (format "dlv %s %s" command args))))

(provide 'gdg-dlv)
