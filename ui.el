;;; ui.el --- Provides function for visualisation.

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

;;;; Faces
(defface gdg--line-face
  '((t :inherit linum :foreground "red" :background "#444444" :weight bold))
  "Face for displaying current line."
  :group 'linum-relative)

;;;; Variables

(defvar gdg--last--cursor-pos 0
  "Store last position.")
   
(defvar gdg--user-format linum-format
  "Store the users linum-format")
  
;;;; Advices
(defadvice linum-update (before relative-linum-update activate)
  "This advice get the last position of linum."
  (setq gdg--last--cursor-pos (line-number-at-pos)))
  
(defun gdg-linum-update (line-number)
  "Customise linum drawing."
  (let ((symbol (number-to-string line-number))
        (face 'gdg--line-face)
        (current-pos linum-relative-last-pos))
    (if (= line-number current-pos)
        (propertize "=>" 'face face)
      (propertize (format "%3s" symbol 'face 'linum)))))

;;; Use:
;;; (setq linum-format 'line-custom)

