;;; fancy-narrow.el --- narrow-to-region with more eye candy.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/fancy-narrow-region
;; Version: 0.1a
;; Keywords: faces convenience
;; Prefix: fancy-narrow
;; Separator: -

;;; Commentary:
;; 
;; fancy-narrow is a package that immitates narrow-to-region with more
;; eye-candy. Instead of completely hiding text beyond the narrowed
;; region, the text is de-emphasized and becomes unreachable.
;; 
;; Simply call `fancy-narrow-to-region' to see it in action. Remember to
;; `fancy-widen' afterwards.
;; 
;; To change the face used on the blocked text, customise `fancy-narrow-blocked-face'.
;; 
;; Note this is designed for user interaction. For using within lisp code,
;; the standard `narrow-to-region' is preferable, because the fancy
;; version is susceptible to `inhibit-read-only' and some corner cases.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 0.1a - 2014/03/17 -  - Created File.
;;; Code:

(defconst fancy-narrow-version "0.1a" "Version of the fancy-narrow-region.el package.")
(defun fancy-narrow-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and fancy-narrow-region versions."
  (interactive)
  (message "Your fancy-narrow-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           fancy-narrow-region-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/fancy-narrow/issues/new"))
(defgroup fancy-narrow nil
  "Customization group for fancy-narrow."
  :prefix "fancy-narrow-")

(defconst fancy-narrow--help-string
  "This region is blocked from editing while buffer is narrowed."
  "Help-echo string displayed on mouse-over.")

(defcustom fancy-narrow-properties
  '(intangible t read-only t
               fontified nil
               font-lock-face fancy-narrow-blocked-face
               help-echo fancy-narrow--help-string
               fancy-narrow-this-text-will-be-deleted t)
  "List of properties given to text beyond the narrowed region."
  :type 'list
  :group 'fancy-narrow-region)

(defvar fancy-narrow--beginning nil "")
(make-variable-buffer-local 'fancy-narrow--beginning)
(defvar fancy-narrow--end nil "")
(make-variable-buffer-local 'fancy-narrow--end)

(defun fancy-narrow--motion-function (&rest ignore)
  "Keep point from going past the boundaries."
  ;; (message "%s:%s" x (p )
  (let ((inhibit-point-motion-hooks t))
    (if (< (point) fancy-narrow--beginning)
        (goto-char fancy-narrow--beginning)
      (if (> (point) fancy-narrow--end)
          (goto-char fancy-narrow--end)))))

(defvar fancy-narrow--was-font-lock nil "")
(make-variable-buffer-local 'fancy-narrow--was-font-lock)

(defvar fancy-narrow-properties-stickiness
  '(front-sticky t rear-nonsticky t) "")

;;;###autoload
(defun fancy-narrow-to-region (start end)
  "Like `narrow-to-region', except it still displays the unreachable text."
  (interactive "r")
  (let ((l (min start end))
        (r (max start end)))
    (unless font-lock-mode
      (setq fancy-narrow--was-font-lock t)
      (font-lock-mode 1))
    (setq fancy-narrow--beginning (copy-marker l nil)
          fancy-narrow--end (copy-marker r t))
    (add-hook 'post-command-hook 'fancy-narrow--motion-function t t)
    (add-text-properties (point-min) l fancy-narrow-properties-stickiness)
    (add-text-properties (point-min) l fancy-narrow-properties)
    (add-text-properties r (point-max) fancy-narrow-properties)))

(defun fancy-widen ()
  "Undo narrowing from `fancy-narrow-to-region'."
  (interactive)
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t))
    (when fancy-narrow--was-font-lock
      (setq fancy-narrow--was-font-lock nil)
      (font-lock-mode -1))
    (setq fancy-narrow--beginning nil
          fancy-narrow--end nil)
    (remove-hook 'post-command-hook 'fancy-narrow--motion-function t)
    (remove-text-properties (point-min) (point-max) fancy-narrow-properties)
    (remove-text-properties (point-min) (point-max) fancy-narrow-properties-stickiness)))

(defface fancy-narrow-blocked-face
  '((((background light)) :foreground "Grey70")
    (((background dark)) :foreground "Grey30"))
  "Face used on blocked text."
  :group 'fancy-narrow-region)

(provide 'fancy-narrow)
;;; fancy-narrow.el ends here.
