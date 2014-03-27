;;; fancy-narrow.el --- narrow-to-region with more eye candy.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/fancy-narrow-region
;; Version: 0.7
;; Keywords: faces convenience
;; Prefix: fancy-narrow
;; Separator: -

;;; Commentary:
;; 
;; fancy-narrow
;; ============
;; 
;; Emacs package to immitate `narrow-to-region' with more eye-candy.
;; 
;; Unlike `narrow-to-region', which completely hides text outside
;; the narrowed region, this package simply deemphasizes the text,
;; makes it readonly, and makes it unreachable.
;; 
;; This leads to a much more natural feeling, where the region stays
;; static (instead of being brutally moved to a blank slate) and is
;; clearly highlighted with respect to the rest of the buffer.
;; 
;; Simply call `fancy-narrow-to-region' to see it in action. To widen the
;; region again afterwards use `fancy-widen'.
;; 
;; To customise the face used to deemphasize unreachable text, customise
;; `fancy-narrow-blocked-face'. There is a known bug at the moment, which
;; is that comments and strings don't deemphasize correctly.
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
;; 0.6  - 2014/03/26 - Successive narrowing results in intersection of previous and new regions.
;; 0.6  - 2014/03/26 - Flycheck protection.
;; 0.5  - 2014/03/25 - define-minor-mode.
;; 0.2a - 2014/03/25 - Stickiness, better motion, and font-lock.
;; 0.1a - 2014/03/17 - Created File.
;;; Code:

(defconst fancy-narrow-version "0.7" "Version of the fancy-narrow-region.el package.")
(defun fancy-narrow-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and fancy-narrow-region versions."
  (interactive)
  (message "Your fancy-narrow-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           fancy-narrow-version emacs-version)
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

(defvar fancy-narrow--wasnt-font-lock nil "")
(make-variable-buffer-local 'fancy-narrow--wasnt-font-lock)
(defvar fancy-narrow--was-flyspell nil "")
(make-variable-buffer-local 'fancy-narrow--was-flyspell)

(defvar fancy-narrow-properties-stickiness
  '(front-sticky t rear-nonsticky t) "")

;;;###autoload
(defun fancy-narrow-to-region (start end)
  "Like `narrow-to-region', except it still displays the unreachable text.

Unlike `narrow-to-region', which completely hides text outside
the narrowed region, this function simply deemphasizes the text,
makes it readonly, and makes it unreachable.

This leads to a much more natural feeling, where the region stays
static (instead of moving up to hide the text above) and is
clearly highlighted with respect to the rest of the buffer.

There is a known bug at the moment, which is that comments and
strings don't deemphasize correctly.

To widen the region again afterwards use `fancy-widen'."
  (interactive "r")
  (let ((l (min start end))
        (r (max start end)))
    ;; If it was already active, just become narrower.
    (when fancy-narrow--beginning (setq l (max l fancy-narrow--beginning)))
    (when fancy-narrow--end (setq r (max r fancy-narrow--end)))
    ;; unless it was already active, patch font-lock and flyspell
    (unless (and fancy-narrow--beginning fancy-narrow--end)
      (unless font-lock-mode
        (setq fancy-narrow--wasnt-font-lock t)
        (font-lock-mode 1))
      (when flyspell-mode
        (setq fancy-narrow--was-flyspell t)
        (flyspell-mode 0)))
    (setq fancy-narrow--beginning (copy-marker l nil)
          fancy-narrow--end (copy-marker r t))
    (add-hook 'post-command-hook 'fancy-narrow--motion-function t t)
    (add-text-properties (point-min) l fancy-narrow-properties-stickiness)
    (add-text-properties (point-min) l fancy-narrow-properties)
    (add-text-properties r (point-max) fancy-narrow-properties)))

;;;###autoload
(defun fancy-widen ()
  "Undo narrowing from `fancy-narrow-to-region'."
  (interactive)
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t))
    (when fancy-narrow--wasnt-font-lock
      (setq fancy-narrow--wasnt-font-lock nil)
      (font-lock-mode -1))
    (when fancy-narrow--was-flyspell
      (setq fancy-narrow--was-flyspell nil)
      (flyspell-mode 1))
    (setq fancy-narrow--beginning nil
          fancy-narrow--end nil)
    (remove-hook 'post-command-hook 'fancy-narrow--motion-function t)
    (remove-text-properties (point-min) (point-max) fancy-narrow-properties)
    (remove-text-properties (point-min) (point-max) fancy-narrow-properties-stickiness)))

(defcustom fancy-narrow-lighter " *"
  "Lighter used in the mode-line while the mode is active."
  :type 'string
  :group 'fancy-narrow
  :package-version '(fancy-narrow . "0.5"))

;;;###autoload
(define-minor-mode fancy-narrow-mode 
  "Global minor mode that binds the fancy-narrow functions.

The keys used are the same used by the non-fancy functions.
Binds that are replaced are:
   widen
   narrow-to-region
   narrow-to-defun
   narrow-to-page
   org-narrow-to-block
   org-narrow-to-element
   org-narrow-to-subtree"
  t fancy-narrow-lighter
  '(("nb" . org-fancy-narrow-to-block)
   ("nd" . fancy-narrow-to-defun)
   ("ne" . org-fancy-narrow-to-element)
   ("nn" . fancy-narrow-to-region)
   ("np" . fancy-narrow-to-page)
   ("ns" . org-fancy-narrow-to-subtree)
   ("nw" . fancy-widen))
  :global t
  :group 'fancy-narrow)

(defface fancy-narrow-blocked-face
  '((((background light)) :foreground "Grey70")
    (((background dark)) :foreground "Grey30"))
  "Face used on blocked text."
  :group 'fancy-narrow-region)

;;; ---------------------------------------
;;; COPIED FUNCTIONS:
;;; The following functions are taken directly from their non-fancy
;;; counterparts. I did not write them.
;;;###autoload
(defun org-fancy-narrow-to-block ()
  "Like `org-narrow-to-block', except using `fancy-narrow-to-region'."
  (interactive)
  (let* ((case-fold-search t)
         (blockp (org-between-regexps-p "^[ \t]*#\\+begin_.*"
                                        "^[ \t]*#\\+end_.*")))
    (if blockp
        (fancy-narrow-to-region (car blockp) (cdr blockp))
      (user-error "Not in a block"))))
;;;###autoload
(defun fancy-narrow-to-defun (&optional _arg)
  "Like `narrow-to-defun', except using `fancy-narrow-to-region'."
  (interactive)
  (save-excursion
    (widen)
    (let ((opoint (point))
          beg end)
      (let ((here (point)))
        (unless (eolp)
          (forward-char))
        (beginning-of-defun)
        (when (< (point) here)
          (goto-char here)
          (beginning-of-defun)))
      (setq beg (point))
      (end-of-defun)
      (setq end (point))
      (while (looking-at "^\n")
        (forward-line 1))
      (unless (> (point) opoint)
        ;; beginning-of-defun moved back one defun
        ;; so we got the wrong one.
        (goto-char opoint)
        (end-of-defun)
        (setq end (point))
        (beginning-of-defun)
        (setq beg (point)))
      (goto-char end)
      (re-search-backward "^\n" (- (point) 1) t)
      (fancy-narrow-to-region beg end))))
;;;###autoload
(defun org-fancy-narrow-to-element ()
  "Like `org-narrow-to-element', except using `fancy-narrow-to-region'."
  (interactive)
  (let ((elem (org-element-at-point)))
    (cond
     ((eq (car elem) 'headline)
      (fancy-narrow-to-region
       (org-element-property :begin elem)
       (org-element-property :end elem)))
     ((memq (car elem) org-element-greater-elements)
      (fancy-narrow-to-region
       (org-element-property :contents-begin elem)
       (org-element-property :contents-end elem)))
     (t
      (fancy-narrow-to-region
       (org-element-property :begin elem)
       (org-element-property :end elem))))))
;;;###autoload
(defun fancy-narrow-to-page (&optional arg)
  "Like `narrow-to-page', except using `fancy-narrow-to-region'."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (widen)
    (if (> arg 0)
        (forward-page arg)
      (if (< arg 0)
          (let ((adjust 0)
                (opoint (point)))
            ;; If we are not now at the beginning of a page,
            ;; move back one extra time, to get to the start of this page.
            (save-excursion
              (beginning-of-line)
              (or (and (looking-at page-delimiter)
                       (eq (match-end 0) opoint))
                  (setq adjust 1)))
            (forward-page (- arg adjust)))))
    ;; Find the end of the page.
    (set-match-data nil)
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction
    ;; at the beginning of that line.
    ;; Before checking the match that was found,
    ;; verify that forward-page actually set the match data.
    (if (and (match-beginning 0)
             (save-excursion
               (goto-char (match-beginning 0)) ; was (beginning-of-line)
               (looking-at page-delimiter)))
        (goto-char (match-beginning 0))) ; was (beginning-of-line)
    (fancy-narrow-to-region (point)
                            (progn
                              ;; Find the top of the page.
                              (forward-page -1)
                              ;; If we found beginning of buffer, stay there.
                              ;; If extra text follows page delimiter on same line,
                              ;; include it.
                              ;; Otherwise, show text starting with following line.
                              (if (and (eolp) (not (bobp)))
                                  (forward-line 1))
                              (point)))))
;;;###autoload
(defun org-fancy-narrow-to-subtree ()
  "Like `org-narrow-to-subtree', except using `fancy-narrow-to-region'."
  (interactive)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
       (fancy-narrow-to-region
        (progn (org-back-to-heading t) (point))
        (progn (org-end-of-subtree t t)
               (if (and (org-at-heading-p) (not (eobp))) (backward-char 1))
               (point)))))))
;;; ---------------------------------------

(provide 'fancy-narrow)
;;; fancy-narrow.el ends here.

