;;; moody.el --- Tabs and ribbons for the mode line  -*- lexical-binding: t -*-

;; Copyright (C) 2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/moody

;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides utilities for displaying elements of the
;; mode line as tabs and ribbons.  It also provides replacements
;; for a few built-in elements.

;; The biggest differences to similar packages is that this one is
;; much simpler and much more consistent.  When using this package,
;; then only the color of the mode line changes when a window
;; becomes in-/active.  Other packages additionally change what
;; elements are being displayed and also the appearance of an
;; individual element may change completely, which I found highly
;; distracting when trying out those packages because I never know
;; what visual clues to look for in order to find a certain piece
;; of information.

;; Usage:

;; * Make sure that the face `mode-line' does not set `:box' and that
;;   it sets `:underline' and `:overline' to the same color.  That
;;   color should be different from the `:background' colors of both
;;   `mode-line' and `default'.  Do the same for `mode-line-inactive'.
;;   The line colors of `mode-line' and `mode-line-inactive' do not
;;   have to be identical.  For example:
;;
;;     (use-package solarized-theme
;;       :config
;;       (load-theme 'solarized-light t)
;;       (let ((line (face-attribute 'mode-line :underline)))
;;         (set-face-attribute 'mode-line-inactive nil :overline   line)
;;         (set-face-attribute 'mode-line          nil :overline   line)
;;         (set-face-attribute 'mode-line          nil :underline  line)
;;         (set-face-attribute 'mode-line          nil :box        nil)
;;         (set-face-attribute 'mode-line-inactive nil :box        nil)
;;         (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; * Add something like this to your init file:
;;
;;     (use-package moody
;;       :config
;;       (setq x-underline-at-descent-line t)
;;       (moody-replace-mode-line-buffer-identification)
;;       (moody-replace-vc-mode))

;; * Such replacement functions are defines as commands, making it
;;   quicker to try them out.

;; * To undo a replacement use the optional REVERSE argument of the
;;   replacement function.  When calling it interactively, then use
;;   a prefix argument to do so.

;;; Code:

;;; Options

(defcustom moody-mode-line-height 30
  "When using `moody', height of the mode line in pixels.
This should be an even number."
  :type 'integer
  :group 'mode-line)

(defcustom moody-slant-function 'moody-slant
  "Function used to create tab slants."
  :type 'function
  :group 'mode-line)

;;; Core

(defun moody-deep-replace (from to lst)
  "Replace FROM with TO wherever it appears in list-of-lists LST.
Comparison is done with `equal'."
  (cond ((equal from lst)
         to)
        ((listp lst)
         (mapcar (lambda (sub) (deep-replace from to sub)) lst))
        (t
         lst)))

(defun moody-replace-element (plain wrapped &optional reverse)
  "Replace PLAIN element with WRAPPED element in `mode-line-format'.
If optional REVERSE is non-nil, then replace WRAPPED with PLAIN."
  (when reverse
    (cl-rotatef plain wrapped))
  (let ((replaced (moody-deep-replace plain wrapped mode-line-format)))
    (cond ((equal replaced mode-line-format)
           (message "Cannot find %s and use %s in its place" plain wrapped))
          (t
           (setq mode-line-format replaced)))))

(defun moody-tab (string &optional width direction)
  "Return STRING as a tab.

STRING is padded with at least one space on either side.  If
optional WIDTH is non-nil, then it has to be an integer and
specifies how much padding is added.

DIRECTION specifies the direction of the slant and defaults
to `up'.  The other valid value is `down'."
  (moody-wrap string width direction 'tab))

(defun moody-ribbon (string &optional width direction face-active face-inactive)
  "Return STRING as a ribbon.

STRING is padded with at least one space on either side.  If
optional WIDTH is non-nil, then it has to be an integer and
specifies how much padding is added.

DIRECTION specifies the direction of the slant and defaults
to `up'.  The other valid value is `down'.

FACE-ACTIVE and FACE-INACTIVE specify the faces to be used when
the window is active respectively inactive.  If these faces are
not specified, then faces based on `default', `mode-line' and
`mode-line-active' are generated and used."
  (moody-wrap string width direction 'ribbon face-active face-inactive))

(defun moody-wrap (string &optional width direction type face-active face-inactive)
  (unless type
    (setq type 'tab))
  (unless direction
    (setq direction 'down))
  (let* ((base  (if (moody-window-active-p)
                    (or face-active 'mode-line)
                  (or face-inactive 'mode-line-inactive)))
         (outer (face-attribute base :background))
         (line  (face-attribute base :underline))
         (inner (if (eq type 'ribbon)
                    (face-attribute base :underline)
                  (face-attribute 'default :background)))
         (slant (if (eq direction 'down)
                    (list outer line inner)
                  (list inner line outer)))
         (face  (if (eq direction 'down)
                    (list :overline nil
                          :underline line
                          :background inner)
                  (list :overline line
                        :underline (and (or (eq type 'ribbon)
                                            (not (window-at-side-p nil 'bottom)))
                                        line)
                        :background inner)))
         (pad   (max (- (or width 0) (length string)) 2)))
    (setq string
          (concat (make-string (ceiling pad 2) ?\s)
                  (substring string 0)
                  (make-string (floor pad 2) ?\s)))
    (add-face-text-property 0 (length string) face nil string)
    (list
     (propertize " " 'face face 'display
                 (apply moody-slant-function
                        (if (eq direction 'down) 'down 'up)
                        slant))
     string
     (propertize " " 'face face 'display
                 (apply moody-slant-function
                        (pcase (list type direction)
                          (`(tab    down) (cons 'up   slant))
                          (`(tab    up)   (cons 'down slant))
                          (`(ribbon down) (cons 'down (reverse slant)))
                          (`(ribbon up)   (cons 'up   (reverse slant)))))))))

(defvar moody--cache nil)

(defun moody-slant (direction c1 c2 c3 &optional height)
  (unless height
    (setq height moody-mode-line-height))
  (let ((key (list direction c1 c2 c3 height)))
    (or (cdr (assoc key moody--cache))
        (let* ((width (/ height 2))
               (image
                (create-image
                 (format "/* XPM */ static char * image[] = {
 \"%s %s 3 1\",\n \"0 c %s\",\n \"1 c %s\",\n \"2 c %s\",%s\n};"
                         width height c1 c2 c3
                         (cl-loop
                          for i from 1 to height concat
                          (format " \"%s\",\n"
                                  (let* ((x (/ i 2))
                                         (a (make-string x ?0))
                                         (b (make-string 1 ?1))
                                         (c (make-string
                                             (max 0 (- width x)) ?2)))
                                    (if (eq direction 'down)
                                        (concat a b c)
                                      (concat c b a))))))
                 'xpm t :ascent 'center)))
          (push (cons key image) moody--cache)
          image))))

;;; Element Definitions
;;;; mode-line-buffer-identification

(defvar moody-mode-line-buffer-identification
  '(:eval (moody-tab (format-mode-line (propertized-buffer-identification "%b"))
                     20 'down)))
(put 'moody-mode-line-buffer-identification 'risky-local-variable t)
(make-variable-buffer-local 'moody-mode-line-buffer-identification)

(defun moody-replace-mode-line-buffer-identification (&optional reverse)
  (interactive "P")
  (moody-replace-element 'mode-line-buffer-identification
                         'moody-mode-line-buffer-identification
                         reverse))

;;;; sml/mode-line-buffer-identification

(defvar sml/mode-line-buffer-identification) ; define in `smart-mode-line.el'

(defvar moody-sml/mode-line-buffer-identification
  '(:eval (moody-tab
           (or sml/buffer-identification
               (sml/generate-buffer-identification)
               ;; Just in case the above are both nil.
               (format-mode-line (propertized-buffer-identification "%b")))
           20 'down)))
(put 'moody-sml/mode-line-buffer-identification 'risky-local-variable t)
(make-variable-buffer-local 'moody-sml/mode-line-buffer-identification)

(defvar moody--default-mode-line-buffer-identification
  mode-line-buffer-identification)

(defun moody-replace-sml/mode-line-buffer-identification (&optional reverse)
  (interactive "P")
  ;; Without this `sml/generate-buffer-identification' would always return nil.
  (setq-default mode-line-buffer-identification
                (if reverse
                    moody--default-mode-line-buffer-identification
                  sml/mode-line-buffer-identification))
  (moody-replace-element 'mode-line-buffer-identification
                         'moody-sml/mode-line-buffer-identification
                         reverse))

;;;; vc-mode

(defvar moody-vc-mode
  ;;'(:eval (moody-ribbon (substring vc-mode 1) nil 'up))
  '(:eval (moody-tab (substring vc-mode 1) nil 'up)))
(put 'moody-vc-mode 'risky-local-variable t)
(make-variable-buffer-local 'moody-vc-mode)

(defun moody-replace-vc-mode (&optional reverse)
  (interactive "P")
  (moody-replace-element '(vc-mode vc-mode)
                         '(vc-mode moody-vc-mode)
                         reverse))

;;; Active Window
;;
;; Inspired by, but not identical to, code in `powerline'.
;; In particular we don't add anything to `focus-out-hook'
;; because that turned out to be counterproductive.

(defvar moody--active-window (frame-selected-window))

(defun moody-window-active-p ()
  "Return t if the selected window is the active window.
Or put differently, return t if the possibly only temporarily
selected window is still going to be selected when we return
to the command loop."
  (eq (selected-window) moody--active-window))

(defun moody--set-active-window (&rest _)
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq moody--active-window win))))

(add-hook 'window-configuration-change-hook 'moody--set-active-window)
(add-hook 'focus-in-hook                    'moody--set-active-window)
(advice-add 'handle-switch-frame :after     'moody--set-active-window)
(advice-add 'select-window :after           'moody--set-active-window)

;;; _
(provide 'moody)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; moody.el ends here
