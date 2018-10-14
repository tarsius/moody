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

;; * Make sure that the face `mode-line' does not set `:box' and
;;   that `:underline' and `:overline' are the same color or are
;;   both `undefined'.  If defined, then the line color should be
;;   different from the `:background' colors of both `mode-line'
;;   and `default'.  The same rules apply to `mode-line-inactive'.
;;   The line colors of `mode-line' and `mode-line-inactive' do
;;   not necessarily have to be identical.  For example:
;;
;;     (use-package solarized-theme
;;       :config
;;       (load-theme 'solarized-light t)
;;       (let ((line (face-attribute 'mode-line :underline)))
;;         (set-face-attribute 'mode-line          nil :overline   line)
;;         (set-face-attribute 'mode-line-inactive nil :overline   line)
;;         (set-face-attribute 'mode-line-inactive nil :underline  line)
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
;;   quicker to try them out without having to add anything to your
;;   init file.

;; * To undo the call to a `moody-replace-*' function, call the same
;;   function with t as the value of the optional REVERSE argument.
;;   You can accomplish the same by interactively calling such a
;;   function with a prefix argument to do so.

;;; Code:

(require 'cl-lib)

;;; Options

(defcustom moody-mode-line-height
  (let ((font (face-font 'mode-line)))
    (if font (* 2 (aref (font-info font) 2)) 30))
  "When using `moody', height of the mode line in pixels.
This should be an even number."
  :type 'integer
  :group 'mode-line)

(defcustom moody-slant-function 'moody-slant
  "Function used to create tab slants."
  :type 'function
  :group 'mode-line)

;;; Core

(defun moody-replace-element (plain wrapped &optional reverse)
  "Replace PLAIN element with WRAPPED element in `mode-line-format'.

Replace every occurance of PLAIN is the complete tree.
If optional REVERSE is non-nil, then replace WRAPPED with PLAIN."
  (when reverse
    (cl-rotatef plain wrapped))
  (let ((format (cl-subst wrapped plain
                          (default-value 'mode-line-format)
                          :test #'equal)))
    (if (eq format (default-value 'mode-line-format))
        (message "Cannot find %s and use %s in its place" plain wrapped)
      (setq-default mode-line-format format))))

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
         (line  (if (eq line 'unspecified) outer line))
         (inner (if (eq type 'ribbon)
                    (face-attribute base :underline)
                  (face-attribute 'default :background)))
         (slant (if (eq direction 'down)
                    (list outer line inner)
                  (list inner line outer)))
         (face  (if (eq direction 'down)
                    (list :overline (and (eq type 'ribbon) line)
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
  (unless (cl-evenp height)
    (cl-incf height))
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

;;;###autoload
(defun moody-replace-mode-line-buffer-identification (&optional reverse)
  (interactive "P")
  (moody-replace-element 'mode-line-buffer-identification
                         'moody-mode-line-buffer-identification
                         reverse))

;;;; sml/mode-line-buffer-identification

(defvar sml/mode-line-buffer-identification) ; defined in `smart-mode-line.el'

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

;;;###autoload
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

;;;###autoload
(defun moody-replace-vc-mode (&optional reverse)
  (interactive "P")
  (moody-replace-element '(vc-mode vc-mode)
                         '(vc-mode moody-vc-mode)
                         reverse))

;;; Active Window

;; Inspired by, but not identical to, code in `powerline'.  Unlike
;; that, do not unset `moody--active-window' using `focus-out-hook'
;; because it is called when a non-Emacs window gains focus, but
;; Emacs still considers the previous Emacs window to be selected,
;; so we have to do the same.

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
      (setq moody--active-window win)
      (force-mode-line-update))))

(add-hook 'after-make-frame-functions       'moody--set-active-window)
(add-hook 'window-configuration-change-hook 'moody--set-active-window)
(add-hook 'focus-in-hook                    'moody--set-active-window)
(advice-add 'select-window :after           'moody--set-active-window)
(advice-add 'select-frame :after            'moody--set-active-window)
(advice-add 'delete-frame :after            'moody--set-active-window)

;;; Kludges

(advice-add 'resize-temp-buffer-window :before 'redisplay)

(declare-function color-srgb-to-xyz "color" (red green blue))
(declare-function color-rgb-to-hex "color" (red green blue &optional
                                                digits-per-component))

(defun moody-slant-apple-rgb (direction c1 c2 c3 &optional height)
  (require (quote color))
  (cl-flet ((cnv (color)
                 (pcase-let*
                     ((`(,r ,g ,b) (color-name-to-rgb color))
                      (`(,x ,y ,z) (color-srgb-to-xyz r g b))
                      (r (expt (+ (*  3.2404542 x)
                                  (* -1.5371385 y)
                                  (* -0.4985314 z))
                               (/ 1.8)))
                      (g (expt (+ (* -0.9692660 x)
                                  (*  1.8760108 y)
                                  (*  0.0415560 z))
                               (/ 1.8)))
                      (b (expt (+ (*  0.0556434 x)
                                  (* -0.2040259 y)
                                  (*  1.0572252 z))
                               (/ 1.8))))
                   (color-rgb-to-hex r g b))))
    (moody-slant direction (cnv c1) (cnv c2) (cnv c3) height)))

;;; _
(provide 'moody)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; moody.el ends here
