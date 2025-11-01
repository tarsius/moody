;;; moody.el --- Tabs and ribbons for the mode line  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.moody@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/moody
;; Keywords: faces

;; Package-Version: 1.2.1
;; Package-Requires: (
;;     (emacs  "28.1")
;;     (compat "30.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides utilities for displaying elements of the
;; mode line as tabs and ribbons.  It also provides replacements
;; for a few built-in elements.

;; The biggest differences to similar packages is that this one is
;; much simpler and much more consistent.  When using this package,
;; only the color of the mode line changes when a window becomes
;; in-/active.  Other packages additionally change what elements
;; are being displayed and also the appearance of an individual
;; element may change completely, which I found highly distracting
;; when trying out those packages, because I never knew what visual
;; clues to look for in order to find a certain piece of information.

;; Configuration
;; =============

;; Replacing default elements
;; --------------------------
;;
;; To style the buffer identification, displayed in the mode line, as
;; shown at https://github.com/tarsius/moody, add the following to your
;; init file.
;;
;;   (require 'moody)
;;   (moody-replace-mode-line-front-space)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)

;; Or if you are using `use-package'.
;;
;;   (use-package moody
;;     :config
;;     (moody-replace-mode-line-front-space)
;;     (moody-replace-mode-line-buffer-identification)
;;     (moody-replace-vc-mode))

;; Moody provides functions named `moody-replace-...', each of which
;; replaces a particular element with a styled variant.  These functions
;; can also be called interactively, in which case they toggle between
;; using the styled and vanilla variants of their respective element.

;; To learn what element substitutions are available out of the box, use
;; M-x moody-replace- TAB.

;; Styling
;; -------

;; Depending on the used theme, the faces `mode-line', `mode-line-active'
;; and `mode-line-inactive' might have to be modified when using Moody.

;; Let's go through some changes that are commonly required.  We will be
;; using `set-face-attribute' to achieve this.  The calls to that function
;; should be placed right after `load-theme'.

;; These examples assume Emacs 29.1 or later.  If you use an older
;; release, modify `mode-line' instead of `mode-line-active'.

;; Many themes (including the default theme) set the `:box' attribute
;; for these faces.  That conflicts with Moody, so you most likely have
;; to remove those boxes.
;;
;;   (set-face-attribute 'mode-line-active nil :box 'unspecified)
;;   (set-face-attribute 'mode-line-inactive nil :box 'unspecified)

;; A look similar to boxes can be achieved by using the `:overline' and
;; `:underline' attributes.
;;
;;   (set-face-attribute 'mode-line-active nil :overline "blue")
;;   (set-face-attribute 'mode-line-active nil
;;                       :underline `(:color "blue" :position t))
;;
;;   (set-face-attribute 'mode-line-inactive nil :overline "green")
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :underline `(:color "green" :position t))
;;
;; Beginning with Emacs 29.1, we can use :position t to put the
;; underline at the very bottom of the mode line.  When using an older
;; release, then this unfortunately can only be enabled globally.
;;
;;   (setq x-underline-at-descent-line t)

;;; Code:

(require 'cl-lib)
(require 'compat)

;;; Options

(defun moody-default-mode-line-height ()
  "Return two times size of font used by `mode-line' face.
If that is not possible (see code), return 30."
  (and (fboundp 'font-info)
       (let ((font (face-font 'mode-line)))
         (if font (* 2 (aref (font-info font) 2)) 30))))

(defcustom moody-mode-line-height (moody-default-mode-line-height)
  "When using `moody', height of the mode line in pixels.

This must be an even number or a function that, when called
with zero arguments, returns an even number.  Unless you plan
to change the sizes of fonts at runtime, it is better to use
a number here.  If you use a function instead, then that ends
up being called a lot.

Increasing the height of the mode-line triggers a bug in Emacs
releases before version 29.1, causing only parts of the buffer
to be displayed in the window even though it would fit exactly.
Moody provides a workaround but that in turn can result in some
flickering.  If you notice such flickering and it bothers you,
then either update to Emacs 29.1, or do not increase the height
of the mode-line."
  :type '(choice (integer :tag "constant value")
                 (function-item moody-default-mode-line-height)
                 (function-item window-mode-line-height)
                 function)
  :group 'mode-line)

(defcustom moody-display-scale 1
  "Factor by which to scale tab slant images.
Slant image height is increased by this factor, and the image
`:scale' is set to one over the factor.  This can be helpful
to improve smoothness for high-DPI systems like MacOS which use
\"logical\" pixels coarser than the physical display resolution
\(usually by 2x)."
  :type 'number
  :group 'mode-line)

(defcustom moody-slant-function 'moody-slant
  "Function used to create tab slants."
  :type 'function
  :group 'mode-line)

(defvar moody-slant-placeholder "|"
  "Placeholder string, which is replaced with the slant image.
Must be a non-empty string and is otherwise only relevant when
the image cannot be displayed.")

(defcustom moody-ribbon-background '(default :background)
  "Indirect specification of the background color used for ribbons.

This has the form (FACE ATTRIBUTE), and the color to be used
is determined using (face-attribute FACE ATTRIBUTE).  If FACE is
the special value `base', then, depending on whether the window
is active or not either `mode-line-active' or `mode-line-inactive'
is used (or if `moody-wrap's optional arguments FACE-ACTIVE and/or
FACE-INACTIVE are specified, then those faces).  `mode-line-active'
was added in Emacs 29.1, for older releases `mode-line' is used.

To get the color used until v0.6.0, then use (base :underline)."
  :type '(list (symbol  :tag "Face")
               (keyword :tag "Attribute"))
  :group 'mode-line)

;;; Core

(defun moody-replace-element (plain wrapped &optional reverse variable)
  "Replace PLAIN element with WRAPPED element in `mode-line-format'.

Replace every occurrence of PLAIN in the complete tree.
If optional REVERSE is non-nil, then replace WRAPPED with PLAIN.
If optional VARIABLE is non-nil, then the replacement happens in
the default value of that variable."
  (when reverse
    (cl-rotatef plain wrapped))
  (let ((format (cl-subst wrapped plain
                          (default-value 'mode-line-format)
                          :test #'equal)))
    (if (eq format (default-value (or variable 'mode-line-format)))
        (message "Cannot find %s and use %s in its place" plain wrapped)
      (set-default (or variable 'mode-line-format) format))))

(defun moody-format-find (elt &optional format)
  (named-let find
      ((elt elt)
       (tree (or format (default-value 'mode-line-format))))
    (cond ((eq tree elt) tree)
          ((consp tree)
           (or (find elt (car tree))
               (find elt (cdr tree)))))))

(defun moody-tab (string &optional width direction)
  "Return STRING as a tab.

STRING is padded with at least one space on either side.
If optional WIDTH is non-nil, then it has to be an integer
and specifies how much padding is added.

DIRECTION specifies the direction of the slant and defaults
to `up'.  The other valid value is `down'."
  (moody-wrap string width direction 'tab))

(defun moody-ribbon (string &optional width direction face-active face-inactive)
  "Return STRING as a ribbon.

STRING is padded with at least one space on either side.
If optional WIDTH is non-nil, then it has to be an integer
and specifies how much padding is added.

DIRECTION specifies the direction of the slant and defaults
to `up'.  The other valid value is `down'.

FACE-ACTIVE and FACE-INACTIVE specify the faces to be used when
the window is active respectively inactive.  If these faces are
not specified, then ad hoc faces based on `default', `mode-line',
`mode-line-active' and `mode-line-inactive' are used."
  (moody-wrap string width direction 'ribbon face-active face-inactive))

(defun moody-wrap (string &optional width direction type face-active face-inactive)
  (unless type
    (setq type 'tab))
  (unless direction
    (setq direction 'down))
  (let* ((base  (if (moody-window-active-p)
                    (or face-active
                        (if (>= emacs-major-version 29)
                            'mode-line-active
                          'mode-line))
                  (or face-inactive 'mode-line-inactive)))
         (outer (face-attribute base :background nil t))
         (line  (face-attribute base :underline nil t))
         (line  (cond ((and line (listp line))
                       (plist-get line :color))
                      ((eq line 'unspecified) outer)
                      ((or line outer))))
         (inner (if (eq type 'ribbon)
                    (pcase-let ((`(,face ,attribute) moody-ribbon-background))
                      (face-attribute (if (eq face 'base) base face)
                                      attribute nil t))
                  (face-attribute 'default :background nil t)))
         (slant (if (eq direction 'down)
                    (list outer line inner)
                  (list inner line outer)))
         (face  (list :overline  (and (or (eq direction 'up)
                                          (eq type 'ribbon))
                                      line)
                      :underline (and (or (eq direction 'down)
                                          (eq type 'ribbon))
                                      `( :color ,line
                                         ,@(and (>= emacs-major-version 29)
                                                '(:position t))))
                      :background inner))
         (pad   (max (- (or width 0) (length string)) 2)))
    (setq string
          (concat (make-string (ceiling pad 2) ?\s)
                  (substring string 0)
                  (make-string (floor pad 2) ?\s)))
    (add-face-text-property 0 (length string) face nil string)
    (list
     (propertize moody-slant-placeholder 'face face 'display
                 (apply moody-slant-function
                        (if (eq direction 'down) 'down 'up)
                        slant))
     string
     (propertize moody-slant-placeholder 'face face 'display
                 (apply moody-slant-function
                        (pcase (list type direction)
                          ('(tab    down) (cons 'up   slant))
                          ('(tab    up)   (cons 'down slant))
                          ('(ribbon down) (cons 'down (reverse slant)))
                          ('(ribbon up)   (cons 'up   (reverse slant)))))))))

(defvar moody--cache nil)

(defun moody-slant (direction c1 c2 c3 &optional height)
  (unless height
    (setq height (or (if (functionp moody-mode-line-height)
                         (funcall moody-mode-line-height)
                       moody-mode-line-height)
                     (window-mode-line-height))))
  (setq height (* height moody-display-scale))
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
                 'xpm t :scale (/ 1.0 moody-display-scale) :ascent 'center)))
          (push (cons key image) moody--cache)
          image))))

;;; Element Definitions
;;;; mode-line-buffer-identification

(defvar-local moody-mode-line-buffer-identification
  '(:eval (moody-tab (car (propertized-buffer-identification
                           (format-mode-line "%b")))
                     20 'down)))

(put 'moody-mode-line-buffer-identification 'risky-local-variable t)

;;;###autoload
(defun moody-replace-mode-line-buffer-identification (&optional restore)
  "Use moody's variant of `mode-line-buffer-identification'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants."
  (interactive (list (moody-format-find
                      'moody-mode-line-buffer-identification)))
  (moody-replace-element 'mode-line-buffer-identification
                         'moody-mode-line-buffer-identification
                         restore))

;;;; sml/mode-line-buffer-identification

(defvar sml/mode-line-buffer-identification) ; defined in `smart-mode-line.el'

(defvar-local moody-sml/mode-line-buffer-identification
  '(:eval (moody-tab
           (or sml/buffer-identification
               (sml/generate-buffer-identification)
               ;; Just in case the above are both nil.
               (car (propertized-buffer-identification
                     (format-mode-line "%b"))))
           20 'down)))

(put 'moody-sml/mode-line-buffer-identification 'risky-local-variable t)

(defvar moody--default-mode-line-buffer-identification
  mode-line-buffer-identification)

;;;###autoload
(defun moody-replace-sml/mode-line-buffer-identification (&optional restore)
  "Use moody's variant of `mode-line-buffer-identification'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants.

Use instead of `moody-replace-mode-line-buffer-identification'
if you use the `smart-mode-line' package, after `sml/setup' has
already been called."
  (interactive (list (moody-format-find
                      'moody-sml/mode-line-buffer-identification)))
  ;; Without this `sml/generate-buffer-identification' would always return nil.
  (setq-default mode-line-buffer-identification
                (if restore
                    moody--default-mode-line-buffer-identification
                  sml/mode-line-buffer-identification))
  (moody-replace-element 'mode-line-buffer-identification
                         'moody-sml/mode-line-buffer-identification
                         restore))

;;;; vc-mode

(defvar-local moody-vc-mode
  '(:eval (moody-ribbon (substring vc-mode 1) nil 'up)))

(put 'moody-vc-mode 'risky-local-variable t)

;;;###autoload
(defun moody-replace-vc-mode (&optional restore)
  "Use moody's variant of `vc-mode' mode-line element.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants."
  (interactive (list (moody-format-find 'moody-vc-mode)))
  (moody-replace-element '(vc-mode vc-mode)
                         '(vc-mode moody-vc-mode)
                         restore))

;;;; eldoc

(defvar moody-eldoc-minibuffer-message-function
  (lambda ()  ;; Only display in a mode-line right above minibuffer.
    (and (window-at-side-p nil 'bottom)
         ;; Side windows tend to be too narrow; so if there
         ;; are any, then display in all bottom mode-lines.
         (or (not (eq (window-main-window)
                      (frame-root-window)))
             (window-at-side-p nil 'left))
         (list " " (moody-tab eldoc-mode-line-string nil 'up)))))

(put 'moody-eldoc-minibuffer-message-function 'risky-local-variable t)

(defun moody-eldoc-minibuffer-message (format-string &rest args)
  "Display messages in the mode-line when in the minibuffer.

Otherwise work like `message'.

Use `moody-replace-eldoc-minibuffer-message-function' to use
this modified copy of `eldoc-minibuffer-message'.

Set `moody-eldoc-minibuffer-message-function' if you want to
change how the message is shown and/or in which mode-line(s)."
  (if (minibufferp)
      (progn
        (add-hook 'minibuffer-exit-hook
                  (lambda ()
                    (setq eldoc-mode-line-string nil)
                    ;; https://debbugs.gnu.org/16920
                    (setq eldoc-last-message nil))
                  nil t)
        (with-current-buffer
            (window-buffer
             (or (window-in-direction 'above (minibuffer-window))
                 (minibuffer-selected-window)
                 (get-largest-window)))
          (when mode-line-format
            ;; Undo eldoc-minibuffer-message's addition if necessary.
            (when (eq (ignore-errors (cadr (cadr (cadr mode-line-format))))
                      'eldoc-mode-line-string)
              (setq mode-line-format (car (cddr mode-line-format))))
            ;; Add our own variant, if it isn't present already.
            (unless (and (listp mode-line-format)
                         (assq 'eldoc-mode-line-string mode-line-format))
              (setq mode-line-format
                    (list ""
                          '(eldoc-mode-line-string
                            (:eval
                             (funcall moody-eldoc-minibuffer-message-function)))
                          mode-line-format))))
          (setq eldoc-mode-line-string
                (when (stringp format-string)
                  (apply #'format-message format-string args)))
          (force-mode-line-update)))
    (apply #'message format-string args)))

;;;###autoload
(defun moody-replace-eldoc-minibuffer-message-function (&optional restore)
  "Use moody's variant of `eldoc-minibuffer-message'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants."
  (interactive (list (or (moody-format-find
                          'moody-eldoc-minibuffer-message-function
                          mode-line-format)
                         (not (moody-format-find
                               'eldoc-minibuffer-message-function
                               mode-line-format)))))
  (if (not restore)
      (setq eldoc-message-function #'moody-eldoc-minibuffer-message)
    (setq eldoc-message-function #'eldoc-minibuffer-message)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq (ignore-errors (car (cadr mode-line-format)))
                  'eldoc-mode-line-string)
          (setq mode-line-format (car (cddr mode-line-format))))))))

;;;; mode-line-front-space

(defvar-local moody-mode-line-front-space
  '(:eval (if (display-graphic-p)
              (propertize " " 'display `((space :align-to 0)))
            "-")))

(put 'moody-mode-line-front-space 'risky-local-variable t)

;;;###autoload
(defun moody-replace-mode-line-front-space (&optional restore)
  "Use moody's variant of `mode-line-front-space'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants.

Adjust the display width so that subsequent character in the
mode-line are aligned with those in the buffer.  Unlike other
moody variants do not use any tab or ribbon."
  (interactive (list (moody-format-find 'moody-mode-line-front-space)))
  (moody-replace-element 'mode-line-front-space
                         'moody-mode-line-front-space
                         restore))

;;; Active Window

(defvar moody--active-window (selected-window))

(defun moody-window-active-p ()
  "Return t if the selected window is the active window.
Or put differently, return t if the possibly only temporarily
selected window is still going to be selected when we return
to the command loop."
  (eq (selected-window) moody--active-window))

(defun moody--set-active-window (_)
  (let ((win (selected-window)))
    (setq moody--active-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))
(add-hook 'pre-redisplay-functions #'moody--set-active-window)

;;; Kludges

(defun moody-redisplay (&optional _force &rest _ignored)
  "Call `redisplay' to trigger mode-line height calculations.

Certain functions, including e.g., `fit-window-to-buffer', base
their size calculations on values which are incorrect if the
mode-line has a height different from that of the `default' face
and certain other calculations have not yet taken place for the
window in question.

These calculations can be triggered by calling `redisplay'
explicitly at the appropriate time and this functions purpose
is to make it easier to do so.

This function is like `redisplay' with non-nil FORCE argument,
except that it only triggers redisplay when there is a non-nil
`mode-line-format' and the height of the mode-line is different
from that of the `default' face.  This function is intended to
be used as an advice to window creation functions."
  (when (and mode-line-format
             (/= (frame-char-height) (window-mode-line-height)))
    (redisplay t)))

(unless (>= emacs-major-version 29)
  (advice-add 'split-window :after #'moody-redisplay))

(declare-function color-srgb-to-xyz "color" (red green blue))
(declare-function color-rgb-to-hex "color" ( red green blue
                                             &optional digits-per-component))

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
