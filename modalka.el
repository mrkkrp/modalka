;;; modalka.el --- Easily introduce native modal editing of your own design -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/modalka
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: modal editing
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a building kit to help switch to modal editing in Emacs. Main
;; goal of the package is making modal editing in Emacs as natural and
;; native as possible. There is no hack, no corner cases, no emulation —
;; just start edit modally the way you want.

;;; Code:

(require 'cl-lib)

(defgroup modalka nil
  "Easily introduce native modal editing of your own design"
  :group  'editing
  :tag    "Modalka"
  :prefix "modalka-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/modalka"))

(defcustom modalka-cursor-type 'box
  "Cursor type to use in `modalka-mode'.

See description of `cursor-type' for mode information, this
variable should follow the same conventions."
  :tag "Cursor Type"
  :type '(or (const :tag "use the cursor specified for the frame" t)
             (const :tag "don't display a cursor"                 nil)
             (const :tag "display a filled box cursor"            box)
             (const :tag "display a hollow box cursor"            hollow)
             (const :tag "display a vertical bar cursor with default width"
                    bar)
             (cons  :tag "display a vertical bar cursor with given width"
                    (const bar) (integer :tag "width of cursor"))
             (const :tag "display a horizontal bar cursor with default height"
                    hbar)
             (cons  :tag "display a horizontal bar cursor with given height"
                    (const hbar (integer :tag "height of cursor")))))

(defvar modalka--translations nil
  "Alist consisting of translation key pairs.

Every pair is of this form:

  (ACTUAL-KEY TARGET-KEY)

This variable is used by `modalka--setup-translations'.  Please
don't edit this manually, use `modalka-set-key' and
`modalka-set-keys' instead.")

(defun modalka-set-key (actual-key target-key)
  "Register translation from ACTUAL-KEY to TARGET-KEY."
  (cl-pushnew (list actual-key target-key) modalka--translations
              :test #'equal))

(defun modalka-set-keys (&rest pairs)
  "Register many translations described by PAIRS.

Every pair should be of this form:

  (ACTUAL-KEY TARGET-KEY)"
  (dolist (args pairs)
    (apply #'modalka-set-key args)))

(defun modalka-remove-key (key)
  "Unregister translation from KEY.

Don't call when `modalka-mode' is enabled."
  (cl-delete key modalka--translations
             :key  #'car
             :test #'equal))

(defun modalka-remove-keys (&rest keys)
  "Unregister translation for KEYS.

Don't call when `modalka-mode' is enabled."
  (cl-delete-if (lambda (x) (member x keys))
                modalka--translations))

(defun modalka--setup-translations (enable)
  "Setup key translation for current buffer.

If ENABLE argument is non-NIL, activate Modalka translations and
remove them otherwise."
  (let ((m (copy-keymap key-translation-map)))
    (if (null enable)
        (setq-local key-translation-map
                    (default-value 'key-translation-map))
      (dolist (args modalka--translations)
        (apply #'define-key m args))
      (setq-local key-translation-map m))))

(define-minor-mode modalka-mode
  "Toggle `modalka-mode' minor mode.

With a prefix argument ARG, enable `modalka-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode setups translation of key bindings according to
configuration created previously with `modalka-set-key' and
`modalka-set-keys'."
  nil "↑" nil
  (modalka--setup-translations modalka-mode)
  (setq-local cursor-type
              (if modalka-mode
                  modalka-cursor-type
                (default-value 'cursor-type))))

(define-globalized-minor-mode modalka-global-mode
  modalka-mode modalka-mode)

(provide 'modalka)

;;; modalka.el ends here
