;;; modalka.el --- Easily introduce native modal editing of your own design -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–present Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/mrkkrp/modalka
;; Version: 0.1.5
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
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

;; This is a building kit to help switch to modal editing in Emacs.  The
;; main goal of the package is making modal editing in Emacs as natural and
;; native as possible.  There is no hack, no corner cases, no emulation—just
;; start edit modally the way you want.
;;
;; More information is available in the README.md file.

;;; Code:

(require 'cl-lib)
(require 'quail)

(defgroup modalka nil
  "Introduce native modal editing of your own design"
  :group  'editing
  :tag    "Modalka"
  :prefix "modalka-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/modalka"))

(defcustom modalka-cursor-type t
  "Cursor type to use in modalka mode.

See description of `cursor-type' for mode information, this
variable should follow the same conventions."
  :tag "Cursor Type"
  :type '(choice
          (const :tag "use the cursor specified for the frame" t)
          (const :tag "don't display a cursor" nil)
          (const :tag "display a filled box cursor" box)
          (const :tag "display a hollow box cursor" hollow)
          (const :tag "display a vertical bar cursor with default width" bar)
          (cons  :tag "display a vertical bar cursor with given width"
                 (const bar) (integer :tag "width of cursor"))
          (const :tag "display a horizontal bar cursor with default height" hbar)
          (cons  :tag "display a horizontal bar cursor with given height"
                 (const hbar (integer :tag "height of cursor")))))

;;;###autoload
(defcustom modalka-excluded-modes nil
  "List of major modes for which modalka mode should not be activated.

This variable is considered when Modalka is enabled globally via
function `modalka-global-mode'."
  :tag  "Excluded Modes"
  :type '(repeat :tag "Major modes to exclude" symbol))

(defcustom modalka-dynamic-lighter t
  "Show state name in modalka lighter."
  :tag  "Dynamic lighter"
  :type 'boolean)

(defcustom modalka-supress-global nil
  "Global bindings will be disabled in modalka mode."
  :tag  "Suppress global keymap."
  :type 'boolean)

(defvar modalka-mode-map (make-sparse-keymap)
  "This is Modalka mode map, used to translate your keys.")

(defvar modalka--supress-map (make-keymap)
  "Modalka suppress map.")
(suppress-keymap modalka--supress-map)
(when modalka-supress-global
  (set-keymap-parent modalka-mode-map modalka--supress-map))

(defvar modalka--states (list)
  "List of modalka states.  Default state is not in list.")

(defmacro modalka--maybe-create-map (suffix)
  "Create modalka state keymap with SUFFIX if does not exist."
  (setq modalka--states (delete-dups (append modalka--states `(,suffix))))
  (let ((name (intern (modalka--map-name suffix))))
    `(progn
       (unless (boundp (quote ,name))
	 (defvar ,name (make-sparse-keymap)
	   ,(format "This is Modalka mode map for state %s." suffix))
         (when modalka-supress-global
           (set-keymap-parent ,name modalka--supress-map)))
       (identity ,name))))

(defun modalka--map-name (suffix)
  "Get keymap name for given SUFFIX."
  (concat "modalka-mode-map--" suffix))

;;;###autoload
(defun modalka-define-key (actual-key target-key &optional state)
  "Register translation from ACTUAL-KEY to TARGET-KEY for STATE.

If STATE is omitted, default keymap will be used."
  (define-key
    (if state (eval `(modalka--maybe-create-map ,(symbol-name state))) modalka-mode-map)
    actual-key
    (defalias (make-symbol "modalka-translation")
      (lambda ()
        (interactive)
        (let ((binding (key-binding target-key)))
          (unless (or (memq binding '(nil undefined))
                      (keymapp binding))
            (call-interactively binding))))
      `(format "This command translates %s into %s, which calls `%s'."
               (key-description ,actual-key)
               (key-description ,target-key)
               (key-binding     ,target-key)))))

;;;###autoload
(defun modalka-define-kbd (actual-kbd target-kbd &optional state)
  "Register translation from ACTUAL-KBD to TARGET-KBD for STATE.

Arguments are accepted in the format used for saving keyboard
macros (see `edmacro-mode').
If STATE is omitted, default keymap will be used."
  (modalka-define-key (kbd actual-kbd) (kbd target-kbd) state))

;;;###autoload
(defun modalka-remove-key (key &optional state)
  "Unregister translation from KEY in STATE keymap for STATE.

If STATE is omitted, default keymap will be used."
  (let ((keymap (if state (eval `(modalka--maybe-create-map ,(symbol-name state))) modalka-mode-map)))
    (define-key keymap key nil)))

;;;###autoload
(defun modalka-remove-kbd (kbd &optional state)
  "Unregister translation from KBD for STATE.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode').
If STATE is omitted, default keymap will be used."
  (modalka-remove-key (kbd kbd) state))

;;;###autoload
(defun modalka-define-binding (key function &optional state)
  "Assign KEY to FUNCTION for STATE.

If STATE is omitted, default keymap will be used."
  (let ((keymap (if state (eval `(modalka--maybe-create-map ,(symbol-name state))) modalka-mode-map)))
    (define-key keymap key function)))

;;;###autoload
(defun modalka-define-binding-kbd (key function &optional state)
  "Assign KEY to FUNCTION for STATE.

Arguments are accepted in the format used for saving keyboard
macros (see `edmacro-mode').
If STATE is omitted, default keymap will be used."
  (modalka-define-binding (kbd key) function state))

;;;###autoload
(defun modalka-change-state (&optional state)
  "Switch STATE non-interactively.

For interactive calls use `modalka-switch-state`.
Set STATE to nil in order to enable default state."
  (let ((state (if state (if (symbolp state) (symbol-name state) state) state)))
    (setf (alist-get 'modalka-mode minor-mode-map-alist)
          (if state
              (eval `(modalka--maybe-create-map ,state))
            modalka-mode-map))
    (when modalka-dynamic-lighter
      (setcar (cdr (assq 'modalka-mode minor-mode-alist))
              (if state (format " ↑[%s]" state) " ↑")))))

;;;###autoload
(defun modalka-switch-state ()
  "Switch state interactively.

For non-interactive calls use `modalka-change-state`."
  (interactive)
  (let ((state (completing-read "State: " (append modalka--states '("*default*")))))
    (if (string= state "*default*")
	(modalka-change-state)
      (modalka-change-state state))))

;;;###autoload
(define-minor-mode modalka-mode
  "Toggle the `modalka-mode' minor mode.

With a prefix argument ARG, enable `modalka-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode setups translation of key bindings according to
configuration created previously with `modalka-define-key' and
`modalka-define-kbd'."
  nil " ↑" modalka-mode-map
  (setq-local cursor-type
              (if modalka-mode
                  modalka-cursor-type
                (default-value 'cursor-type))))

(defun modalka--maybe-activate ()
  "Activate modalka mode if current buffer is not minibuffer or blacklisted.

This is used by function `modalka-global-mode'."
  (unless (or (minibufferp)
              (member major-mode modalka-excluded-modes))
    (modalka-mode 1)))

;;;###autoload
(define-globalized-minor-mode modalka-global-mode
  modalka-mode
  modalka--maybe-activate)

(defun modalka--input-function-advice (fnc key)
  "Call FNC with KEY as argument only when modalka mode is disabled.

Otherwise use `list'."
  (funcall (if modalka-mode #'list fnc) key))

(advice-add 'quail-input-method :around #'modalka--input-function-advice)

(provide 'modalka)

;;; modalka.el ends here
