;;; centaur-tabs.el --- Aesthetic, modern looking customizable tabs plugin  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Emmanuel Bustos

;; Filename: centaur-tabs.el
;; Description: Provide an out of box configuration to use highly customizable tabs.
;; URL: https://github.com/ema2159/centaur-tabs
;; Author: Emmanuel Bustos <ema2159@gmail.com>
;; Maintainer: Emmanuel Bustos <ema2159@gmail.com>
;; Created: 2019-21-19 22:14:34
;; Version: 5
;; Known Compatibility: GNU Emacs 26.2
;; Package-Requires: ((emacs "24.4") (powerline "2.4")  (cl-lib "0.5"))
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Emacs plugin aiming to become an aesthetic, modern looking tabs plugin.
;;

;;; Code:
;;; Requires
(require 'centaur-tabs-elements)
(require 'centaur-tabs-functions)
(require 'centaur-tabs-interactive)
;;;;;;;;;;;;;;;;;;;;;;; Centaur-Tabs source code ;;;;;;;;;;;;;;;;;;;;;;;

(defgroup centaur-tabs nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defvar centaur-tabs--buffer-show-groups nil)

;;; Minor modes
;;
(defsubst centaur-tabs-mode-on-p ()
  "Return non-nil if Centaur-Tabs mode is on."
  (eq (default-value 'header-line-format)
      centaur-tabs-header-line-format))

;;; Centaur-Tabs-Local mode
;;
(defvar centaur-tabs--local-hlf nil)

;;;###autoload
(define-minor-mode centaur-tabs-local-mode
  "Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Centaur-Tabs mode is off."
  :group 'centaur-tabs
  :global nil
  (unless (centaur-tabs-mode-on-p)
    (error "Centaur-Tabs mode must be enabled"))
;;; ON
  (if centaur-tabs-local-mode
      (if (and (local-variable-p 'header-line-format)
	       header-line-format)
	  ;; A local header line exists, hide it to show the tab bar.
	  (progn
	    ;; Fail in case of an inconsistency because another local
	    ;; header line is already hidden.
	    (when (local-variable-p 'centaur-tabs--local-hlf)
	      (error "Another local header line is already hidden"))
	    (set (make-local-variable 'centaur-tabs--local-hlf)
		 header-line-format)
	    (kill-local-variable 'header-line-format))
	;; Otherwise hide the tab bar in this buffer.
	(setq header-line-format nil))
;;; OFF
    (if (local-variable-p 'centaur-tabs--local-hlf)
	;; A local header line is hidden, show it again.
	(progn
	  (setq header-line-format centaur-tabs--local-hlf)
	  (kill-local-variable 'centaur-tabs--local-hlf))
      ;; The tab bar is locally hidden, show it again.
      (kill-local-variable 'header-line-format))))

;;; Centaur-Tabs mode
;;
(defvar centaur-tabs-prefix-key [(control ?c)]
  "The common prefix key used in Centaur-Tabs mode.")

(defvar centaur-tabs-prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km [(control left)]  'centaur-tabs-backward)
    (define-key km [(control right)] 'centaur-tabs-forward)
    (define-key km [(control up)]    'centaur-tabs-backward-group)
    (define-key km [(control down)]  'centaur-tabs-forward-group)
    (define-key km [(control f10)]   'centaur-tabs-local-mode)
    km)
  "The key bindings provided in Centaur-Tabs mode.")

(defvar centaur-tabs-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km centaur-tabs-prefix-key centaur-tabs-prefix-map)

    ;;; Use mouse wheel to switch between buffers of same group
    (define-key km (kbd "<header-line> <mouse-5>") 'centaur-tabs-forward )
    (define-key km (kbd "<header-line> <mouse-4>") 'centaur-tabs-backward )

    ;;; Use right click to show the rest of groups
    (define-key km (kbd "<header-line> <mouse-3>") 'centaur-tabs--groups-menu )


    km)
  "Keymap to use in  Centaur-Tabs mode.")

(defvar centaur-tabs--global-hlf nil)

;;;###autoload
(define-minor-mode centaur-tabs-mode
  "Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{centaur-tabs-mode-map}"
  :group 'centaur-tabs
  :require 'centaur-tabs
  :global t
  :keymap centaur-tabs-mode-map
  (if centaur-tabs-mode
;;; ON
      (unless (centaur-tabs-mode-on-p)
	;; Save current default value of `header-line-format'.
	(setq centaur-tabs--global-hlf (default-value 'header-line-format))
	(centaur-tabs-init-tabsets-store)
	(setq-default header-line-format centaur-tabs-header-line-format))
;;; OFF
    (when (centaur-tabs-mode-on-p)
      ;; Turn off Centaur-Tabs-Local mode globally.
      (mapc #'(lambda (b)
		(condition-case nil
		    (with-current-buffer b
		      (and centaur-tabs-local-mode
			   (centaur-tabs-local-mode -1)))
		  (error nil)))
	    (buffer-list))
      ;; Restore previous `header-line-format'.
      (setq-default header-line-format centaur-tabs--global-hlf)
      (centaur-tabs-free-tabsets-store))
    ))

;;; Tab bar buffer setup
;;
(defun centaur-tabs-buffer-init ()
  "Initialize tab bar buffer data.
Run as `centaur-tabs-init-hook'."
  (setq centaur-tabs--buffers nil
	centaur-tabs-current-tabset-function 'centaur-tabs-buffer-tabs
	centaur-tabs-tab-label-function 'centaur-tabs-buffer-tab-label
	centaur-tabs-select-tab-function 'centaur-tabs-buffer-select-tab
	)
  ;; If set, initialize selected overline
  (when (eq centaur-tabs-set-bar 'under)
    (set-face-attribute 'centaur-tabs-selected nil
			:underline (face-background 'centaur-tabs-active-bar-face)
			:overline nil)
    (set-face-attribute 'centaur-tabs-selected-modified nil
			:underline (face-background 'centaur-tabs-active-bar-face)
			:overline nil)
    (set-face-attribute 'centaur-tabs-unselected nil
			:underline nil
			:overline nil)
    (set-face-attribute 'centaur-tabs-unselected-modified nil
			:underline nil
			:overline nil))
  (when (eq centaur-tabs-set-bar 'over)
    (set-face-attribute 'centaur-tabs-selected nil
			:overline (face-background 'centaur-tabs-active-bar-face)
			:underline nil)
    (set-face-attribute 'centaur-tabs-selected-modified nil
			:overline (face-background 'centaur-tabs-active-bar-face)
			:underline nil)
    (set-face-attribute 'centaur-tabs-unselected nil
			:overline nil
			:underline nil)
    (set-face-attribute 'centaur-tabs-unselected-modified nil
			:overline nil
			:underline nil))
  (add-hook 'after-save-hook #'centaur-tabs-on-saving-buffer)
  (add-hook 'first-change-hook #'centaur-tabs-on-modifying-buffer)
  (add-hook 'after-change-functions #'centaur-tabs-after-modifying-buffer)
  (add-hook 'kill-buffer-hook #'centaur-tabs-buffer-track-killed))

(defun centaur-tabs-buffer-quit ()
  "Quit tab bar buffer.
Run as `centaur-tabs-quit-hook'."
  (setq centaur-tabs--buffers nil
	centaur-tabs-current-tabset-function nil
	centaur-tabs-tab-label-function nil
	centaur-tabs-select-tab-function nil
	)
  (remove-hook 'after-save-hook 'centaur-tabs-after-modifying-buffer)
  (remove-hook 'first-change-hook 'centaur-tabs-on-modifying-buffer)
  (remove-hook 'after-change-functions 'centaur-tabs-after-modifying-buffer)
  (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed))

(add-hook 'centaur-tabs-init-hook #'centaur-tabs-buffer-init)
(add-hook 'centaur-tabs-quit-hook #'centaur-tabs-buffer-quit)

(provide 'centaur-tabs)

;;; centaur-tabs.el ends here
