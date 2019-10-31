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
;;; Require
(require 'cl-lib)
(require 'color)
(require 'powerline)

;; Compiler pacifier
(declare-function ivy-read "ext:ivy.el" t t)
(declare-function helm-build-sync-source "ext:helm-source.el" t t)
(declare-function all-the-icons-match? "ext:all-the-icons.el" t t)
(declare-function all-the-icons-auto-mode-match? "ext:all-the-icons.el" t t)
(declare-function all-the-icons-icon-for-file "ext:all-the-icons.el" t t)
(declare-function all-the-icons-icon-for-mode "ext:all-the-icons.el" t t)
(declare-function projectile-project-root "ext:projectile.el" t t)
(declare-function projectile-project-name "ext:projectile.el" t t)
(defvar helm-source-centaur-tabs-group)

;;;;;;;;;;;;;;;;;;;;;;; Centaur-Tabs source code ;;;;;;;;;;;;;;;;;;;;;;;

(defgroup centaur-tabs nil
  "Display a tab bar in the header line."
  :group 'convenience)

;;; Faces
;;
(defface centaur-tabs-active-bar-face
  '((t (:background "cyan")))
  "Face used for the dirname part of the buffer path.")

(defface centaur-tabs-default
  '((t
     (:background "black" :foreground "black")))
  "Default face used in the tab bar."
  :group 'centaur-tabs)

(defface centaur-tabs-unselected
  '((t
     (:background "#3D3C3D" :foreground "grey50")))
  "Face used for unselected tabs."
  :group 'centaur-tabs)

(defface centaur-tabs-selected
  '((t (:background "#31343E" :foreground "white")))
  "Face used for the selected tab."
  :group 'centaur-tabs)

(defface centaur-tabs-unselected-modified
  '((t
     (:background "#3D3C3D" :foreground "grey50")))
  "Face used for unselected-modified tabs."
  :group 'centaur-tabs)

(defface centaur-tabs-selected-modified
  '((t (:background "#31343E" :foreground "white")))
  "Face used for the selected-modified tab."
  :group 'centaur-tabs)

(defface centaur-tabs-close-unselected
  '((t
     (:inherit centaur-tabs-unselected)))
  "Face used for unselected close button."
  :group 'centaur-tabs)

(defface centaur-tabs-close-selected
  '((t (:inherit centaur-tabs-selected)))
  "Face used for selected close button."
  :group 'centaur-tabs)

(defface centaur-tabs-close-mouse-face
  '((t (:inherit underline)))
  "Face used for close button when hovered with the mouse."
  :group 'centaur-tabs)

(defface centaur-tabs-modified-marker-selected
  `((t (:inherit centaur-tabs-selected)))
  "Face used for selected modified marker."
  :group 'centaur-tabs)

(defface centaur-tabs-modified-marker-unselected
  `((t (:inherit centaur-tabs-unselected)))
  "Face used for unselected modified marker."
  :group 'centaur-tabs)

(defvar centaur-tabs-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) 'centaur-tabs-do-close)
    (define-key map (vector 'header-line 'mouse-2) 'centaur-tabs-do-close)
    map)
  "Keymap used for setting mouse events for close button.")


(defvar centaur-tabs-backward-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) 'centaur-tabs-backward--button)
    (define-key map (vector 'header-line 'mouse-3) 'centaur-tabs--groups-menu)
    map)
  "Keymap used for setting mouse events for backward tab button.")

(defvar centaur-tabs-forward-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) 'centaur-tabs-forward--button)
    (define-key map (vector 'header-line 'mouse-3) 'centaur-tabs--groups-menu)
    map)
  "Keymap used for setting mouse events for forward tab button.")

(defvar centaur-tabs-down-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) 'centaur-tabs--groups-menu)
    (define-key map (vector 'header-line 'mouse-3) 'centaur-tabs--groups-menu)
    map)
  "Keymap used for setting mouse events for down tab button.")




(defvar centaur-tabs-default-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) 'centaur-tabs-do-select)
    (define-key map (vector 'header-line 'mouse-2) 'centaur-tabs-do-close)
    map)
  "Keymap used for setting mouse events for a tab.")

(defvar centaur-tabs-icon-scale-factor
  1.0
  "The base scale factor for the `height' face property of tab icons.")

(defvar centaur-tabs-icon-v-adjust
  0.01
  "The vertical adjust for tab icons.")

;;; Customs
;;
(defcustom centaur-tabs-cycle-scope nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- default
    Navigate through visible tabs, then through tab groups."
  :group 'centaur-tabs
  :type '(choice :tag "Cycle through..."
		 (const :tag "Visible Tabs Only" tabs)
		 (const :tag "Tab Groups Only" groups)
		 (const :tag "Visible Tabs then Tab Groups" nil)))

(defcustom centaur-tabs-auto-scroll-flag t
  "*Non-nil means to automatically scroll the tab bar.
That is, when a tab is selected outside of the tab bar visible area,
the tab bar is scrolled horizontally so the selected tab becomes
visible."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-common-group-name "Common"
  "If the current buffer does not belong to any project the group name uses the name of this variable."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-label-fixed-length 0
  "Fixed length of label.  Set to 0 if dynamic."
  :group 'centaur-tabs
  :type 'int)

(defcustom centaur-tabs-hide-tabs-hooks
  '(magit-status-mode-hook
    magit-popup-mode-hook
    reb-mode-hook
    completion-list-mode-hook)
  "Set hooks to buffer in which it isn't desired to have tabs."
  :type '(repeat symbol)
  :group 'centaur-tabs)

(defcustom centaur-tabs-background-color (face-background 'centaur-tabs-default)
  "*Background color of the tab bar.
By default, use the background color specified for the
`centaur-tabs-default' face (or inherited from another face), or the
background color of the `default' face otherwise."
  :group 'centaur-tabs
  :type 'face)

(defcustom centaur-tabs-height 22
  "The height of tab."
  :group 'centaur-tabs
  :type 'int)

(defcustom centaur-tabs-bar-height (+ 8 centaur-tabs-height)
  "The height of bar."
  :group 'centaur-tabs
  :type 'int)

(defcustom centaur-tabs-style "bar"
  "The style of tab."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-set-icons nil
  "When non nil, display an icon from all-the-icons alongside the tab name."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-gray-out-icons nil
  "When non nil, enable gray icons for unselected buffer."
  :group 'centaur-tabs
  :type '(choice :tag "Gray out icons for unselected..."
		 (const :tag "Buffer" buffer)))

(defcustom centaur-tabs-set-bar nil
  "When non nil, display a bar to show the currently selected tab.
There are two options:
- 'left: displays the bar at the left of the currently selected tab.
- 'over: displays the bar over the currently selected tab."
  :group 'centaur-tabs
  :type '(choice :tag "Display bar at..."
		 (const :tag "Put bar on the left" left)
		 (const :tag "Put bar as an overline" over)))

(defcustom centaur-tabs-set-close-button t
  "When non nil, display a clickable x button for closing the tabs."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-close-button (make-string 1 #x00D7)
  "When non nil, display a clickable x button for closing the tabs."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-set-modified-marker nil
  "When non nil, display a marker when the buffer is modified."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-modified-marker (make-string 1 #x23FA)
  "When non nil, display a marker when the buffer is modified."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-mouse-pointer 'hand
  "Cursor to display when hovering the tabs.
Default is 'hand.  The following scopes are possible:
- arrow
- hand
- vdrag
- hdrag
- modeline
- hourglass"
  :group 'centaur-tabs
  :type 'variable)

(defvar centaur-tabs-hide-tab-function 'centaur-tabs-hide-tab
  "Function to hide tab.
This function filters tabs.  The tab will hide if this function returns nil.")

(defvar centaur-tabs-current-tabset-function nil
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar.")

(defvar centaur-tabs-tab-label-function nil
  "Function that obtains a tab label displayed on the tab bar.
The function is passed a tab and should return a string.")

(defvar centaur-tabs-select-tab-function nil
  "Function that select a tab.
The function is passed a tab, and should make it the
selected tab.")

(defvar centaur-tabs-buffer-list-function 'centaur-tabs-buffer-list
  "Function that returns the list of buffers to show in tabs.
That function is called with no arguments and must return a list of
buffers.")

(defvar centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups
  "Function that gives the group names the current buffer belongs to.
It must return a list of group names, or nil if the buffer has no
group.  Notice that it is better that a buffer belongs to one group.")

(defvar centaur-tabs-adjust-buffer-order-function 'centaur-tabs-adjust-buffer-order
  "Function to adjust buffer order after switch tab.
Default is `centaur-tabs-adjust-buffer-order', you can write your own rule.")

(defcustom centaur-tabs-adjust-buffer-order nil
  "Set automatic buffer ordering for buffer changing commands.
The ordering is appliet for non click or tab motion commands.
There are four options:
1 - nil: No ordering applied
2 - t: Move the currently selected tab to the side (right or left) of the last
visited tab.
3 - left: Move the currently selected tab to left of the last visited tab.
4 - right: Move the currently selected tab to right of the last visited tab."
  :group 'centaur-tabs
  :type '(choice :tag "Automatic buffer reordering..."
		 (const :tag "Do not adjust buffer order." nil)
		 (const :tag "When the currently selected tab(A) is at the right of the last visited
tab(B), move A to the right of B. When the currently selected tab(A) is at the left of the last visited
tab(B), move A to the left of B" t)
		 (const :tag "Move the currently selected tab to the left of the the last visited tab." left)
		 (const :tag "Move the currently selected tab to the right of the the last visited tab." right)))

(defcustom centaur-tabs-show-navigation-buttons nil
  "When non-nil, show the buttons for backward/forward tabs."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-down-tab-text " ▾ "
  "Text icon to show in the down button tab."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-backward-tab-text " ⏴ "
  "Text icon to show in the backward button tab."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-forward-tab-text " ⏵ "
  "Text icon to show in the forward button tab."
  :group 'centaur-tabs
  :type 'string)


(defvar centaur-tabs--buffer-show-groups nil)

;; Separators
(defvar centaur-tabs-style-left nil)
(defvar centaur-tabs-style-right nil)

;;; Misc.
;;
(eval-and-compile
  (defalias 'centaur-tabs-display-update
    (if (fboundp 'force-window-update)
	#'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update)))

;; Copied from s.el
(defun centaur-tabs-truncate-string (len s &optional ellipsis)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters
long.

When not specified, ELLIPSIS defaults to ‘...’."
  (declare (pure t) (side-effect-free t))
  (unless ellipsis
    (setq ellipsis "..."))
  (if (> (length s) len)
      (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
    (concat s (make-string (- len (length s)) ? ))))

;;; Tab and tab set
;;
(defsubst centaur-tabs-make-tab (object tabset)
  "Return a new tab with value OBJECT.
TABSET is the tab set the tab belongs to."
  (cons object tabset))

(defsubst centaur-tabs-tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

(defsubst centaur-tabs-tab-tabset (tab)
  "Return the tab set TAB belongs to."
  (cdr tab))

(defvar centaur-tabs-tabsets nil
  "The tab sets store.")

(defvar centaur-tabs-tabsets-tabset nil
  "The special tab set of existing tab sets.")

(defvar centaur-tabs-current-tabset nil
  "The tab set currently displayed on the tab bar.")
(make-variable-buffer-local 'centaur-tabs-current-tabset)

(defvar centaur-tabs-init-hook nil
  "Hook run after tab bar data has been initialized.
You should use this hook to initialize dependent data.")

(defsubst centaur-tabs-init-tabsets-store ()
  "Initialize the tab set store."
  (setq centaur-tabs-tabsets (make-vector 31 0)
	centaur-tabs-tabsets-tabset (make-symbol "centaur-tabs-tabsets-tabset"))
  (put centaur-tabs-tabsets-tabset 'start 0)
  (run-hooks 'centaur-tabs-init-hook))

(defvar centaur-tabs-quit-hook nil
  "Hook run after tab bar data has been freed.
You should use this hook to reset dependent data.")

(defsubst centaur-tabs-free-tabsets-store ()
  "Free the tab set store."
  (setq centaur-tabs-tabsets nil
	centaur-tabs-tabsets-tabset nil)
  (run-hooks 'centaur-tabs-quit-hook))

(defun centaur-tabs--make-xpm (face width height)
  "Create an XPM bitmap via FACE WIDTH and HEIGHT.
Taken from `doom-modeline'."
  (when (and (display-graphic-p)
	     (image-type-available-p 'xpm))
    (propertize
     " " 'display
     (let ((data (make-list height (make-list width 1)))
	   (color (or (face-background face nil t) "None")))
       (ignore-errors
	 (create-image
	  (concat
	   (format
	    "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
	    (length (car data)) (length data) color color)
	   (apply #'concat
		  (cl-loop with idx = 0
			   with len = (length data)
			   for dl in data
			   do (cl-incf idx)
			   collect
			   (concat
			    "\""
			    (cl-loop for d in dl
				     if (= d 0) collect (string-to-char " ")
				     else collect (string-to-char "."))
			    (if (eq idx len) "\"};" "\",\n")))))
	  'xpm t :ascent 'center))))))

(defvar centaur-tabs-active-bar
  (centaur-tabs--make-xpm 'centaur-tabs-active-bar-face
			  2
			  centaur-tabs-bar-height))

;; Define an "hygienic" function free of side effect between its local
;; variables and those of the callee.
(eval-and-compile
  (defalias 'centaur-tabs-map-tabsets
    (let ((function (make-symbol "function"))
	  (result   (make-symbol "result"))
	  (tabset   (make-symbol "tabset")))
      `(lambda (,function)
	 "Apply FUNCTION to each tab set, and make a list of the results.
The result is a list just as long as the number of existing tab sets."
	 (let (,result)
	   (mapatoms
	    #'(lambda (,tabset)
		(push (funcall ,function ,tabset) ,result))
	    centaur-tabs-tabsets)
	   ,result)))))

(defun centaur-tabs-make-tabset (name &rest objects)
  "Make a new tab set whose name is the string NAME.
It is initialized with tabs build from the list of OBJECTS."
  (let* ((tabset (intern name centaur-tabs-tabsets))
	 (tabs (cl-mapcar #'(lambda (object)
			      (centaur-tabs-make-tab object tabset))
			  objects)))
    (set tabset tabs)
    (put tabset 'select (car tabs))
    (put tabset 'start 0)
    tabset))

(defsubst centaur-tabs-get-tabset (name)
  "Return the tab set whose name is the string NAME.
Return nil if not found."
  (intern-soft name centaur-tabs-tabsets))

(defsubst centaur-tabs-delete-tabset (tabset)
  "Delete the tab set TABSET.
That is, remove it from the tab sets store."
  (unintern tabset centaur-tabs-tabsets))

(defsubst centaur-tabs-tabs (tabset)
  "Return the list of tabs in TABSET."
  (symbol-value tabset))

(defsubst centaur-tabs-tab-values (tabset)
  "Return the list of tab values in TABSET."
  (cl-mapcar 'centaur-tabs-tab-value (centaur-tabs-tabs tabset)))

(defsubst centaur-tabs-get-tab (object tabset)
  "Search for a tab with value OBJECT in TABSET.
Return the tab found, or nil if not found."
  (assoc object (centaur-tabs-tabs tabset)))

(defsubst centaur-tabs-member (tab tabset)
  "Return non-nil if TAB is in TABSET."
  (or (eq (centaur-tabs-tab-tabset tab) tabset)
      (memq tab (centaur-tabs-tabs tabset))))

(defsubst centaur-tabs-template (tabset)
  "Return the cached visual representation of TABSET.
That is, a `header-line-format' template, or nil if the cache is
empty."
  (get tabset 'template))

(defsubst centaur-tabs-set-template (tabset template)
  "Set the cached visual representation of TABSET to TEMPLATE.
TEMPLATE must be a valid `header-line-format' template, or nil to
cleanup the cache."
  (put tabset 'template template))

(defsubst centaur-tabs-selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst centaur-tabs-selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (centaur-tabs-tab-value (centaur-tabs-selected-tab tabset)))

(defsubst centaur-tabs-selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (centaur-tabs-selected-tab tabset)))

(defvar centaur-tabs--track-selected nil)

(defsubst centaur-tabs-select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (centaur-tabs-member tab tabset)
    (unless (centaur-tabs-selected-p tab tabset)
      (centaur-tabs-set-template tabset nil)
      (setq centaur-tabs--track-selected centaur-tabs-auto-scroll-flag))
    (put tabset 'select tab)))

(defsubst centaur-tabs-select-tab-value (object tabset)
  "Make the tab with value OBJECT, the selected tab in TABSET.
Does nothing if a tab with value OBJECT is not found in TABSET.
Return the tab selected, or nil if nothing was selected."
  (centaur-tabs-select-tab (centaur-tabs-get-tab object tabset) tabset))

(defsubst centaur-tabs-start (tabset)
  "Return the index of the first visible tab in TABSET."
  (get tabset 'start))

(defsubst centaur-tabs-view (tabset)
  "Return the list of visible tabs in TABSET.
That is, the sub-list of tabs starting at the first visible one."
  (nthcdr (centaur-tabs-start tabset) (centaur-tabs-tabs tabset)))

(defun centaur-tabs-add-tab (tabset object)
  "Check if OBJECT tab is already open in TABSET.
Otherwise insert it."
  (let ((tabs (centaur-tabs-tabs tabset)))
    (if (centaur-tabs-get-tab object tabset)
	tabs
      (let* ((tab (centaur-tabs-make-tab object tabset))
	     (selected (centaur-tabs-selected-tab tabset))
	     (selected-index (cl-position (car selected) (cl-mapcar 'car tabs))))
	(centaur-tabs-set-template tabset nil)
	(set tabset (centaur-tabs-insert-at tabs selected-index tab))))))

(defun centaur-tabs-insert-at (list index insert-element)
  "Insert INSERT-ELEMENT in LIST at index INDEX."
  (let ((counter 0)
	result)
    (dolist (element list)
      (if (equal counter index)
	  (setq result (append result (list element insert-element)))
	(setq result (append result (list element))))
      (setq counter (+ 1 counter)))
    result))

(defun centaur-tabs-delete-tab (tab)
  "Remove TAB from its tab set."
  (let* ((tabset (centaur-tabs-tab-tabset tab))
	 (tabs   (centaur-tabs-tabs tabset))
	 (sel    (eq tab (centaur-tabs-selected-tab tabset)))
	 (next   (and sel (cdr (memq tab tabs)))))
    (centaur-tabs-set-template tabset nil)
    (setq tabs (delq tab tabs))
    ;; When the selected tab is deleted, select the next one, if
    ;; available, or the last one otherwise.
    (and sel (centaur-tabs-select-tab (car (or next (last tabs))) tabset))
    (set tabset tabs)))

(defun centaur-tabs-scroll (tabset count)
  "Scroll the visible tabs in TABSET of COUNT units.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (centaur-tabs-start tabset) count))
		    (1- (length (centaur-tabs-tabs tabset))))))
    (when (/= start (centaur-tabs-start tabset))
      (centaur-tabs-set-template tabset nil)
      (put tabset 'start start))))

(defun centaur-tabs-tab-next (tabset tab &optional before)
  "Search in TABSET for the tab after TAB.
If optional argument BEFORE is non-nil, search for the tab before
TAB.  Return the tab found, or nil otherwise."
  (let* ((tabs (centaur-tabs-tabs tabset))
	 (tabs-len (1- (length tabs)))
	 (tab-pos (cl-position tab tabs)))
    (if before
	(if (= tab-pos 0)      nil (nth (1- tab-pos) tabs))
      (if (= tab-pos tabs-len) nil (nth (1+ tab-pos) tabs)))))

(defun centaur-tabs-current-tabset (&optional update)
  "Return the tab set currently displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`centaur-tabs-current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (and update centaur-tabs-current-tabset-function
       (setq centaur-tabs-current-tabset
	     (funcall centaur-tabs-current-tabset-function)))
  centaur-tabs-current-tabset)

(defun centaur-tabs-get-tabsets-tabset ()
  "Return the tab set of selected tabs in existing tabsets."
  (set centaur-tabs-tabsets-tabset (centaur-tabs-map-tabsets 'centaur-tabs-selected-tab))
  (centaur-tabs-scroll centaur-tabs-tabsets-tabset 0)
  (centaur-tabs-set-template centaur-tabs-tabsets-tabset nil)
  centaur-tabs-tabsets-tabset)

(defun centaur-tabs-icon (tab face selected)
  "Generate all-the-icons icon for TAB using FACE's background.
If icon gray out option enabled, gray out icon if not SELECTED."
  (if (featurep 'all-the-icons)
      (with-current-buffer (car tab)
	(let* ((icon
		(if (and (buffer-file-name)
			 (all-the-icons-auto-mode-match?))
		    (all-the-icons-icon-for-file
		     (file-name-nondirectory (buffer-file-name))
		     :v-adjust centaur-tabs-icon-v-adjust
	             :height centaur-tabs-icon-scale-factor)
		  (all-the-icons-icon-for-mode
		   major-mode
		   :v-adjust centaur-tabs-icon-v-adjust
		   :height centaur-tabs-icon-scale-factor)))
	       (background (face-background face))
	       (inactive (if (and (not selected)
				  (eq centaur-tabs-gray-out-icons 'buffer))
			     'mode-line-inactive
			   'unspecified))
	       (overline (if (eq centaur-tabs-set-bar 'over)
			     (face-attribute face :overline)
			   nil)))
	  (if (stringp icon)
	      (progn
		(propertize icon 'face `(:inherit ,(get-text-property 0 'face icon)
						  :inherit ,inactive
						  :background ,background
						  :overline ,overline)))
	    "")))
    ""))

;; Utility functions
(defun centaur-tabs-buffer-close-tab (tab)
  "Function for closing TAB."
  (let ((buffer (centaur-tabs-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))
    (centaur-tabs-display-update)))

(defun centaur-tabs-headline-match ()
  "Make headline use centaur-tabs-default-face."
  (set-face-attribute 'header-line nil :background (face-background 'centaur-tabs-unselected)
		      :box nil
		      :overline nil
		      :underline nil))

;; Hooks for modification
(defun centaur-tabs-saved-marker-update ()
  "Function to be run after the buffer is saved."
  (centaur-tabs-set-template centaur-tabs-current-tabset nil)
  (centaur-tabs-display-update))
(defun centaur-tabs-after-modifying-buffer (_begin _end _length)
  "Function to be run after the buffer is changed.
BEGIN, END and LENGTH are just standard arguments for after-changes-function
hooked functions"
  (centaur-tabs-set-template centaur-tabs-current-tabset nil)
  (centaur-tabs-display-update))

(defun centaur-tabs-force-update ()
  "Force update all tabs."
  (centaur-tabs-buffer-update-groups)
  (centaur-tabs-saved-marker-update))

(defun centaur-tabs-get-tab-from-event (event)
  "Given a mouse EVENT, extract the tab at the mouse point."
  (let ((pos (posn-string (event-start event))))
    (get-text-property (cdr pos) 'centaur-tabs-tab (car pos))))

(defun centaur-tabs-do-select (event)
  "Given a mouse EVENT, select the tab at the mouse point."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (centaur-tabs-buffer-select-tab `,(centaur-tabs-get-tab-from-event event)))

(defun centaur-tabs-do-close (event)
  "Given a mouse EVENT, close the tab at the mouse point."
  (interactive "e")
  (centaur-tabs-buffer-close-tab `,(centaur-tabs-get-tab-from-event event)))

;; Change the font and height for all tab faces
(defun centaur-tabs-change-fonts (family height)
  "Change the fonts of all the tabs.
FAMILY is the font family and HEIGHT is the font height."
  (dolist (centaur-face '(centaur-tabs-selected
			  centaur-tabs-selected-modified
			  centaur-tabs-unselected
			  centaur-tabs-unselected-modified))
    (set-face-attribute centaur-face nil :family family :height height)))

;;; Tabs
;;
(defsubst centaur-tabs-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `centaur-tabs-tab-label-function' to obtain a label for TAB."
  (let* ((buf (centaur-tabs-tab-value tab))
	 (buf-file-name (buffer-file-name buf))
	 (selected-p (centaur-tabs-selected-p tab (centaur-tabs-current-tabset)))
	 (not-read-only-p (with-current-buffer buf (not buffer-read-only)))
	 (modified-p (and not-read-only-p (buffer-modified-p buf)))
	 (use-mod-mark-p (and centaur-tabs-set-modified-marker modified-p))
	 (mod-mark-face (if selected-p
			    'centaur-tabs-modified-marker-selected
			  'centaur-tabs-modified-marker-unselected))
	 (face (if selected-p
		   (if modified-p
		       'centaur-tabs-selected-modified
		     'centaur-tabs-selected)
		 (if modified-p
		     'centaur-tabs-unselected-modified
		   'centaur-tabs-unselected)))
	 (bar (if (and selected-p (eq centaur-tabs-set-bar 'left))
		  (propertize
		   centaur-tabs-active-bar
		   'centaur-tabs-tab tab
		   'pointer centaur-tabs-mouse-pointer
		   'local-map centaur-tabs-default-map)
		""))
	 (icon (if (and centaur-tabs-set-icons
			(not centaur-tabs--buffer-show-groups))
		   (propertize
		    (centaur-tabs-icon tab face selected-p)
		    'centaur-tabs-tab tab
		    'pointer centaur-tabs-mouse-pointer
		    'help-echo (with-current-buffer buf (format-mode-line mode-name))
		    'local-map centaur-tabs-default-map)
		 "")))
    (when (or (not centaur-tabs-style-left)
	      (not centaur-tabs-style-right))
      (centaur-tabs-select-separator-style centaur-tabs-style))
    (concat
     (centaur-tabs-separator-render centaur-tabs-style-left face)
     bar

     ;; icon
     (if (= (length icon) 0) ""
       (concat
	(propertize
	 " "
	 'face face
	 'centaur-tabs-tab tab
	 'pointer centaur-tabs-mouse-pointer
	 'local-map centaur-tabs-default-map)
	icon))

     ;; tab name
     (propertize
      (concat
       (if centaur-tabs-tab-label-function
	   (funcall centaur-tabs-tab-label-function tab)
	 (buffer-name buf))
       " ")
      'centaur-tabs-tab tab
      'face face
      'pointer centaur-tabs-mouse-pointer
      'help-echo buf-file-name
      'local-map centaur-tabs-default-map)

     ;; close button and/or modified marker
     (if centaur-tabs-set-close-button
	 (propertize
	  (if use-mod-mark-p
	      centaur-tabs-modified-marker
	    centaur-tabs-close-button)
	  'face (if use-mod-mark-p
		    mod-mark-face
		  (if selected-p
		      'centaur-tabs-close-selected
		    'centaur-tabs-close-unselected))
	  'pointer centaur-tabs-mouse-pointer
	  'help-echo "Close buffer"
	  'centaur-tabs-tab tab
	  'mouse-face 'centaur-tabs-close-mouse-face
	  'local-map centaur-tabs-close-map)
       (if (and centaur-tabs-set-modified-marker modified-p)
	   (propertize
	    centaur-tabs-modified-marker
	    'face mod-mark-face
	    'pointer centaur-tabs-mouse-pointer
	    'centaur-tabs-tab tab
	    'help-echo buf-file-name
	    'local-map centaur-tabs-default-map)
	 ""))

     (propertize
      " "
      'face face
      'centaur-tabs-tab tab
      'pointer centaur-tabs-mouse-pointer
      'local-map centaur-tabs-default-map)
     (centaur-tabs-separator-render centaur-tabs-style-right face))))

(defsubst centaur-tabs-button-tab (button)
  "Return the display representation of button BUTTON.
That is, a propertized string used as an `header-line-format' template
element."
  (let* ((face 'centaur-tabs-unselected))
    (concat
     (centaur-tabs-separator-render centaur-tabs-style-left face)
     (propertize
      button
      'face face
      'mouse-face 'highlight)
     (centaur-tabs-separator-render centaur-tabs-style-right face))))

(defun centaur-tabs-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (centaur-tabs-selected-tab tabset))
	 (tabs (centaur-tabs-view tabset))
	 (padcolor centaur-tabs-background-color)
	 atsel elts)
    ;; Track the selected tab to ensure it is always visible.
    (when centaur-tabs--track-selected
      (while (not (memq sel tabs))
	(centaur-tabs-scroll tabset -1)
	(setq tabs (centaur-tabs-view tabset)))
      (while (and tabs (not atsel))
	(setq elts  (cons (centaur-tabs-line-tab (car tabs)) elts)
	      atsel (eq (car tabs) sel)
	      tabs  (cdr tabs)))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
	(let ((truncate-partial-width-windows nil)
	      (inhibit-modification-hooks t)
	      deactivate-mark ;; Prevent deactivation of the mark!
	      start)
	  (setq truncate-lines nil
		buffer-undo-list t)
	  (setq start (point))
	  (while (and (cdr elts) ;; Always show the selected tab!
		      (progn
			(delete-region start (point-max))
			(goto-char (point-max))
			(apply #'insert elts)
			(goto-char (point-min))
			(> (vertical-motion 1) 0)))
	    (centaur-tabs-scroll tabset 1)
	    (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq centaur-tabs--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (centaur-tabs-line-tab (car tabs)) elts)
	    tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (centaur-tabs-set-template
     tabset
     (list
      (centaur-tabs-line-format--buttons)
      (nreverse elts)
      (propertize "% "
                  'face (list :background padcolor)
                  'pointer 'arrow)))
    ))

(defun centaur-tabs-line-format--buttons ()
  "Return the buttons fragment of the header line."
  (if centaur-tabs-show-navigation-buttons
      (concat
       (propertize (centaur-tabs-button-tab centaur-tabs-down-tab-text)
                   'local-map centaur-tabs-down-tab-map
                   'help-echo "Change tab group")
       (propertize (centaur-tabs-button-tab centaur-tabs-backward-tab-text)
                   'local-map centaur-tabs-backward-tab-map
                   'help-echo "Previous tab")
       (propertize (centaur-tabs-button-tab centaur-tabs-forward-tab-text)
                   'local-map centaur-tabs-forward-tab-map
                   'help-echo "Next tab"))
    ""))

(defun centaur-tabs-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window `centaur-tabs-hide-tab-function' return nil."
  (cond
   ((centaur-tabs-hide-tab-cached (current-buffer))
    ;; Don't show the tab bar.
    (setq header-line-format nil))
   ((centaur-tabs-current-tabset t)
    ;; When available, use a cached tab bar value, else recompute it.
    (or (centaur-tabs-template centaur-tabs-current-tabset)
	(centaur-tabs-line-format centaur-tabs-current-tabset)))))

(defconst centaur-tabs-header-line-format '(:eval (centaur-tabs-line))
  "The tab bar header line format.")

(defun centaur-tabs-next-group (&optional backward)
  "Switch to next group/tabset.
If BACKWARD is non-nil switch to
previous group."
  (let*
      ((groups (seq-map 'centaur-tabs-get-tabset (centaur-tabs-get-groups)))
       (groups-len (1- (length groups)))
       (current-group-pos (cl-position (window-parameter nil 'tabset) groups)))
    (centaur-tabs-saved-marker-update)
    (set-window-parameter
     nil 'tabset
     (nth
      (if backward
	  (if (= current-group-pos 0) groups-len (1- current-group-pos))
	(if (= current-group-pos groups-len) 0 (1+ current-group-pos)))
      groups))))

(defun centaur-tabs-cycle-groups (&optional backward)
  "Cycle to the next tabset/tab-group.
If BACKWARD is non-nil cycle to the previous tabset."
  (rassq
   (centaur-tabs-next-group backward)
   (centaur-tabs-tabs (centaur-tabs-get-tabsets-tabset))))

(defun centaur-tabs-cycle-tabs (&optional backward)
  "Cycle to the next tab.
If BACKWARD is non-nil cycle to the
previous tab."
  (let* ((tabset centaur-tabs-current-tabset)
	 (tab (centaur-tabs-selected-tab tabset))
	 (tabs (centaur-tabs-tabs tabset))
	 (tabs-len (1- (length tabs)))
	 (tab-pos (cl-position tab tabs)))
    (nth
     (if backward
	(if (= tab-pos 0) tabs-len (1- tab-pos))
       (if (= tab-pos tabs-len) 0 (1+ tab-pos)))
     tabs)))

(defun centaur-tabs-cycle-default (&optional backward)
  "Switch to next tab.
If there is no next tab, cycle to next
group.  If BACKWARD is non-nil switch to previous tab.  If there is
no previous tab cycle to previous group."
  (let* ((tabset centaur-tabs-current-tabset)
	 (tab (centaur-tabs-selected-tab tabset))
	 (next-tab (centaur-tabs-tab-next tabset tab backward)))
    (if next-tab next-tab
      (let ((tabs (centaur-tabs-tabs (centaur-tabs-next-group backward))))
	(car (if backward (last tabs) tabs))))))

;;; Cyclic navigation through tabs
;;
(defun centaur-tabs-cycle (&optional backward)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `centaur-tabs-cycle-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead."
  (let (;; If navigation through groups is requested, and there is
	;; only one group, navigate through visible tabs.
	(cycle
	 (if (and (eq centaur-tabs-cycle-scope 'groups)
		  (not (cdr (centaur-tabs-tabs (centaur-tabs-get-tabsets-tabset)))))
	     'tabs
	   centaur-tabs-cycle-scope)))
    (centaur-tabs-buffer-select-tab
     (cond
      ((eq cycle 'tabs) (centaur-tabs-cycle-tabs backward))
      ((eq cycle 'groups) (centaur-tabs-cycle-groups backward))
      (t (centaur-tabs-cycle-default backward))))))

;;;###autoload
(defun centaur-tabs-backward ()
  "Select the previous available tab.
Depend on the setting of the option `centaur-tabs-cycle-scope'."
  (interactive)
  (if (centaur-tabs-current-tabset t)
      (centaur-tabs-cycle t)
    (previous-buffer)))

;;;###autoload
(defun centaur-tabs-forward ()
  "Select the next available tab.
Depend on the setting of the option `centaur-tabs-cycle-scope'."
  (interactive)
  (if (centaur-tabs-current-tabset t)
      (centaur-tabs-cycle)
    (next-buffer)))

;;;###autoload
(defun centaur-tabs-backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((centaur-tabs-cycle-scope 'groups))
    (centaur-tabs-cycle t)))

;;;###autoload
(defun centaur-tabs-forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((centaur-tabs-cycle-scope 'groups))
    (centaur-tabs-cycle)))

;;;###autoload
(defun centaur-tabs-backward-tab ()
  "Select the previous visible tab."
  (interactive)
  (let ((centaur-tabs-cycle-scope 'tabs))
    (centaur-tabs-cycle t)))

;;;###autoload
(defun centaur-tabs-forward-tab ()
  "Select the next visible tab."
  (interactive)
  (let ((centaur-tabs-cycle-scope 'tabs))
    (centaur-tabs-cycle)))

(defun centaur-tabs-backward--button (event)
  "Same as centaur-tabs-backward, but changing window to EVENT source."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (centaur-tabs-backward))


(defun centaur-tabs-forward--button (event)
  "Same as centaur-tabs-forward, but changing window to EVENT source."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (centaur-tabs-forward))


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

;;; Buffer tabs
;;
(defgroup centaur-tabs-buffer nil
  "Display buffers in the tab bar."
  :group 'centaur-tabs)

(defun centaur-tabs-filter-out (condp lst)
  "Filter list LST with using CONDP as the filtering condition."
  (delq nil
	(cl-mapcar (lambda (x) (if (funcall condp x) nil x)) lst)))

(defun centaur-tabs-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
  (centaur-tabs-filter-out
   'centaur-tabs-hide-tab-cached
   (delq nil
	 (cl-mapcar #'(lambda (b)
			(cond
			 ;; Always include the current buffer.
			 ((eq (current-buffer) b) b)
			 ((buffer-file-name b) b)
			 ((char-equal ?\  (aref (buffer-name b) 0)) nil)
			 ((buffer-live-p b) b)))
		    (buffer-list)))))

(defun centaur-tabs-buffer-mode-derived-p (mode parents)
  "Return non-nil if MODE derives from a mode in PARENTS."
  (let (derived)
    (while (and (not derived) mode)
      (if (memq mode parents)
	  (setq derived t)
	(setq mode (get mode 'derived-mode-parent))))
    derived))

;;; Group buffers in tab sets.
;;
(defvar centaur-tabs--buffers nil)

(defun centaur-tabs-buffer-update-groups ()
  "Update tabsets from groups of existing buffers.
Return the the first group where the current buffer is."
  (let ((bl (sort
	     (cl-mapcar
	      #'(lambda (b)
		  (with-current-buffer b
		    (list (current-buffer)
			  (buffer-name)
			  (if centaur-tabs-buffer-groups-function
			      (funcall centaur-tabs-buffer-groups-function)
			    '(centaur-tabs-common-group-name)))))
	      (and centaur-tabs-buffer-list-function
		   (funcall centaur-tabs-buffer-list-function)))
	     #'(lambda (e1 e2)
		 (string-lessp (nth 1 e1) (nth 1 e2))))))
    ;; If the cache has changed, update the tab sets.
    (unless (equal bl centaur-tabs--buffers)
      ;; Add new buffers, or update changed ones.
      (dolist (e bl)
	(dolist (g (nth 2 e))
	  (let ((tabset (centaur-tabs-get-tabset g)))
	    (if tabset
		(unless (equal e (assq (car e) centaur-tabs--buffers))
		  ;; This is a new buffer, or a previously existing
		  ;; buffer that has been renamed, or moved to another
		  ;; group.  Update the tab set, and the display.
		  (centaur-tabs-add-tab tabset (car e))
		  (centaur-tabs-set-template tabset nil))
	      (centaur-tabs-make-tabset g (car e))))))
      ;; Remove tabs for buffers not found in cache or moved to other
      ;; groups, and remove empty tabsets.
      (mapc 'centaur-tabs-delete-tabset
	    (centaur-tabs-map-tabsets
	     #'(lambda (tabset)
		 (dolist (tab (centaur-tabs-tabs tabset))
		   (let ((e (assq (centaur-tabs-tab-value tab) bl)))
		     (or (and e (memq tabset
				      (cl-mapcar 'centaur-tabs-get-tabset
						 (nth 2 e))))
			 (centaur-tabs-delete-tab tab))))
		 ;; Return empty tab sets
		 (unless (centaur-tabs-tabs tabset)
		   tabset))))
      ;; The new cache becomes the current one.
      (setq centaur-tabs--buffers bl))))

(defun centaur-tabs-get-group (&optional window)
  "Return the first group the current buffer belongs to.
Given optional WINDOW, get the tab group of that WINDOW."
  (let ((groups (nth 2 (assq (current-buffer) centaur-tabs--buffers)))
	(current-tabset (window-parameter window 'tabset)))
    (setq groups (seq-map 'centaur-tabs-get-tabset groups))
    (when (or (null current-tabset) (not (member current-tabset groups)))
      (setq current-tabset (set-window-parameter window 'tabset (car groups))))
    current-tabset))

;;; Tab bar callbacks
;;

(defsubst centaur-tabs-buffer-show-groups (flag)
  "Set display of tabs for groups of buffers to FLAG."
  (setq centaur-tabs--buffer-show-groups flag))

(defun centaur-tabs-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((tabset (centaur-tabs-get-group)))
    (centaur-tabs-select-tab-value (current-buffer) tabset)
    (when centaur-tabs--buffer-show-groups
      (setq tabset (centaur-tabs-get-tabsets-tabset))
      (centaur-tabs-select-tab-value (current-buffer) tabset))
    tabset))

;;; Separator
;;

(defvar ns-use-srgb-colorspace)

(defvar centaur-tabs-image-apple-rgb
  (and (eq (window-system) 'ns)
       ns-use-srgb-colorspace
       (< 11
	  (string-to-number
	   (and (string-match "darwin\\([0-9]+\\)" system-configuration)
		(match-string-no-properties 1 system-configuration)))))
  "Boolean variable to determine whether to use Apple RGB colorspace.
used to render images.

t on macOS 10.7+ and `ns-use-srgb-colorspace' is t, nil otherwise.

This variable is automatically set, there's no need to modify it.")

(defun centaur-tabs-separator-interpolate (color1 color2)
  "Interpolate between COLOR1 and COLOR2.

COLOR1 and COLOR2 must be supplied as hex strings with a leading #."
  (let* ((c1 (color-name-to-rgb color1))
	 (c2 (color-name-to-rgb color2))
	 (red (/ (+ (nth 0 c1) (nth 0 c2)) 2))
	 (green (/ (+ (nth 1 c1) (nth 1 c2)) 2))
	 (blue (/ (+ (nth 2 c1) (nth 2 c2)) 2)))
    (color-rgb-to-hex red green blue)))

(defun centaur-tabs-separator-color-xyz-to-apple-rgb (X Y Z)
  "Convert CIE X Y Z colors to Apple RGB color space."
  (let ((r (+ (* 3.2404542 X) (* -1.5371385 Y) (* -0.4985314 Z)))
	(g (+ (* -0.9692660 X) (* 1.8760108 Y) (* 0.0415560 Z)))
	(b (+ (* 0.0556434 X) (* -0.2040259 Y) (* 1.0572252 Z))))
    (list (expt r (/ 1.8)) (expt g (/ 1.8)) (expt b (/ 1.8)))))

(defun centaur-tabs-separator-color-srgb-to-apple-rgb (red green blue)
  "Convert RED GREEN BLUE colors from sRGB color space to Apple RGB.
RED, GREEN and BLUE should be between 0.0 and 1.0, inclusive."
  (apply #'centaur-tabs-separator-color-xyz-to-apple-rgb (color-srgb-to-xyz red green blue)))

(defun centaur-tabs-separator-hex-color (color)
  "Get the hexadecimal value of COLOR."
  (when color
    (let ((srgb-color (color-name-to-rgb color)))
      (if centaur-tabs-image-apple-rgb
	  (apply #'color-rgb-to-hex (apply #'centaur-tabs-separator-color-srgb-to-apple-rgb srgb-color))
	(apply #'color-rgb-to-hex srgb-color)))))

(defun centaur-tabs-separator-pattern (lst)
  "Turn LST into an infinite pattern."
  (when lst
    (let ((pattern (cl-copy-list lst)))
      (setcdr (last pattern) pattern))))

(defun centaur-tabs-separator-pattern-to-string (pattern)
  "Convert a PATTERN into a string that can be used in an XPM."
  (concat "\"" (mapconcat #'number-to-string pattern "") "\","))

(defun centaur-tabs-separator-reverse-pattern (pattern)
  "Reverse each line in PATTERN."
  (cl-mapcar 'reverse pattern))

(defun centaur-tabs-separator-row-pattern (fill total &optional fade)
  "Make a list that has FILL 0s out of TOTAL 1s with FADE 2s to the right of the fill."
  (unless fade
    (setq fade 0))
  (let ((fill (min fill total))
	(fade (min fade (max (- total fill) 0))))
    (append (make-list fill 0)
	    (make-list fade 2)
	    (make-list (- total fill fade) 1))))

(defun centaur-tabs-separator-pattern-bindings-body (patterns height-exp pattern-height-sym
							      second-pattern-height-sym)
  "Create let-var bindings and a function body from PATTERNS.
The `car' and `cdr' parts of the result can be passed to the
function `centaur-tabs-separator-wrap-defun' as its `let-vars'
and `body' arguments,respectively.  HEIGHT-EXP is an expression
 calculating the image height and it should contain a free variable `height'.
PATTERN-HEIGHT-SYM and SECOND-PATTERN-HEIGHT-SYM are symbols used
for let-var binding variables."
  (let* ((pattern (centaur-tabs-separator-pattern (cl-mapcar 'centaur-tabs-separator-pattern-to-string (car patterns))))
	 (header (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 1 patterns)))
	 (footer (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 2 patterns)))
	 (second-pattern (centaur-tabs-separator-pattern (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 3 patterns))))
	 (center (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 4 patterns)))
	 (reserve (+ (length header) (length footer) (length center))))
    (when pattern
      (cons `((,pattern-height-sym (max (- ,height-exp ,reserve) 0))
	      (,second-pattern-height-sym (/ ,pattern-height-sym 2))
	      (,pattern-height-sym ,(if second-pattern `(ceiling ,pattern-height-sym 2) `,pattern-height-sym)))
	    (list (when header `(mapconcat 'identity ',header ""))
		  `(mapconcat 'identity
			      (cl-subseq ',pattern 0 ,pattern-height-sym) "")
		  (when center `(mapconcat 'identity ',center ""))
		  (when second-pattern
		    `(mapconcat 'identity
				(cl-subseq ',second-pattern
					   0 ,second-pattern-height-sym) ""))
		  (when footer `(mapconcat 'identity ',footer "")))))))

(defun centaur-tabs-separator-pattern-defun (name dir width &rest patterns)
  "Create a powerline function of NAME in DIR with WIDTH for PATTERNS.

PATTERNS is of the form (PATTERN HEADER FOOTER SECOND-PATTERN CENTER
PATTERN-2X HEADER-2X FOOTER-2X SECOND-PATTERN-2X CENTER-2X).
PATTERN is required, all other components are optional.
The first 5 components are for the standard resolution image.
The remaining ones are for the high resolution image where both
width and height are doubled.  If PATTERN-2X is nil or not given,
then the remaining components are ignored and the standard
resolution image with magnification and interpolation will be
used in high resolution environments

All generated functions generate the form:
HEADER
PATTERN ...
CENTER
SECOND-PATTERN ...
FOOTER

PATTERN and SECOND-PATTERN repeat infinitely to fill the space needed to
generate a full height XPM.

PATTERN, HEADER, FOOTER, SECOND-PATTERN, CENTER are of the form
\((COLOR ...) (COLOR ...) ...).

COLOR can be one of 0, 1, or 2, where 0 is the source color, 1 is the
destination color, and 2 is the interpolated color between 0 and 1."
  (when (eq dir 'right)
    (setq patterns (cl-mapcar 'centaur-tabs-separator-reverse-pattern patterns)))
  (let ((bindings-body (centaur-tabs-separator-pattern-bindings-body patterns
								     'height
								     'pattern-height
								     'second-pattern-height))
	(bindings-body-2x (centaur-tabs-separator-pattern-bindings-body (nthcdr 5 patterns)
									'(* height 2)
									'pattern-height-2x
									'second-pattern-height-2x)))
    (centaur-tabs-separator-wrap-defun name dir width
				       (append (car bindings-body) (car bindings-body-2x))
				       (cdr bindings-body) (cdr bindings-body-2x))))

(defun centaur-tabs-separator-background-color (face)
  "Set the separator background color using FACE."
  (face-attribute face
		  (if (face-attribute face :inverse-video nil 'default)
		      :foreground
		    :background)
		  nil
		  'default))

(defun centaur-tabs-separator-wrap-defun (name dir width let-vars body &optional body-2x)
  "Generate a powerline function of name NAME in dir DIR.
This is made with WIDTH using LET-VARS and BODY.
BODY-2X is an optional argument."
  (let* ((src-face (if (eq dir 'left) 'face1 'face2))
	 (dst-face (if (eq dir 'left) 'face2 'face1)))
    `(defun ,(intern (format "powerline-%s-%s" name (symbol-name dir)))
	 (face1 face2 &optional height)
       (when window-system
	 (unless height (setq height centaur-tabs-height))
	 (let* ,(append `((color1 (when ,src-face
				    (centaur-tabs-separator-hex-color (centaur-tabs-separator-background-color ,src-face))))
			  (color2 (when ,dst-face
				    (centaur-tabs-separator-hex-color (centaur-tabs-separator-background-color ,dst-face))))
			  (colori (when (and color1 color2) (centaur-tabs-separator-interpolate color1 color2)))
			  (color1 (or color1 "None"))
			  (color2 (or color2 "None"))
			  (colori (or colori "None")))
			let-vars)
	   (apply #'create-image
		  ,(append `(concat (format "/* XPM */ static char * %s_%s[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
					    ,(replace-regexp-in-string "-" "_" name)
					    (symbol-name ',dir)
					    ,width
					    height
					    color1
					    color2
					    colori))
			   body
			   '("};"))
		  'xpm t
		  :ascent 'center
		  :face (when (and face1 face2)
			  ,dst-face)
		  ,(and body-2x
			`(and (featurep 'mac)
			      (list :data-2x
				    ,(append `(concat (format "/* XPM */ static char * %s_%s_2x[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
							      ,(replace-regexp-in-string "-" "_" name)
							      (symbol-name ',dir)
							      (* ,width 2)
							      (* height 2)
							      color1
							      color2
							      colori))
					     body-2x
					     '("};")))))))))))

(defun centaur-tabs-separator-alternate (dir)
  "Generate an alternating pattern XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "alternate" dir 4
					'((2 2 1 1)
					  (0 0 2 2))
					nil nil nil nil
					;; 2x
					'((2 2 2 2 1 1 1 1)
					  (2 2 2 2 1 1 1 1)
					  (0 0 0 0 2 2 2 2)
					  (0 0 0 0 2 2 2 2))))

(defun centaur-tabs-separator-bar (dir)
  "Generate a bar XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "bar" dir 2
					'((2 2))))

(defun centaur-tabs-separator-box (dir)
  "Generate a box XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "box" dir 2
					'((0 0)
					  (0 0)
					  (1 1)
					  (1 1))
					nil nil nil nil
					;; 2x
					'((0 0 0 0)
					  (0 0 0 0)
					  (0 0 0 0)
					  (0 0 0 0)
					  (1 1 1 1)
					  (1 1 1 1)
					  (1 1 1 1)
					  (1 1 1 1))))

(defun centaur-tabs-separator-chamfer (dir)
  "Generate a chamfer XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "chamfer" dir 3
					'((0 0 0))
					'((1 1 1)
					  (0 1 1)
					  (0 0 1))
					nil nil nil
					;; 2x
					'((0 0 0 0 0 0))
					'((1 1 1 1 1 1)
					  (0 1 1 1 1 1)
					  (0 0 1 1 1 1)
					  (0 0 0 1 1 1)
					  (0 0 0 0 1 1)
					  (0 0 0 0 0 1))))

(defun centaur-tabs-separator-rounded (dir)
  "Generate a rounded XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "rounded" dir 6
					'((0 0 0 0 0 0))
					'((2 1 1 1 1 1)
					  (0 0 2 1 1 1)
					  (0 0 0 0 1 1)
					  (0 0 0 0 2 1)
					  (0 0 0 0 0 1)
					  (0 0 0 0 0 2))
					nil nil nil
					;; 2x
					'((0 0 0 0 0 0 0 0 0 0 0 0))
					'((1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 2 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 2 1 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1)
					  (0 0 0 0 0 0 0 0 0 0 2 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1))))

(defun centaur-tabs-separator-slant (dir)
  "Generate a slant XPM function for DIR."
  (let* ((row-modifier (if (eq dir 'left) 'identity 'reverse)))
    (centaur-tabs-separator-wrap-defun "slant" dir 'width
				       '((width (1- (ceiling height 2))))
				       `((cl-loop for i from 0 to (1- height)
						  concat (centaur-tabs-separator-pattern-to-string (,row-modifier (centaur-tabs-separator-row-pattern (/ i 2) width)))))
				       `((cl-loop for i from 0 to (1- (* height 2))
						  concat (centaur-tabs-separator-pattern-to-string (,row-modifier (centaur-tabs-separator-row-pattern (/ i 2) (* width 2)))))))))

(defun centaur-tabs-separator-wave (dir)
  "Generate a wave XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "wave" dir 11
					'((0 0 0 0 0 0 1 1 1 1 1))
					'((2 1 1 1 1 1 1 1 1 1 1)
					  (0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 2 1 1 1 1 1 1 1)
					  (0 0 0 0 1 1 1 1 1 1 1)
					  (0 0 0 0 2 1 1 1 1 1 1)
					  (0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 2 1 1 1 1 1))
					'((0 0 0 0 0 0 2 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1)
					  (0 0 0 0 0 0 0 2 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1)
					  (0 0 0 0 0 0 0 0 2 1 1)
					  (0 0 0 0 0 0 0 0 0 0 2))
					nil nil
					;; 2x
					'((0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1))
					'((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1))
					'((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defun centaur-tabs-separator-zigzag (dir)
  "Generate a zigzag pattern XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "zigzag" dir 3
					'((1 1 1)
					  (0 1 1)
					  (0 0 1)
					  (0 0 0)
					  (0 0 1)
					  (0 1 1))
					nil nil nil nil
					;; 2x
					'((1 1 1 1 1 1)
					  (0 1 1 1 1 1)
					  (0 0 1 1 1 1)
					  (0 0 0 1 1 1)
					  (0 0 0 0 1 1)
					  (0 0 0 0 0 1)
					  (0 0 0 0 0 0)
					  (0 0 0 0 0 1)
					  (0 0 0 0 1 1)
					  (0 0 0 1 1 1)
					  (0 0 1 1 1 1)
					  (0 1 1 1 1 1))))

(defun centaur-tabs-separator-memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the memoized function over
the original function.  Use frame-local memoization."
  (cl-typecase func
    (symbol (fset func (centaur-tabs-separator-memoize-wrap-frame-local (symbol-function func))) func)
    (function (centaur-tabs-separator-memoize-wrap-frame-local func))))

(defun centaur-tabs-separator-memoize-wrap-frame-local (func)
  "Return the memoized version of FUNC.
The memoization cache is frame-local."
  (let ((funcid (cl-gensym)))
    `(lambda (&rest args)
       ,(concat (documentation func) (format "\n(memoized function %s)" funcid))
       (let* ((cache (centaur-tabs-separator-create-or-get-cache))
	      (key (cons ',funcid args))
	      (val (gethash key cache)))
	 (if val
	     val
	   (puthash key (apply ,func args) cache))))))

(defun centaur-tabs-separator-create-or-get-cache ()
  "Return a frame-local hash table that acts as a memoization cache.
The cache is for the powerline.
Create one if the frame doesn't have one yet."
  (let ((table (frame-parameter nil 'powerline-cache)))
    (if (hash-table-p table) table (centaur-tabs-separator-reset-cache))))

(defun centaur-tabs-separator-reset-cache ()
  "Reset and return the frame-local hash table used for a memoization cache."
  (let ((table (make-hash-table :test 'equal)))
    ;; Store it as a frame-local variable
    (modify-frame-parameters nil `((powerline-cache . ,table)))
    table))

(centaur-tabs-separator-memoize (centaur-tabs-separator-alternate 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-alternate 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-bar 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-bar 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-box 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-box 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-chamfer 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-chamfer 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-rounded 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-rounded 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-slant 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-slant 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-wave 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-wave 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-zigzag 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-zigzag 'right))

(defun centaur-tabs-select-separator-style (tab-style)
  "Set the separator style to TAB-STYLE."
  (setq centaur-tabs-style-left (funcall (intern (format "powerline-%s-right" tab-style)) 'centaur-tabs-default nil centaur-tabs-height))
  (setq centaur-tabs-style-right (funcall (intern (format "powerline-%s-left" tab-style)) nil 'centaur-tabs-default centaur-tabs-height)))

(defun centaur-tabs-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  ;; Init tab style.
  ;; Render tab.
  (format " %s"
	  (let ((bufname (if centaur-tabs--buffer-show-groups
			     (centaur-tabs-tab-tabset tab)
			   (buffer-name (car tab)))))
	    (if (> centaur-tabs-label-fixed-length 0)
		(centaur-tabs-truncate-string  centaur-tabs-label-fixed-length bufname)
	      bufname))))

(defvar centaur-tabs-last-scroll-y 0
  "Holds the scroll y of window from the last run of post-command-hooks.")

(defun centaur-tabs-separator-render (item face)
  "Render ITEM using FACE."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
		'face face))
   (t item)))

(defvar centaur-tabs-last-focused-buffer nil
  "The last focused buffer.")

(defvar centaur-tabs-last-focused-buffer-group nil
  "The group name of last focused buffer.")

(defun centaur-tabs-buffer-select-tab (tab)
  "Select TAB."
  (let ((buffer (centaur-tabs-tab-value tab))
	(group (centaur-tabs-tab-tabset tab)))
    (switch-to-buffer buffer)
    (setq centaur-tabs-last-focused-buffer buffer)
    (setq centaur-tabs-last-focused-buffer-group group)
    ;; (centaur-tabs-buffer-show-groups nil)
    (centaur-tabs-display-update)
    ))

(defun centaur-tabs-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Centaur-Tabs mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and (eq header-line-format centaur-tabs-header-line-format)
       (eq centaur-tabs-current-tabset-function 'centaur-tabs-buffer-tabs)
       (eq (current-buffer) (window-buffer (selected-window)))
       (let ((bl (centaur-tabs-tab-values (centaur-tabs-current-tabset)))
	     (b  (current-buffer))
	     found sibling)
	 (while (and bl (not found))
	   (if (eq b (car bl))
	       (setq found t)
	     (setq sibling (car bl)))
	   (setq bl (cdr bl)))
	 (when (and (setq sibling (or (car bl) sibling))
		    (buffer-live-p sibling))
	   ;; Move sibling buffer in front of the buffer list.
	   (save-current-buffer
	     (switch-to-buffer sibling))))))

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
  (when (eq centaur-tabs-set-bar 'over)
    (set-face-attribute 'centaur-tabs-selected nil :overline (face-background 'centaur-tabs-active-bar-face))
    (set-face-attribute 'centaur-tabs-selected-modified nil :overline (face-background 'centaur-tabs-active-bar-face))
    (set-face-attribute 'centaur-tabs-unselected nil :overline nil)
    (set-face-attribute 'centaur-tabs-unselected-modified nil :overline nil))
  (add-hook 'after-save-hook #'centaur-tabs-saved-marker-update)
  (add-hook 'first-change-hook #'centaur-tabs-saved-marker-update)
  (add-hook 'after-change-functions #'centaur-tabs-after-modifying-buffer)
  (add-hook 'kill-buffer-hook #'centaur-tabs-buffer-track-killed)
  (add-hook 'buffer-list-update-hook #'centaur-tabs-buffer-update-groups)
  (centaur-tabs-force-update))

(defun centaur-tabs-buffer-quit ()
  "Quit tab bar buffer.
Run as `centaur-tabs-quit-hook'."
  (setq centaur-tabs--buffers nil
	centaur-tabs-current-tabset-function nil
	centaur-tabs-tab-label-function nil
	centaur-tabs-select-tab-function nil
	)
  (remove-hook 'after-save-hook 'centaur-tabs-saved-marker-update)
  (remove-hook 'first-change-hook 'centaur-tabs-saved-marker-update)
  (remove-hook 'after-change-functions 'centaur-tabs-after-modifying-buffer)
  (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed)
  (remove-hook 'buffer-list-update-hook 'centaur-tabs-buffer-update-groups))

(add-hook 'centaur-tabs-init-hook #'centaur-tabs-buffer-init)
(add-hook 'centaur-tabs-quit-hook #'centaur-tabs-buffer-quit)

;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun centaur-tabs-switch-group (&optional groupname)
  "Switch tab groups using ido.  GROUPNAME can optionaly be provided."
  (interactive)
  (let* ((tab-buffer-list (cl-mapcar
			   #'(lambda (b)
			       (with-current-buffer b
				 (list (current-buffer)
				       (buffer-name)
				       (funcall centaur-tabs-buffer-groups-function) )))
			   (funcall centaur-tabs-buffer-list-function)))
	 (groups (centaur-tabs-get-groups))
	 (group-name (or groupname (ido-completing-read "Groups: " groups))) )
    (catch 'done
      (mapc
       #'(lambda (group)
	   (when (member group-name (car (cdr (cdr group))))
	     (set-window-parameter nil 'tabset (centaur-tabs-get-tabset group-name))
	     (centaur-tabs-saved-marker-update)
	     (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list) )))

(defun centaur-tabs-select-end-tab ()
  "Select end tab of current tabset."
  (interactive)
  (centaur-tabs-select-beg-tab t))

(defun centaur-tabs-select-beg-tab (&optional backward)
  "Select beginning tab of current tabs.
If BACKWARD is non-nil, move backward, otherwise move forward.
TYPE is default option."
  (interactive)
  (let* ((tabset (centaur-tabs-current-tabset t))
	 (ttabset (centaur-tabs-get-tabsets-tabset))
	 (_cycle (if (and (eq centaur-tabs-cycle-scope 'groups)
			  (not (cdr (centaur-tabs-tabs ttabset))))
		     'tabs
		   centaur-tabs-cycle-scope))
	 _selected tab)
    (when tabset
      (setq tabset (centaur-tabs-tabs tabset)
	    tab (car (if backward (last tabset) tabset)))
      (centaur-tabs-buffer-select-tab tab))))

(defun centaur-tabs-backward-tab-other-window (&optional reversed)
  "Move to left tab in other window.
Optional argument REVERSED default is move backward, if reversed is non-nil move forward."
  (interactive)
  (other-window 1)
  (if reversed
      (centaur-tabs-forward-tab)
    (centaur-tabs-backward-tab))
  (other-window -1))

(defun centaur-tabs-forward-tab-other-window ()
  "Move to right tab in other window."
  (interactive)
  (centaur-tabs-backward-tab-other-window t))

(defun centaur-tabs-move-current-tab-to-right ()
  "Move current tab one place right, unless it's already the rightmost."
  (interactive)
  (let* ((bufset (centaur-tabs-current-tabset t))
	 (old-bufs (centaur-tabs-tabs bufset))
	 (new-bufs (list))
	 the-buffer)
    (while (and
	    old-bufs
	    (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
      (push (car old-bufs) new-bufs)
      (setq old-bufs (cdr old-bufs)))
    (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
	(progn
	  (setq the-buffer (car old-bufs))
	  (setq old-bufs (cdr old-bufs))
	  (if old-bufs ; if this is false, then the current tab is the rightmost
	      (push (car old-bufs) new-bufs))
	  (push the-buffer new-bufs)) ; this is the tab that was to be moved
      (error "Error: current buffer's name was not found in Centaur-Tabs's buffer list"))
    (setq new-bufs (reverse new-bufs))
    (setq new-bufs (append new-bufs (cdr old-bufs)))
    (set bufset new-bufs)
    (centaur-tabs-set-template bufset nil)
    (centaur-tabs-display-update)))

(defun centaur-tabs-move-current-tab-to-left ()
  "Move current tab one place left, unless it's already the leftmost."
  (interactive)
  (let* ((bufset (centaur-tabs-current-tabset t))
	 (old-bufs (centaur-tabs-tabs bufset))
	 (first-buf (car old-bufs))
	 (new-bufs (list))
	 not-yet-this-buf)
    (if (string= (buffer-name) (format "%s" (car first-buf)))
	old-bufs                     ; the current tab is the leftmost
      (setq not-yet-this-buf first-buf)
      (setq old-bufs (cdr old-bufs))
      (while (and
	      old-bufs
	      (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
	(push not-yet-this-buf new-bufs)
	(setq not-yet-this-buf (car old-bufs))
	(setq old-bufs (cdr old-bufs)))
      (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
	  (progn
	    (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
	    (push not-yet-this-buf new-bufs)
	    (setq new-bufs (reverse new-bufs))
	    (setq new-bufs (append new-bufs (cdr old-bufs))))
	(error "Error: current buffer's name was not found in Centaur-Tabs's buffer list"))
      (set bufset new-bufs)
      (centaur-tabs-set-template bufset nil)
      (centaur-tabs-display-update))))

(defmacro centaur-tabs-kill-buffer-match-rule (match-rule)
  "If buffer match MATCH-RULE,  kill it."
  `(save-excursion
     (mapc #'(lambda (buffer)
	       (with-current-buffer buffer
		 (when (string-equal current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t))))
		   (when (funcall ,match-rule buffer)
		     (kill-buffer buffer))
		   )))
	   (buffer-list))))

(defun centaur-tabs-kill-all-buffers-in-current-group ()
  "Kill all buffers in current group."
  (interactive)
  (let* ((current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t)))))
    ;; Kill all buffers in current group.
    (centaur-tabs-kill-buffer-match-rule
     (lambda (_buffer) t))
    ;; Switch to next group.
    (centaur-tabs-forward-group)
    ))

(defun centaur-tabs-kill-other-buffers-in-current-group ()
  "Kill all buffers except current buffer in current group."
  (interactive)
  (let* ((current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t))))
	 (currentbuffer (current-buffer)))
    ;; Kill all buffers in current group.
    (centaur-tabs-kill-buffer-match-rule
     (lambda (buffer) (not (equal buffer currentbuffer))))
    ))

(defun centaur-tabs-kill-match-buffers-in-current-group ()
  "Kill all buffers match extension in current group."
  (interactive)
  (let* ((current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t))))
	 (extension-names (centaur-tabs-get-extensions))
	 match-extension)
    ;; Read extension need to kill.
    (setq match-extension (ido-completing-read "Kill buffers suffix with: " extension-names))
    ;; Kill all buffers match extension in current group.
    (centaur-tabs-kill-buffer-match-rule
     (lambda (buffer)
       (let ((filename (buffer-file-name buffer)))
	 (and filename (string-equal (file-name-extension filename) match-extension))
	 )))
    ;; Switch to next group if last file killed.
    (when (equal (length extension-names) 1)
      (centaur-tabs-forward-group))
    ))

(defun centaur-tabs-keep-match-buffers-in-current-group ()
  "Keep all buffers match extension in current group."
  (interactive)
  (let* ((current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t))))
	 (extension-names (centaur-tabs-get-extensions))
	 match-extension)
    ;; Read extension need to kill.
    (setq match-extension (ido-completing-read "Just keep buffers suffix with: " extension-names))
    ;; Kill all buffers match extension in current group.
    (centaur-tabs-kill-buffer-match-rule
     (lambda (buffer)
       (let ((filename (buffer-file-name buffer)))
	 (and filename (not (string-equal (file-name-extension filename) match-extension)))
	 )))
    ;; Switch to next group if last file killed.
    (when (equal (length extension-names) 1)
      (centaur-tabs-forward-group))
    ))

(defun centaur-tabs-select-visible-nth-tab (tab-index)
  "Select visible tab with TAB-INDEX'.
Example, when `tab-index' is 1, this function will select the leftmost label in
the visible area,  instead of the first label in the current group.
If `tab-index' more than length of visible tabs, selet the last tab.

If `tab-index' is 0, select last tab."
  (let ((visible-tabs (centaur-tabs-view centaur-tabs-current-tabset)))
    (switch-to-buffer
     (car
      (if (or (equal tab-index 0)
	      (> tab-index (length visible-tabs)))
	  (car (last visible-tabs))
	(nth (- tab-index 1) visible-tabs))))))

(defun centaur-tabs-select-visible-tab ()
  "Bind this function with number keystroke, such as s-1, s-2, s-3 ... etc.

This function automatically recognizes the number at the end of the keystroke
and switches to the tab of the corresponding index.

Note that this function switches to the visible range,
not the actual logical index position of the current group."
  (interactive)
  (let* ((event last-command-event)
	 (key (make-vector 1 event))
	 (key-desc (key-description key)))
    (centaur-tabs-select-visible-nth-tab
     (string-to-number (car (last (split-string key-desc "-")))))))

;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun centaur-tabs-get-groups ()
  "Refresh tabs groups."
  (set centaur-tabs-tabsets-tabset (centaur-tabs-map-tabsets 'centaur-tabs-selected-tab))
  (cl-mapcar #'(lambda (group)
		 (format "%s" (cdr group)))
	     (centaur-tabs-tabs centaur-tabs-tabsets-tabset)))

(defun centaur-tabs-get-extensions ()
  "Get file extension of tabs."
  (set centaur-tabs-tabsets-tabset (centaur-tabs-map-tabsets 'centaur-tabs-selected-tab))
  (let (extension-names)
    (mapc #'(lambda (buffer)
	      (with-current-buffer buffer
		(when (string-equal 'current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t))))
		  (when (buffer-file-name buffer)
		    (add-to-list 'extension-names (file-name-extension (buffer-file-name buffer))))
		  )))
	  (buffer-list))
    extension-names))

;;;;;;;;;;;;;;;;;;;;;;; Default configurations ;;;;;;;;;;;;;;;;;;;;;;;

(mapc (lambda (hook)
	(add-hook hook (lambda ()
			 (setq-local header-line-format nil))))
      centaur-tabs-hide-tabs-hooks)

;; Rules to control buffer's group rules.
(defvar centaur-tabs-groups-hash (make-hash-table :test 'equal))
(defvar centaur-tabs-hide-hash (make-hash-table :test 'equal))

(defun centaur-tabs-project-name ()
  "Get project name for tabs."
  (let ((project-name (cdr (project-current))))
    (if project-name
	(format "Project: %s" (expand-file-name project-name))
      centaur-tabs-common-group-name)))

(defun centaur-tabs-get-group-name (buf)
  "Get group name of buffer BUF."
  (let ((group-name (gethash buf centaur-tabs-groups-hash)))
    ;; Return group name cache if it exists for improve performance.
    (if group-name
	group-name
      ;; Otherwise try get group name with `project-current'.
      ;; `project-current' is very slow, it will slow down Emacs if you call it when switch buffer.
      (with-current-buffer buf
	(let ((project-name (centaur-tabs-project-name)))
	  (puthash buf project-name centaur-tabs-groups-hash)
	  project-name)))))

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
	 (memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode
			    )))
     "Emacs")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

(defun centaur-tabs-group-buffer-groups ()
  "Use centaur-tabs's own buffer grouping function."
  (interactive)
  (setq centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups)
  (centaur-tabs-force-update))

(defun centaur-tabs-group-by-projectile-project ()
  "Group by projectile project."
  (interactive)
  (setq centaur-tabs-buffer-groups-function 'centaur-tabs-projectile-buffer-groups)
  (centaur-tabs-force-update))

;; Projectile integration. Taken from tabbar-ruler
(defvar centaur-tabs-projectile-buffer-group-calc nil
  "Set buffer groups for projectile.
Should be buffer local and speed up calculation of buffer groups.")

(defun centaur-tabs-projectile-buffer-groups ()
  "Return the list of group names BUFFER belongs to."
  (if centaur-tabs-projectile-buffer-group-calc
      (symbol-value 'centaur-tabs-projectile-buffer-group-calc)
    (set (make-local-variable 'centaur-tabs-projectile-buffer-group-calc)

	 (cond
	  ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
	  ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
	  ((condition-case _err
		    (projectile-project-root)
		  (error nil)) (list (projectile-project-name)))
	  ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode
					      c++-mode javascript-mode js-mode
					      js2-mode makefile-mode
					      lua-mode vala-mode)) '("Coding"))
	  ((memq major-mode '(nxhtml-mode html-mode
					  mhtml-mode css-mode)) '("HTML"))
	  ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
	  ((memq major-mode '(dired-mode)) '("Dir"))
	  (t '("Other"))))
    (symbol-value 'centaur-tabs-projectile-buffer-group-calc)))

;; Show groups instead of tabs
(defun centaur-tabs-toggle-groups ()
  "Show group names on the tabs instead of buffer names."
  (interactive)
  (centaur-tabs-buffer-show-groups (not centaur-tabs--buffer-show-groups))
  (centaur-tabs-display-update))

;; Helm source for switching group in helm.

(defun centaur-tabs-build-helm-source ()
  "Display a list of current buffer groups in Helm."
  (interactive)
  (setq helm-source-centaur-tabs-group
	(when (featurep 'helm)
	  (require 'helm)
	  (helm-build-sync-source "Centaur-Tabs Group"
				  :candidates #'centaur-tabs-get-groups
				  :action '(("Switch to group" . centaur-tabs-switch-group))))))

;; Ivy source for switching group in ivy.

;;;###autoload
(defun centaur-tabs-counsel-switch-group ()
  "Display a list of current buffer groups using Counsel."
  (interactive)
  (when (featurep 'ivy)
    (require 'ivy)
    (ivy-read
     "Centaur Tabs Groups:"
     (centaur-tabs-get-groups)
     :action #'centaur-tabs-switch-group
     :caller 'centaur-tabs-counsel-switch-group)))

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*Help" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	  (not (file-name-extension name)))
     )))

(defun centaur-tabs-hide-tab-cached (buf)
  "Cached vesion of `centaur-tabs-hide-tab' to improve performance.
Operates over buffer BUF"
  (let ((hide (gethash buf centaur-tabs-hide-hash 'not-found)))
    (when (eq hide 'not-found)
      (setq hide (funcall centaur-tabs-hide-tab-function buf))
      (puthash buf hide centaur-tabs-hide-hash))
    hide))

(defun centaur-tabs-remove-nth-element (nth list)
  "Remove NTH element from LIST."
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun centaur-tabs-insert-after (list aft-el el)
  "Insert EL after AFT-EL in LIST."
  (push el (cdr (member aft-el list)))
  list)

(defun centaur-tabs-insert-before (list bef-el el)
  "Insert EL before BEF-EL in LIST."
  (nreverse (centaur-tabs-insert-after (nreverse list) bef-el el)))

(defun centaur-tabs-adjust-buffer-order ()
  "Put the two buffers switched to the adjacent position after current buffer changed."
  ;; Don't trigger by centaur-tabs command, it's annoying.
  ;; This feature should be trigger by search plugins, such as ibuffer, helm or ivy.
  (unless (or (string-prefix-p "centaur-tabs" (format "%s" this-command))
	      (string-prefix-p "mouse-drag-header-line" (format "%s" this-command))
	      (string-prefix-p "(lambda (event) (interactive e)" (format "%s" this-command)))
    ;; Just continue when the buffer has changed.
    (when (and centaur-tabs-adjust-buffer-order
	       (not (eq (current-buffer) centaur-tabs-last-focused-buffer))
	       (not (minibufferp)))
      (let* ((current (current-buffer))
	     (previous centaur-tabs-last-focused-buffer)
	     (current-group (cl-first (funcall centaur-tabs-buffer-groups-function))))
	;; Record the last focused buffer.
	(setq centaur-tabs-last-focused-buffer current)

	;; Just continue if two buffers are in the same group.
	(when (string= current-group centaur-tabs-last-focused-buffer-group)
	  (let* ((bufset (centaur-tabs-get-tabset current-group))
		 (current-group-tabs (centaur-tabs-tabs bufset))
		 (current-group-buffers (cl-mapcar 'car current-group-tabs))
		 (current-buffer-index (cl-position current current-group-buffers))
		 (previous-buffer-index (cl-position previous current-group-buffers)))

	    ;; If the tabs are not adjacent, swap their positions.
	    (when (and current-buffer-index
		       previous-buffer-index
		       (> (abs (- current-buffer-index previous-buffer-index)) 1))
	      (let* ((copy-group-tabs (cl-copy-list current-group-tabs))
		     (previous-tab (nth previous-buffer-index copy-group-tabs))
		     (current-tab (nth current-buffer-index copy-group-tabs))
		     (base-group-tabs (centaur-tabs-remove-nth-element current-buffer-index copy-group-tabs))
		     new-group-tabs)
		(cond
		 ((eq centaur-tabs-adjust-buffer-order 'left)
		  (setq new-group-tabs (centaur-tabs-insert-before base-group-tabs previous-tab current-tab)))
		 ((eq centaur-tabs-adjust-buffer-order 'right)
		  (setq new-group-tabs (centaur-tabs-insert-after  base-group-tabs previous-tab current-tab)))
		 (t
		  (if (> current-buffer-index previous-buffer-index)
		      (setq new-group-tabs (centaur-tabs-insert-after  base-group-tabs previous-tab current-tab))
		    (setq new-group-tabs (centaur-tabs-insert-before  base-group-tabs previous-tab current-tab)))))
		(set bufset new-group-tabs)
		(centaur-tabs-set-template bufset nil)
		(centaur-tabs-display-update)
		))))

	;; Update the group name of the last accessed tab.
	(setq centaur-tabs-last-focused-buffer-group current-group)
	))))

(defun centaur-tabs-enable-buffer-reordering ()
  "Enable the buffer adjusting functionality."
  (add-hook 'post-command-hook centaur-tabs-adjust-buffer-order-function))


(defun centaur-tabs--groups-menu-list ()
  "Make the menu of the tabs groups."
  (cons "Centaur tabs groups menu"
	(mapcar
	 (lambda (g)
	   (cons g g))
	 (sort (centaur-tabs-get-groups) 'string<))))

(defun centaur-tabs--groups-menu ()
  "Show a popup menu with the centaur tabs groups."
  (interactive)
  (let*
      ((sorted-groups (centaur-tabs--groups-menu-list))
       (group (x-popup-menu t (list "Centaur tabs groups menu" sorted-groups))))
    (centaur-tabs-switch-group group)))


(provide 'centaur-tabs)

;;; centaur-tabs.el ends here
