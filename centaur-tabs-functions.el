;;; centaur-tabs-functions.el --- centaur-tabs logic components -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Emmanuel Bustos

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;; This file contains functions that control the logic of centaur-tabs

;;; Code:
;;; Require
(require 'cl-lib)
(require 'seq)
(require 'centaur-tabs-elements)

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
  "Set hooks for buffers in which it isn't desired to have tabs."
  :type '(repeat symbol)
  :group 'centaur-tabs)

(defvar centaur-tabs-hide-tab-function 'centaur-tabs-hide-tab
  "Function to hide tabs.
This function filters tabs.  The tab will hide if this function returns t.")

(defvar centaur-tabs-current-tabset-function nil
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar.")

(defvar centaur-tabs-tab-label-function nil
  "Function that obtains a tab label displayed on the tab bar.
The function is passed a tab and should return a string.")

(defvar centaur-tabs-select-tab-function nil
  "Function that selects a tab.
The function is passed a tab, and makes it the
selected tab.")

(defvar centaur-tabs-buffer-list-function 'centaur-tabs-buffer-list
  "Function that returns the list of buffers to show in the tab line.
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

(defcustom centaur-tabs-enable-key-bindings nil
  "Enable a selection of default key bindings for centaur-tabs."
  :group 'centaur-tabs
  :type 'boolean)

(defun centaur-tabs-headline-match ()
  "Make headline use centaur-tabs-default-face."
  (set-face-attribute centaur-tabs-display-line nil :background (face-background 'centaur-tabs-unselected)
		      :box nil
		      :overline nil
		      :underline nil))

;; Change the font and height for all tab faces
(defun centaur-tabs-change-fonts (family height)
  "Change the fonts of all the tabs.
FAMILY is the font family and HEIGHT is the font height."
  (dolist (centaur-face '(centaur-tabs-selected
			  centaur-tabs-selected-modified
			  centaur-tabs-unselected
			  centaur-tabs-unselected-modified))
    (set-face-attribute centaur-face nil :family family :height height)))

;;; Tabs Redisplay function
;;
(eval-and-compile
  (defalias 'centaur-tabs-display-update
    (if (fboundp 'force-window-update)
	#'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update)))

;;; Name truncation
;;
;; Copied from s.el
(defun centaur-tabs-truncate-string (len s &optional ellipsis)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters
long.

When not specified, ELLIPSIS defaults to ‘...’."
  (declare (pure t) (side-effect-free t))
  (unless ellipsis
    (setq ellipsis (if (char-displayable-p ?…) "…" "...")))
  (if (> (length s) len)
      (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
    (concat s (make-string (- len (length s)) ? ))))

;;; Keymaps
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
    (define-key km (kbd "C-5")     'centaur-tabs-extract-window-to-new-frame)
    (define-key km [(control k)]   'centaur-tabs-kill-other-buffers-in-current-group)
    (define-key km [(control o)]   'centaur-tabs-open-in-external-application)
    (define-key km [(control d)]   'centaur-tabs-open-directory-in-external-application)
    km)
  "The key bindings provided in Centaur-Tabs mode.")

(defvar centaur-tabs-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Optional keybord bindings
    (when centaur-tabs-enable-key-bindings
      (define-key map centaur-tabs-prefix-key centaur-tabs-prefix-map))
    ;;; Use mouse wheel to switch between buffers of same group
    (define-key map (vector centaur-tabs-display-line 'mouse-5   ) 'centaur-tabs-forward )
    (define-key map (vector centaur-tabs-display-line 'mouse-4   ) 'centaur-tabs-backward)
    (define-key map (vector centaur-tabs-display-line 'wheel-down) 'centaur-tabs-forward )
    (define-key map (vector centaur-tabs-display-line 'wheel-up  ) 'centaur-tabs-backward)

    ;;; Use right click to show the rest of groups
    (define-key map (vector centaur-tabs-display-line 'mouse-3) 'centaur-tabs--tab-menu )

    ;;; Use double click to maximize window
    (define-key map (vector centaur-tabs-display-line 'double-mouse-1) 'delete-other-windows)

    map)
  "Keymap to use in  Centaur-Tabs mode.")

(defvar centaur-tabs-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector centaur-tabs-display-line 'mouse-1) 'centaur-tabs-do-close)
    (define-key map (vector centaur-tabs-display-line 'mouse-2) 'centaur-tabs-do-close)
    map)
  "Keymap used for setting mouse events for close button.")

(defvar centaur-tabs-backward-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector centaur-tabs-display-line 'mouse-1) 'centaur-tabs-backward--button)
    (define-key map (vector centaur-tabs-display-line 'mouse-3) 'centaur-tabs--groups-menu)
    (define-key map (vector centaur-tabs-display-line 'C-mouse-1) 'centaur-tabs-move-current-tab-to-left--button)
    map)
  "Keymap used for setting mouse events for backward tab button.")

(defvar centaur-tabs-forward-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector centaur-tabs-display-line 'mouse-1) 'centaur-tabs-forward--button)
    (define-key map (vector centaur-tabs-display-line 'mouse-3) 'centaur-tabs--groups-menu)
    (define-key map (vector centaur-tabs-display-line 'C-mouse-1) 'centaur-tabs-move-current-tab-to-right--button)
    map)
  "Keymap used for setting mouse events for forward tab button.")

(defvar centaur-tabs-down-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector centaur-tabs-display-line 'mouse-1) 'centaur-tabs--groups-menu)
    (define-key map (vector centaur-tabs-display-line 'mouse-3) 'centaur-tabs--groups-menu)
    map)
  "Keymap used for setting mouse events for down tab button.")

(defvar centaur-tabs-default-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector centaur-tabs-display-line 'mouse-1) 'centaur-tabs-do-select)
    (define-key map (vector centaur-tabs-display-line 'mouse-2) 'centaur-tabs-do-close)
    map)
  "Keymap used for setting mouse events for a tab.")

(defvar centaur-tabs-new-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector centaur-tabs-display-line 'mouse-1) 'centaur-tabs-new-tab--button)
    map)
  "Keymap used for setting mouse events for new tab button.")

;;; Events and event functions
;;
(defun centaur-tabs-buffer-close-tab (tab)
  "Function for closing TAB."
  (let ((buffer (centaur-tabs-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))
    (centaur-tabs-display-update)))

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
  (let ((window (posn-window (event-start event))))
    (with-selected-window window
      (select-window window)
      (let ((foreground-buffer-name (buffer-name)))
	(centaur-tabs-buffer-select-tab `,(centaur-tabs-get-tab-from-event event))

	(let* ((buffer             (window-buffer window))
	       (target-buffer-name (buffer-name))
	       (same-target-check  (string-equal foreground-buffer-name target-buffer-name))
	       (window-num         (- (length (get-buffer-window-list buffer))
				      (if same-target-check 0 1))))
          (if (> window-num 1)
              (delete-window window)
            (centaur-tabs-buffer-close-tab `,(centaur-tabs-get-tab-from-event event))))))))

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

(defun centaur-tabs-new-tab--button (event)
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (centaur-tabs--create-new-tab))

(defun centaur-tabs-move-current-tab-to-left--button (evt)
  "Same as centaur-tabs-move-current-tab-to-left, but ensuring the tab will remain visible.  The active window will the the EVT source."
  (interactive "e")
  (centaur-tabs-move-current-tab-to-left)
  (centaur-tabs--button-ensure-selected-tab-is-visible evt))


(defun centaur-tabs-move-current-tab-to-right--button (evt)
  "Same as centaur-tabs-move-current-tab-to-right, but ensuring the tab will remain visible.  The active window will the the EVT source."
  (interactive "e")
  (centaur-tabs-move-current-tab-to-right)
  (centaur-tabs--button-ensure-selected-tab-is-visible evt))

(defun centaur-tabs--button-ensure-selected-tab-is-visible (evt)
  "This is a nasty trick to make the current tab visible, since centaur-tabs--track-selected or centaur-tabs-auto-scroll-flag seems not to work.  EVT is used to change the active window."
  ;;; This works if the tab has not reached the last position
  (centaur-tabs-forward--button evt)
  (centaur-tabs-backward--button evt)
  ;;; Just in case the tab has the tab reached the last position
  (centaur-tabs-backward--button evt)
  (centaur-tabs-forward--button evt))


;;; Tab and tab sets
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
That is, a `centaur-tabs-display-line-format' template, or nil if the cache is
empty."
  (get tabset 'template))

(defsubst centaur-tabs-set-template (tabset template)
  "Set the cached visual representation of TABSET to TEMPLATE.
TEMPLATE must be a valid `centaur-tabs-display-line-format' template, or nil to
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
  (let* (last (tabs (centaur-tabs-tabs tabset)))
    (while (and tabs (not (eq tab (car tabs))))
      (setq last (car tabs)
	    tabs (cdr tabs)))
    (and tabs (if before last (nth 1 tabs)))))

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

;; Functions for modification hooks and advices
(defun centaur-tabs-on-saving-buffer ()
  "Function to be run after the buffer is saved."
  (centaur-tabs-set-template centaur-tabs-current-tabset nil)
  (centaur-tabs-display-update))
(defun centaur-tabs-on-modifying-buffer ()
  "Function to be run after the buffer is first changed."
  (set-buffer-modified-p (buffer-modified-p))
  (centaur-tabs-set-template centaur-tabs-current-tabset nil)
  (centaur-tabs-display-update))
(defun centaur-tabs-after-modifying-buffer (&rest _)
  "Function to be run after the buffer is changed.
BEGIN, END and LENGTH are just standard arguments for after-changes-function
hooked functions"
  (set-buffer-modified-p (buffer-modified-p))
  (centaur-tabs-set-template centaur-tabs-current-tabset nil)
  (centaur-tabs-display-update))

;;; Tabs display
;;
(defsubst centaur-tabs-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `centaur-tabs-display-line-format'
template element.
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

     ;; left margin
     (if centaur-tabs-left-edge-margin
	 (propertize
	  centaur-tabs-left-edge-margin
	  'face face
	  'centaur-tabs-tab tab
	  'pointer centaur-tabs-mouse-pointer
	  'local-map centaur-tabs-default-map))

     ;; left close button
     (if centaur-tabs-set-left-close-button
	 (propertize
	  centaur-tabs-close-button
	  'face (if selected-p
		    'centaur-tabs-close-selected
		  'centaur-tabs-close-unselected)
	  'pointer centaur-tabs-mouse-pointer
	  'help-echo "Close buffer"
	  'centaur-tabs-tab tab
	  'mouse-face 'centaur-tabs-close-mouse-face
	  'local-map centaur-tabs-close-map))

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

     ;; right margin
     (if centaur-tabs-right-edge-margin
	 (propertize
	  centaur-tabs-right-edge-margin
	  'face face
	  'centaur-tabs-tab tab
	  'pointer centaur-tabs-mouse-pointer
	  'local-map centaur-tabs-default-map))

     (centaur-tabs-separator-render centaur-tabs-style-right face))))

(defsubst centaur-tabs-button-tab (button)
  "Return the display representation of button BUTTON.
That is, a propertized string used as an `centaur-tabs-display-line-format'
template element."
  (let* ((face 'centaur-tabs-unselected))
    (concat
     (centaur-tabs-separator-render centaur-tabs-style-left face)
     (propertize
      button
      'face face
      'mouse-face 'highlight)
     (centaur-tabs-separator-render centaur-tabs-style-right face))))

(defun centaur-tabs-line-format (tabset)
  "Return the `centaur-tabs-display-line-format' value to display TABSET."
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
      (let (buffer-list-update-hook)
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
	      (setq elts (cdr elts))))))
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
                  'pointer 'arrow)
      (centaur-tabs-line-format--new-button)))
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

(defun centaur-tabs-line-format--new-button ()
  "Return the new-tab button fragment at the right end of the
header line."
  (if centaur-tabs-show-new-tab-button
      (concat
       (propertize (centaur-tabs-button-tab centaur-tabs-new-tab-text)
                   'local-map centaur-tabs-new-tab-map
                   'help-echo "Create new tab")
    "")))

(defun centaur-tabs-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window where
`centaur-tabs-hide-tab-function' return t."
  (cond
   ((centaur-tabs-hide-tab-cached (current-buffer))
    ;; Don't show the tab bar.
    (set centaur-tabs-display-line-format nil))
   ((centaur-tabs-current-tabset t)
    ;; When available, use a cached tab bar value, else recompute it.
    (or (centaur-tabs-template centaur-tabs-current-tabset)
	(centaur-tabs-line-format centaur-tabs-current-tabset)))))

(defconst centaur-tabs-header-line-format '(:eval (centaur-tabs-line))
  "The tab bar header line format.")

;;; Cyclic navigation through tabs
;;
(defun centaur-tabs-cycle (&optional backward)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `centaur-tabs-cycle-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead."
  (let* ((tabset (centaur-tabs-current-tabset t))
	 (ttabset (centaur-tabs-get-tabsets-tabset))
	 ;; If navigation through groups is requested, and there is
	 ;; only one group, navigate through visible tabs.
	 (cycle (if (and (eq centaur-tabs-cycle-scope 'groups)
			 (not (cdr (centaur-tabs-tabs ttabset))))
		    'tabs
		  centaur-tabs-cycle-scope))
	 selected tab)
    (when tabset
      (setq selected (centaur-tabs-selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq cycle 'tabs)
	(setq tab (centaur-tabs-tab-next tabset selected backward))
	;; When there is no tab after/before the selected one, cycle
	;; to the first/last visible tab.
	(unless tab
	  (setq tabset (centaur-tabs-tabs tabset)
		tab (car (if backward (last tabset) tabset))))
	)
       ;; Cycle through tab groups only.
       ((eq cycle 'groups)
	(setq tab (centaur-tabs-tab-next ttabset selected backward))
	;; When there is no group after/before the selected one, cycle
	;; to the first/last available group.
	(unless tab
	  (setq tabset (centaur-tabs-tabs ttabset)
		tab (car (if backward (last tabset) tabset))))
	)
       (t
	;; Cycle through visible tabs then tab groups.
	(setq tab (centaur-tabs-tab-next tabset selected backward))
	;; When there is no visible tab after/before the selected one,
	;; cycle to the next/previous available group.
	(unless tab
	  (setq tab (centaur-tabs-tab-next ttabset selected backward))
	  ;; When there is no next/previous group, cycle to the
	  ;; first/last available group.
	  (unless tab
	    (setq tabset (centaur-tabs-tabs ttabset)
		  tab (car (if backward (last tabset) tabset))))
	  ;; Select the first/last visible tab of the new group.
	  (setq tabset (centaur-tabs-tabs (centaur-tabs-tab-tabset tab))
		tab (car (if backward (last tabset) tabset))))
	))
      (centaur-tabs-buffer-select-tab tab))))

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
      (setq centaur-tabs--buffers bl)))
  ;; Return the first group the current buffer belongs to.
  (car (nth 2 (assq (current-buffer) centaur-tabs--buffers))))

;;; Tab bar callbacks
;;

(defsubst centaur-tabs-buffer-show-groups (flag)
  "Set display of tabs for groups of buffers to FLAG."
  (setq centaur-tabs--buffer-show-groups flag))

(defun centaur-tabs-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((tabset (centaur-tabs-get-tabset (centaur-tabs-buffer-update-groups))))
    (centaur-tabs-select-tab-value (current-buffer) tabset)
    (when centaur-tabs--buffer-show-groups
      (setq tabset (centaur-tabs-get-tabsets-tabset))
      (centaur-tabs-select-tab-value (current-buffer) tabset))
    tabset))

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
  (and (eq (eval centaur-tabs-display-line-format) centaur-tabs-header-line-format)
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

;; Buffer reordering
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
	      (string-prefix-p "mouse-drag-tab-line" (format "%s" this-command))
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
	(setq centaur-tabs-last-focused-buffer-group current-group)))))

(defun centaur-tabs-adjust-buffer-order-alphabetically ()
  "Order tabs in group alphabetically."
  ;; Don't trigger by centaur-tabs command, it's annoying.
  (unless (or (string-prefix-p "centaur-tabs" (format "%s" this-command))
	      (string-prefix-p "mouse-drag-header-line" (format "%s" this-command))
	      (string-prefix-p "mouse-drag-tab-line" (format "%s" this-command))
	      (string-prefix-p "(lambda (event) (interactive e)" (format "%s" this-command)))
    ;; Just continue when the buffer has changed.
    (when (and centaur-tabs-adjust-buffer-order
	       (not (eq (current-buffer) centaur-tabs-last-focused-buffer)) ;;???
	       (not (minibufferp)))
      (let* ((current (current-buffer))
	     (current-group (cl-first (funcall centaur-tabs-buffer-groups-function))))
	(setq centaur-tabs-last-focused-buffer current)
	;; Just continue if two buffers are in the same group.
	(when (string= current-group centaur-tabs-last-focused-buffer-group)
	  (let* ((bufset (centaur-tabs-get-tabset current-group))
		 (current-group-tabs (centaur-tabs-tabs bufset)))
	    (setq new-group-tabs (sort current-group-tabs
				       (lambda (x y)
					 (string< (buffer-name (car x)) (buffer-name (car y))))))
	    (set bufset new-group-tabs)
	    (centaur-tabs-set-template bufset nil)
	    (centaur-tabs-display-update)))
	(setq centaur-tabs-last-focused-buffer-group current-group)))))

(defun centaur-tabs-enable-buffer-reordering ()
  "Enable the buffer reordering functionality, according to buffer usage."
  (add-hook 'post-command-hook centaur-tabs-adjust-buffer-order-function))

(defun centaur-tabs-enable-buffer-alphabetical-reordering ()
  "Enable the buffer alphabetical reordering functionality."
  (setq centaur-tabs-adjust-buffer-order-function 'centaur-tabs-adjust-buffer-order-alphabetically)
  (add-hook 'post-command-hook centaur-tabs-adjust-buffer-order-function))

;;; Buffer grouping and tab hiding
;;
(defun centaur-tabs-project-name ()
  "Get project name for tabs."
  (let ((project-name (cdr (project-current))))
    (if project-name
	(format "Project: %s" (expand-file-name project-name))
      centaur-tabs-common-group-name)))

;; Rules to control buffer's group rules.
(defvar centaur-tabs-groups-hash (make-hash-table :test 'equal))
(defvar centaur-tabs-hide-hash (make-hash-table :test 'equal))

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

(defun centaur-tabs--create-new-empty-buffer ()
  "Open a New empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "New empty")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun centaur-tabs--create-new-tab ()
  "Create a context-aware new tab."
  (interactive)
  (cond
   ((eq major-mode 'eshell-mode)
    (eshell t))
   ((eq major-mode 'vterm-mode)
    (vterm t))
   ((eq major-mode 'term-mode)
    (ansi-term "/bin/bash"))
   ((derived-mode-p 'eww-mode)
    (let ((current-prefix-arg 4))
      (call-interactively #'eww)))
   (t
    (centaur-tabs--create-new-empty-buffer))))

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*LSP" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
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

(defcustom centaur-tabs-enable-ido-completion t
  "Non-nil means use `ido-completing-read' for completing reads else `completing-read'."
  :group 'centaur-tabs
  :type 'boolean)

(defun centaur-tabs-completing-read (prompt choices)
  "Prompt user with PROMPT to select from CHOICES using a completing read.
Refer to  the variable `centaur-tabs-enable-ido-completion'."
  (interactive)
  (if centaur-tabs-enable-ido-completion
      (ido-completing-read prompt choices)
    (completing-read prompt choices)))

;;;;;;;;;;;;;;;;;;;;;;; Default configurations ;;;;;;;;;;;;;;;;;;;;;;;

(mapc (lambda (hook)
	(add-hook hook (lambda ()
			 (if (boundp 'tab-line-format)
			     (setq-local tab-line-format nil)
			   (setq-local header-line-format nil))
			 )))
      centaur-tabs-hide-tabs-hooks)

(provide 'centaur-tabs-functions)

;;; centaur-tabs-functions.el ends here
