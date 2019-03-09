;;; awesome-tab.el --- Provide an out of box configuration to use tab in Emacs.

;; Filename: awesome-tab.el
;; Description: Provide an out of box configuration to use awesome-tab in Emacs.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-17 22:14:34
;; Version: 2.3
;; Last-Updated: 2019-03-03 21:01:30
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-tab.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `powerline' `cl'
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
;; Provide an out of box configuration to use tab in Emacs.
;;

;;; Installation:
;;
;; Put powerline-separators.el, powerline-themes.el, powerline.el, awesome-tab.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-tab)
;; (awesome-tab-mode t)
;;
;; No need more.
;;
;; You can use below commands for you need:
;;
;; `awesome-tab-switch-group'
;; `awesome-tab-select-beg-tab'
;; `awesome-tab-select-end-tab'
;; `awesome-tab-forward-tab-other-window'
;; `awesome-tab-backward-tab-other-window'
;; `awesome-tab-kill-other-buffers-in-current-group'
;; `awesome-tab-kill-all-buffers-in-current-group'
;; `awesome-tab-kill-match-buffers-in-current-group'
;; `awesome-tab-keep-match-buffers-in-current-group'
;; `awesome-tab-move-current-tab-to-left'
;; `awesome-tab-move-current-tab-to-right'
;;
;; If you're helm fans, you need add below code in your helm config:
;;
;; (awesome-tab-build-helm-source)
;;
;; Then add `helm-source-awesome-tab-group' in `helm-source-list'
;;

;;; Customize:
;;
;; `awesome-tab-background-color'
;; `awesome-tab-selected'
;; `awesome-tab-unselected'
;; `awesome-tab-label-fixed-length'
;;

;;; Change log:
;;
;; 2019/03/07
;;      * Add `cl' dependence.
;;
;; 2019/03/03
;;      * Automatically adsorb tabs after switching tabs, making switch tabs quickly.
;;      * Fix many typo errors.
;;      * Add `awesome-tab-adjust-buffer-order-function'.
;;      * Don't trigger by awesome-tab command, it's annoying.
;;
;; 2019/02/23
;;      * Significantly optimize the performance of switching tab by avoiding excessive calls `project-current'.
;;      * Use `powerline' render tab, it's beautiful!!!
;;
;; 2018/12/27
;;      * Tab will hide if ```awesome-tab-hide-tab-function``` return t, you can write your own code to customize hide rules.
;;
;; 2018/11/16
;;  * Open new tab on right of current one.
;;
;; 2018/11/14
;;      * Remove wheel features, emacser should only use the keyboard to operate Emacs.
;;
;; 2018/11/01
;;      * Remove `projectile' depend.
;;
;; 2018/10/29
;;      * Add `mwheel' depend.
;;
;; 2018/09/29
;;      * Add new command `awesome-tab-kill-other-buffers-in-current-group'
;;      * Not enable mode default.
;;
;; 2018/09/25
;;      * Adjust magit regexp to only match magit buffer, not file that named with magit.
;;
;; 2018/09/22
;;      * Adjust `awesome-tab-buffer-list' to hide unused buffer to user.
;;
;; 2018/09/20
;;      * Remove empty header line from magit buffers.
;;      * Add new function `awesome-tab-kill-match-buffers-in-current-group', it's handy in mixin mode, such as web-mode.
;;      * Add new function `awesome-tab-keep-match-buffers-in-current-group', it's handy in mixin mode, such as web-mode.
;;      * Fix error cause by `awesome-tab-kill-buffer-match-rule'.
;;
;; 2018/09/18
;;      * Fix unselect tab height and add option `awesome-tab-hide-tab-rules'
;;      * Use `awesome-tab-groups-hash' store every buffer's project name avoid performance issue cause by `projectile-project-name'.
;;
;; 2018/09/17
;;      * First released.
;;

;;; Acknowledgements:
;;
;; casouri: documentation and many useful patches.
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'cl)
(require 'powerline)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;; Awesome-Tab source code ;;;;;;;;;;;;;;;;;;;;;;;

(defgroup awesome-tab nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defcustom awesome-tab-cycle-scope nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- default
    Navigate through visible tabs, then through tab groups."
  :group 'awesome-tab
  :type '(choice :tag "Cycle through..."
                 (const :tag "Visible Tabs Only" tabs)
                 (const :tag "Tab Groups Only" groups)
                 (const :tag "Visible Tabs then Tab Groups" nil)))

(defcustom awesome-tab-auto-scroll-flag t
  "*Non-nil means to automatically scroll the tab bar.
That is, when a tab is selected outside of the tab bar visible area,
the tab bar is scrolled horizontally so the selected tab becomes
visible."
  :group 'awesome-tab
  :type 'boolean)

(defcustom awesome-tab-common-group-name "Common"
  "If the current buffer does not belong to any project,
the group name uses the name of this variable."
  :group 'awesome-tab
  :type 'string)

(defcustom awesome-tab-label-fixed-length 0
  "Fixed length of label. Set to 0 if dynamic."
  :group 'awesome-tab
  :type 'int)

(defcustom awesometab-hide-tabs-hooks
  '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook)
  "Some buffer's header line is empty that make its window insufficient of space to display all content.
Feel free to add hook in this option. ;)"
  :type '(repeat symbol)
  :group 'awesome-tab)

(defcustom awesome-tab-background-color "black"
  "*Background color of the tab bar.
By default, use the background color specified for the
`awesome-tab-default' face (or inherited from another face), or the
background color of the `default' face otherwise."
  :group 'awesome-tab
  :type 'face)

(defvar awesome-tab-hide-tab-function 'awesome-tab-hide-tab
  "Function to hide tab.
This fucntion accepet tab name, tab will hide if this function return ni.")

(defvar awesome-tab-current-tabset-function nil
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar.")

(defvar awesome-tab-tab-label-function nil
  "Function that obtains a tab label displayed on the tab bar.
The function is passed a tab and should return a string.")

(defvar awesome-tab-select-tab-function nil
  "Function that select a tab.
The function is passed a tab, and should make it the
selected tab.")

(defvar awesome-tab-buffer-list-function 'awesome-tab-buffer-list
  "Function that returns the list of buffers to show in tabs.
That function is called with no arguments and must return a list of
buffers.")

(defvar awesome-tab-buffer-groups-function 'awesome-tab-buffer-groups
  "Function that gives the group names the current buffer belongs to.
It must return a list of group names, or nil if the buffer has no
group.  Notice that it is better that a buffer belongs to one group.")

(defvar awesome-tab-adjust-buffer-order-function 'awesome-tab-adjust-buffer-order
  "Function to adjust buffer order after switch tab.
Default is `awesome-tab-adjust-buffer-order', you can write your own rule.")

;;; Misc.
;;
(eval-and-compile
  (defalias 'awesome-tab-display-update
    (if (fboundp 'force-window-update)
        #'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update)))

(defun awesome-tab-shorten (str width)
  "Return a shortened string from STR that fits in the given display WIDTH.
WIDTH is specified in terms of character display width in the current
buffer; see also `char-width'.  If STR display width is greater than
WIDTH, STR is truncated and an ellipsis string \"...\" is inserted at
end or in the middle of the returned string, depending on available
room."
  (let* ((n  (length str))
         (sw (string-width str))
         (el "...")
         (ew (string-width el))
         (w  0)
         (i  0))
    (cond
     ;; STR fit in WIDTH, return it.
     ((<= sw width)
      str)
     ;; There isn't enough room for the ellipsis, STR is just
     ;; truncated to fit in WIDTH.
     ((<= width ew)
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (substring str 0 i))
     ;; There isn't enough room to insert the ellipsis in the middle
     ;; of the truncated string, so put the ellipsis at end.
     ((zerop (setq sw (/ (- width ew) 2)))
      (setq width (- width ew))
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (concat (substring str 0 i) el))
     ;; Put the ellipsis in the middle of the truncated string.
     (t
      (while (< w sw)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (setq w (+ w ew))
      (while (< w width)
        (setq n (1- n)
              w (+ w (char-width (aref str n)))))
      (concat (substring str 0 i) el (substring str n)))
     )))

;; Copied from s.el
(defun awesome-tab-truncate-string (len s &optional ellipsis)
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
(defsubst awesome-tab-make-tab (object tabset)
  "Return a new tab with value OBJECT.
TABSET is the tab set the tab belongs to."
  (cons object tabset))

(defsubst awesome-tab-tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

(defsubst awesome-tab-tab-tabset (tab)
  "Return the tab set TAB belongs to."
  (cdr tab))

(defvar awesome-tab-tabsets nil
  "The tab sets store.")

(defvar awesome-tab-tabsets-tabset nil
  "The special tab set of existing tab sets.")

(defvar awesome-tab-current-tabset nil
  "The tab set currently displayed on the tab bar.")
(make-variable-buffer-local 'awesome-tab-current-tabset)

(defvar awesome-tab-init-hook nil
  "Hook run after tab bar data has been initialized.
You should use this hook to initialize dependent data.")

(defsubst awesome-tab-init-tabsets-store ()
  "Initialize the tab set store."
  (setq awesome-tab-tabsets (make-vector 31 0)
        awesome-tab-tabsets-tabset (make-symbol "awesome-tab-tabsets-tabset"))
  (put awesome-tab-tabsets-tabset 'start 0)
  (run-hooks 'awesome-tab-init-hook))

(defvar awesome-tab-quit-hook nil
  "Hook run after tab bar data has been freed.
You should use this hook to reset dependent data.")

(defsubst awesome-tab-free-tabsets-store ()
  "Free the tab set store."
  (setq awesome-tab-tabsets nil
        awesome-tab-tabsets-tabset nil)
  (run-hooks 'awesome-tab-quit-hook))

;; Define an "hygienic" function free of side effect between its local
;; variables and those of the callee.
(eval-and-compile
  (defalias 'awesome-tab-map-tabsets
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
            awesome-tab-tabsets)
           ,result)))))

(defun awesome-tab-make-tabset (name &rest objects)
  "Make a new tab set whose name is the string NAME.
It is initialized with tabs build from the list of OBJECTS."
  (let* ((tabset (intern name awesome-tab-tabsets))
         (tabs (mapcar #'(lambda (object)
                           (awesome-tab-make-tab object tabset))
                       objects)))
    (set tabset tabs)
    (put tabset 'select (car tabs))
    (put tabset 'start 0)
    tabset))

(defsubst awesome-tab-get-tabset (name)
  "Return the tab set whose name is the string NAME.
Return nil if not found."
  (intern-soft name awesome-tab-tabsets))

(defsubst awesome-tab-delete-tabset (tabset)
  "Delete the tab set TABSET.
That is, remove it from the tab sets store."
  (unintern tabset awesome-tab-tabsets))

(defsubst awesome-tab-tabs (tabset)
  "Return the list of tabs in TABSET."
  (symbol-value tabset))

(defsubst awesome-tab-tab-values (tabset)
  "Return the list of tab values in TABSET."
  (mapcar 'awesome-tab-tab-value (awesome-tab-tabs tabset)))

(defsubst awesome-tab-get-tab (object tabset)
  "Search for a tab with value OBJECT in TABSET.
Return the tab found, or nil if not found."
  (assoc object (awesome-tab-tabs tabset)))

(defsubst awesome-tab-member (tab tabset)
  "Return non-nil if TAB is in TABSET."
  (or (eq (awesome-tab-tab-tabset tab) tabset)
      (memq tab (awesome-tab-tabs tabset))))

(defsubst awesome-tab-template (tabset)
  "Return the cached visual representation of TABSET.
That is, a `header-line-format' template, or nil if the cache is
empty."
  (get tabset 'template))

(defsubst awesome-tab-set-template (tabset template)
  "Set the cached visual representation of TABSET to TEMPLATE.
TEMPLATE must be a valid `header-line-format' template, or nil to
cleanup the cache."
  (put tabset 'template template))

(defsubst awesome-tab-selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst awesome-tab-selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (awesome-tab-tab-value (awesome-tab-selected-tab tabset)))

(defsubst awesome-tab-selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (awesome-tab-selected-tab tabset)))

(defvar awesome-tab--track-selected nil)

(defsubst awesome-tab-select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (awesome-tab-member tab tabset)
    (unless (awesome-tab-selected-p tab tabset)
      (awesome-tab-set-template tabset nil)
      (setq awesome-tab--track-selected awesome-tab-auto-scroll-flag))
    (put tabset 'select tab)))

(defsubst awesome-tab-select-tab-value (object tabset)
  "Make the tab with value OBJECT, the selected tab in TABSET.
Does nothing if a tab with value OBJECT is not found in TABSET.
Return the tab selected, or nil if nothing was selected."
  (awesome-tab-select-tab (awesome-tab-get-tab object tabset) tabset))

(defsubst awesome-tab-start (tabset)
  "Return the index of the first visible tab in TABSET."
  (get tabset 'start))

(defsubst awesome-tab-view (tabset)
  "Return the list of visible tabs in TABSET.
That is, the sub-list of tabs starting at the first visible one."
  (nthcdr (awesome-tab-start tabset) (awesome-tab-tabs tabset)))

(defun awesome-tab-add-tab (tabset object)
  "Return tab if it has opend.
Otherwise insert new tab on right of current tab."
  (let ((tabs (awesome-tab-tabs tabset)))
    (if (awesome-tab-get-tab object tabset)
        tabs
      (let* ((tab (awesome-tab-make-tab object tabset))
             (selected (awesome-tab-selected-tab tabset))
             (selected-index (cl-position (car selected) (mapcar 'car tabs))))
        (awesome-tab-set-template tabset nil)
        (set tabset (awesome-tab-insert-at tabs selected-index tab))
        ))))

(defun awesome-tab-insert-at (list index insert-element)
  (let ((counter 0)
        (result '()))
    (dolist (element list)
      (if (equal counter index)
          (setq result (append result (list element insert-element)))
        (setq result (append result (list element))))
      (setq counter (+ 1 counter)))
    result))

(defun awesome-tab-delete-tab (tab)
  "Remove TAB from its tab set."
  (let* ((tabset (awesome-tab-tab-tabset tab))
         (tabs   (awesome-tab-tabs tabset))
         (sel    (eq tab (awesome-tab-selected-tab tabset)))
         (next   (and sel (cdr (memq tab tabs)))))
    (awesome-tab-set-template tabset nil)
    (setq tabs (delq tab tabs))
    ;; When the selected tab is deleted, select the next one, if
    ;; available, or the last one otherwise.
    (and sel (awesome-tab-select-tab (car (or next (last tabs))) tabset))
    (set tabset tabs)))

(defun awesome-tab-scroll (tabset count)
  "Scroll the visible tabs in TABSET of COUNT units.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (awesome-tab-start tabset) count))
                    (1- (length (awesome-tab-tabs tabset))))))
    (when (/= start (awesome-tab-start tabset))
      (awesome-tab-set-template tabset nil)
      (put tabset 'start start))))

(defun awesome-tab-tab-next (tabset tab &optional before)
  "Search in TABSET for the tab after TAB.
If optional argument BEFORE is non-nil, search for the tab before
TAB.  Return the tab found, or nil otherwise."
  (let* (last (tabs (awesome-tab-tabs tabset)))
    (while (and tabs (not (eq tab (car tabs))))
      (setq last (car tabs)
            tabs (cdr tabs)))
    (and tabs (if before last (nth 1 tabs)))))

(defun awesome-tab-current-tabset (&optional update)
  "Return the tab set currently displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`awesome-tab-current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (and update awesome-tab-current-tabset-function
       (setq awesome-tab-current-tabset
             (funcall awesome-tab-current-tabset-function)))
  awesome-tab-current-tabset)

(defun awesome-tab-get-tabsets-tabset ()
  "Return the tab set of selected tabs in existing tab sets."
  (set awesome-tab-tabsets-tabset (awesome-tab-map-tabsets 'awesome-tab-selected-tab))
  (awesome-tab-scroll awesome-tab-tabsets-tabset 0)
  (awesome-tab-set-template awesome-tab-tabsets-tabset nil)
  awesome-tab-tabsets-tabset)

;;; Faces
;;
(defface awesome-tab-default
  '((t
     (:background "black" :foreground "black")))
  "Default face used in the tab bar."
  :group 'awesome-tab)

(defface awesome-tab-unselected
  '((t
     (:background "#3D3C3D" :foreground "grey50")))
  "Face used for unselected tabs."
  :group 'awesome-tab)

(defface awesome-tab-selected
  '((t (:background "#31343E" :foreground "white")))
  "Face used for the selected tab."
  :group 'awesome-tab)

(defface awesome-tab-button
  '((t
     :box nil
     ))
  "Face used for tab bar buttons."
  :group 'awesome-tab)

;;; Tabs
;;
(defsubst awesome-tab-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list
   (1- (length (awesome-tab-tabs tabset)))
   ))

(defsubst awesome-tab-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `awesome-tab-tab-label-function' to obtain a label for TAB."
  (concat (propertize
           (if awesome-tab-tab-label-function
               (funcall awesome-tab-tab-label-function tab)
             tab)
           'awesome-tab-tab tab
           'face (if (awesome-tab-selected-p tab (awesome-tab-current-tabset))
                     'awesome-tab-selected
                   'awesome-tab-unselected)
           'pointer 'hand)
          ))

(defun awesome-tab-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (awesome-tab-selected-tab tabset))
         (tabs (awesome-tab-view tabset))
         (padcolor awesome-tab-background-color)
         atsel elts)
    ;; Track the selected tab to ensure it is always visible.
    (when awesome-tab--track-selected
      (while (not (memq sel tabs))
        (awesome-tab-scroll tabset -1)
        (setq tabs (awesome-tab-view tabset)))
      (while (and tabs (not atsel))
        (setq elts  (cons (awesome-tab-line-tab (car tabs)) elts)
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
          (apply 'insert (awesome-tab-line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (awesome-tab-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq awesome-tab--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (awesome-tab-line-tab (car tabs)) elts)
            tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (awesome-tab-set-template
     tabset
     (list (awesome-tab-line-buttons tabset)
           (nreverse elts)
           (propertize "%-"
                       'face (list :background padcolor
                                   :foreground padcolor)
                       'pointer 'arrow)))
    ))

(defun awesome-tab-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window `awesome-tab-hide-tab-function' return nil."
  (cond
   ((funcall awesome-tab-hide-tab-function (current-buffer))
    ;; Don't show the tab bar.
    (setq header-line-format nil))
   ((awesome-tab-current-tabset t)
    ;; When available, use a cached tab bar value, else recompute it.
    (or (awesome-tab-template awesome-tab-current-tabset)
        (awesome-tab-line-format awesome-tab-current-tabset)))))

(defconst awesome-tab-header-line-format '(:eval (awesome-tab-line))
  "The tab bar header line format.")

;;; Cyclic navigation through tabs
;;
(defun awesome-tab-cycle (&optional backward type)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `awesome-tab-cycle-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead."
  (let* ((tabset (awesome-tab-current-tabset t))
         (ttabset (awesome-tab-get-tabsets-tabset))
         ;; If navigation through groups is requested, and there is
         ;; only one group, navigate through visible tabs.
         (cycle (if (and (eq awesome-tab-cycle-scope 'groups)
                         (not (cdr (awesome-tab-tabs ttabset))))
                    'tabs
                  awesome-tab-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (awesome-tab-selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq cycle 'tabs)
        (setq tab (awesome-tab-tab-next tabset selected backward))
        ;; When there is no tab after/before the selected one, cycle
        ;; to the first/last visible tab.
        (unless tab
          (setq tabset (awesome-tab-tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       ;; Cycle through tab groups only.
       ((eq cycle 'groups)
        (setq tab (awesome-tab-tab-next ttabset selected backward))
        ;; When there is no group after/before the selected one, cycle
        ;; to the first/last available group.
        (unless tab
          (setq tabset (awesome-tab-tabs ttabset)
                tab (car (if backward (last tabset) tabset))))
        )
       (t
        ;; Cycle through visible tabs then tab groups.
        (setq tab (awesome-tab-tab-next tabset selected backward))
        ;; When there is no visible tab after/before the selected one,
        ;; cycle to the next/previous available group.
        (unless tab
          (setq tab (awesome-tab-tab-next ttabset selected backward))
          ;; When there is no next/previous group, cycle to the
          ;; first/last available group.
          (unless tab
            (setq tabset (awesome-tab-tabs ttabset)
                  tab (car (if backward (last tabset) tabset))))
          ;; Select the first/last visible tab of the new group.
          (setq tabset (awesome-tab-tabs (awesome-tab-tab-tabset tab))
                tab (car (if backward (last tabset) tabset))))
        ))
      (awesome-tab-buffer-select-tab tab))))

;;;###autoload
(defun awesome-tab-backward ()
  "Select the previous available tab.
Depend on the setting of the option `awesome-tab-cycle-scope'."
  (interactive)
  (awesome-tab-cycle t))

;;;###autoload
(defun awesome-tab-forward ()
  "Select the next available tab.
Depend on the setting of the option `awesome-tab-cycle-scope'."
  (interactive)
  (awesome-tab-cycle))

;;;###autoload
(defun awesome-tab-backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((awesome-tab-cycle-scope 'groups))
    (awesome-tab-cycle t)))

;;;###autoload
(defun awesome-tab-forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((awesome-tab-cycle-scope 'groups))
    (awesome-tab-cycle)))

;;;###autoload
(defun awesome-tab-backward-tab ()
  "Select the previous visible tab."
  (interactive)
  (let ((awesome-tab-cycle-scope 'tabs))
    (awesome-tab-cycle t)))

;;;###autoload
(defun awesome-tab-forward-tab ()
  "Select the next visible tab."
  (interactive)
  (let ((awesome-tab-cycle-scope 'tabs))
    (awesome-tab-cycle)))

;;; Minor modes
;;
(defsubst awesome-tab-mode-on-p ()
  "Return non-nil if Awesome-Tab mode is on."
  (eq (default-value 'header-line-format)
      awesome-tab-header-line-format))

;;; Awesome-Tab-Local mode
;;
(defvar awesome-tab--local-hlf nil)

;;;###autoload
(define-minor-mode awesome-tab-local-mode
  "Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Awesome-Tab mode is off."
  :group 'awesome-tab
  :global nil
  (unless (awesome-tab-mode-on-p)
    (error "Awesome-Tab mode must be enabled"))
;;; ON
  (if awesome-tab-local-mode
      (if (and (local-variable-p 'header-line-format)
               header-line-format)
          ;; A local header line exists, hide it to show the tab bar.
          (progn
            ;; Fail in case of an inconsistency because another local
            ;; header line is already hidden.
            (when (local-variable-p 'awesome-tab--local-hlf)
              (error "Another local header line is already hidden"))
            (set (make-local-variable 'awesome-tab--local-hlf)
                 header-line-format)
            (kill-local-variable 'header-line-format))
        ;; Otherwise hide the tab bar in this buffer.
        (setq header-line-format nil))
;;; OFF
    (if (local-variable-p 'awesome-tab--local-hlf)
        ;; A local header line is hidden, show it again.
        (progn
          (setq header-line-format awesome-tab--local-hlf)
          (kill-local-variable 'awesome-tab--local-hlf))
      ;; The tab bar is locally hidden, show it again.
      (kill-local-variable 'header-line-format))))

;;; Awesome-Tab mode
;;
(defvar awesome-tab-prefix-key [(control ?c)]
  "The common prefix key used in Awesome-Tab mode.")

(defvar awesome-tab-prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km [(control left)]  'awesome-tab-backward)
    (define-key km [(control right)] 'awesome-tab-forward)
    (define-key km [(control up)]    'awesome-tab-backward-group)
    (define-key km [(control down)]  'awesome-tab-forward-group)
    (define-key km [(control f10)]   'awesome-tab-local-mode)
    km)
  "The key bindings provided in Awesome-Tab mode.")

(defvar awesome-tab-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km awesome-tab-prefix-key awesome-tab-prefix-map)
    km)
  "Keymap to use in  Awesome-Tab mode.")

(defvar awesome-tab--global-hlf nil)

;;;###autoload
(define-minor-mode awesome-tab-mode
  "Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{awesome-tab-mode-map}"
  :group 'awesome-tab
  :require 'awesome-tab
  :global t
  :keymap awesome-tab-mode-map
  (if awesome-tab-mode
;;; ON
      (unless (awesome-tab-mode-on-p)
        ;; Save current default value of `header-line-format'.
        (setq awesome-tab--global-hlf (default-value 'header-line-format))
        (awesome-tab-init-tabsets-store)
        (setq-default header-line-format awesome-tab-header-line-format))
;;; OFF
    (when (awesome-tab-mode-on-p)
      ;; Turn off Awesome-Tab-Local mode globally.
      (mapc #'(lambda (b)
                (condition-case nil
                    (with-current-buffer b
                      (and awesome-tab-local-mode
                           (awesome-tab-local-mode -1)))
                  (error nil)))
            (buffer-list))
      ;; Restore previous `header-line-format'.
      (setq-default header-line-format awesome-tab--global-hlf)
      (awesome-tab-free-tabsets-store))
    ))

;;; Buffer tabs
;;
(defgroup awesome-tab-buffer nil
  "Display buffers in the tab bar."
  :group 'awesome-tab)

(defun awesome-tab-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun awesome-tab-filter-out (condp lst)
  (delq nil
        (mapcar (lambda (x) (if (funcall condp x) nil x)) lst)))

(defun awesome-tab-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
  (awesome-tab-filter-out
   awesome-tab-hide-tab-function
   (delq nil
         (mapcar #'(lambda (b)
                     (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((buffer-file-name b) b)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      ((buffer-live-p b) b)))
                 (buffer-list)))))

(defun awesome-tab-buffer-mode-derived-p (mode parents)
  "Return non-nil if MODE derives from a mode in PARENTS."
  (let (derived)
    (while (and (not derived) mode)
      (if (memq mode parents)
          (setq derived t)
        (setq mode (get mode 'derived-mode-parent))))
    derived))

;;; Group buffers in tab sets.
;;
(defvar awesome-tab--buffers nil)

(defun awesome-tab-buffer-update-groups ()
  "Update tab sets from groups of existing buffers.
Return the the first group where the current buffer is."
  (let ((bl (sort
             (mapcar
              #'(lambda (b)
                  (with-current-buffer b
                    (list (current-buffer)
                          (buffer-name)
                          (if awesome-tab-buffer-groups-function
                              (funcall awesome-tab-buffer-groups-function)
                            '(awesome-tab-common-group-name)))))
              (and awesome-tab-buffer-list-function
                   (funcall awesome-tab-buffer-list-function)))
             #'(lambda (e1 e2)
                 (string-lessp (nth 1 e1) (nth 1 e2))))))
    ;; If the cache has changed, update the tab sets.
    (unless (equal bl awesome-tab--buffers)
      ;; Add new buffers, or update changed ones.
      (dolist (e bl)
        (dolist (g (nth 2 e))
          (let ((tabset (awesome-tab-get-tabset g)))
            (if tabset
                (unless (equal e (assq (car e) awesome-tab--buffers))
                  ;; This is a new buffer, or a previously existing
                  ;; buffer that has been renamed, or moved to another
                  ;; group.  Update the tab set, and the display.
                  (awesome-tab-add-tab tabset (car e))
                  (awesome-tab-set-template tabset nil))
              (awesome-tab-make-tabset g (car e))))))
      ;; Remove tabs for buffers not found in cache or moved to other
      ;; groups, and remove empty tabsets.
      (mapc 'awesome-tab-delete-tabset
            (awesome-tab-map-tabsets
             #'(lambda (tabset)
                 (dolist (tab (awesome-tab-tabs tabset))
                   (let ((e (assq (awesome-tab-tab-value tab) bl)))
                     (or (and e (memq tabset
                                      (mapcar 'awesome-tab-get-tabset
                                              (nth 2 e))))
                         (awesome-tab-delete-tab tab))))
                 ;; Return empty tab sets
                 (unless (awesome-tab-tabs tabset)
                   tabset))))
      ;; The new cache becomes the current one.
      (setq awesome-tab--buffers bl)))
  ;; Return the first group the current buffer belongs to.
  (car (nth 2 (assq (current-buffer) awesome-tab--buffers))))

;;; Tab bar callbacks
;;
(defvar awesome-tab--buffer-show-groups nil)

(defsubst awesome-tab-buffer-show-groups (flag)
  "Set display of tabs for groups of buffers to FLAG."
  (setq awesome-tab--buffer-show-groups flag))

(defun awesome-tab-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((tabset (awesome-tab-get-tabset (awesome-tab-buffer-update-groups))))
    (awesome-tab-select-tab-value (current-buffer) tabset)
    (when awesome-tab--buffer-show-groups
      (setq tabset (awesome-tab-get-tabsets-tabset))
      (awesome-tab-select-tab-value (current-buffer) tabset))
    tabset))

(defvar awesome-tab-height 22)
(defvar awesome-tab-style-left (powerline-wave-right 'awesome-tab-default nil awesome-tab-height))
(defvar awesome-tab-style-right (powerline-wave-left nil 'awesome-tab-default awesome-tab-height))

(defun awesome-tab-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (powerline-render (list awesome-tab-style-left
                          (format " %s "
                                  (let ((bufname (buffer-name (car tab))))
                                    (if (> awesome-tab-label-fixed-length 0)
                                        (awesome-tab-truncate-string  awesome-tab-label-fixed-length bufname)
                                      bufname)))
                          awesome-tab-style-right)))

(defun awesome-tab-buffer-select-tab (tab)
  "Select tab."
  (let ((buffer (awesome-tab-tab-value tab)))
    (switch-to-buffer buffer)
    (awesome-tab-buffer-show-groups nil)
    (awesome-tab-display-update)
    ))

(defun awesome-tab-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Awesome-Tab mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and (eq header-line-format awesome-tab-header-line-format)
       (eq awesome-tab-current-tabset-function 'awesome-tab-buffer-tabs)
       (eq (current-buffer) (window-buffer (selected-window)))
       (let ((bl (awesome-tab-tab-values (awesome-tab-current-tabset)))
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
(defun awesome-tab-buffer-init ()
  "Initialize tab bar buffer data.
Run as `awesome-tab-init-hook'."
  (setq awesome-tab--buffers nil
        awesome-tab--buffer-show-groups nil
        awesome-tab-current-tabset-function 'awesome-tab-buffer-tabs
        awesome-tab-tab-label-function 'awesome-tab-buffer-tab-label
        awesome-tab-select-tab-function 'awesome-tab-buffer-select-tab
        )
  (add-hook 'kill-buffer-hook 'awesome-tab-buffer-track-killed))

(defun awesome-tab-buffer-quit ()
  "Quit tab bar buffer.
Run as `awesome-tab-quit-hook'."
  (setq awesome-tab--buffers nil
        awesome-tab--buffer-show-groups nil
        awesome-tab-current-tabset-function nil
        awesome-tab-tab-label-function nil
        awesome-tab-select-tab-function nil
        )
  (remove-hook 'kill-buffer-hook 'awesome-tab-buffer-track-killed))

(add-hook 'awesome-tab-init-hook 'awesome-tab-buffer-init)
(add-hook 'awesome-tab-quit-hook 'awesome-tab-buffer-quit)

;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun awesome-tab-switch-group (&optional groupname)
  "Switch tab groups using ido."
  (interactive)
  (let* ((tab-buffer-list (mapcar
                           #'(lambda (b)
                               (with-current-buffer b
                                 (list (current-buffer)
                                       (buffer-name)
                                       (funcall awesome-tab-buffer-groups-function) )))
                           (funcall awesome-tab-buffer-list-function)))
         (groups (awesome-tab-get-groups))
         (group-name (or groupname (ido-completing-read "Groups: " groups))) )
    (catch 'done
      (mapc
       #'(lambda (group)
           (when (equal group-name (car (car (cdr (cdr group)))))
             (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list) )))

(defun awesome-tab-select-end-tab ()
  "Select end tab of current tabset."
  (interactive)
  (awesome-tab-select-beg-tab t))

(defun awesome-tab-select-beg-tab (&optional backward type)
  "Select beginning tab of current tabs.
If BACKWARD is non-nil, move backward, otherwise move forward.
TYPE is default option."
  (interactive)
  (let* ((tabset (awesome-tab-current-tabset t))
         (ttabset (awesome-tab-get-tabsets-tabset))
         (cycle (if (and (eq awesome-tab-cycle-scope 'groups)
                         (not (cdr (awesome-tab-tabs ttabset))))
                    'tabs
                  awesome-tab-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (awesome-tab-selected-tab tabset))
      (setq tabset (awesome-tab-tabs tabset)
            tab (car (if backward (last tabset) tabset)))
      (awesome-tab-buffer-select-tab tab))))

(defun awesome-tab-backward-tab-other-window (&optional reversed)
  "Move to left tab in other window.
Optional argument REVERSED default is move backward, if reversed is non-nil move forward."
  (interactive)
  (other-window 1)
  (if reversed
      (awesome-tab-forward-tab)
    (awesome-tab-backward-tab))
  (other-window -1))

(defun awesome-tab-forward-tab-other-window ()
  "Move to right tab in other window."
  (interactive)
  (awesome-tab-backward-tab-other-window t))

(defun awesome-tab-move-current-tab-to-right ()
  "Move current tab one place right, unless it's already the rightmost."
  (interactive)
  (let* ((bufset (awesome-tab-current-tabset t))
         (old-bufs (awesome-tab-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
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
      (error "Error: current buffer's name was not found in Awesome-Tab's buffer list."))
    (setq new-bufs (reverse new-bufs))
    (setq new-bufs (append new-bufs (cdr old-bufs)))
    (set bufset new-bufs)
    (awesome-tab-set-template bufset nil)
    (awesome-tab-display-update)))

(defun awesome-tab-move-current-tab-to-left ()
  "Move current tab one place left, unless it's already the leftmost."
  (interactive)
  (let* ((bufset (awesome-tab-current-tabset t))
         (old-bufs (awesome-tab-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
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
        (error "Error: current buffer's name was not found in Awesome-Tab's buffer list."))
      (set bufset new-bufs)
      (awesome-tab-set-template bufset nil)
      (awesome-tab-display-update))))

(defun awesome-tab-kill-all-buffers-in-current-group ()
  "Kill all buffers in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t)))))
    ;; Kill all buffers in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer) t))
    ;; Switch to next group.
    (awesome-tab-forward-group)
    ))

(defun awesome-tab-kill-other-buffers-in-current-group ()
  "Kill all buffers except current buffer in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
         (currentbuffer (current-buffer)))
    ;; Kill all buffers in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer) (not (equal buffer currentbuffer))))
    ))

(defun awesome-tab-kill-match-buffers-in-current-group ()
  "Kill all buffers match extension in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
         (extension-names (awesome-tab-get-extensions))
         match-extension)
    ;; Read extension need to kill.
    (setq match-extension (ido-completing-read "Kill buffers suffix with: " extension-names))
    ;; Kill all buffers match extension in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer)
       (let ((filename (buffer-file-name buffer)))
         (and filename (string-equal (file-name-extension filename) match-extension))
         )))
    ;; Switch to next group if last file killed.
    (when (equal (length extension-names) 1)
      (awesome-tab-forward-group))
    ))

(defun awesome-tab-keep-match-buffers-in-current-group ()
  "Keep all buffers match extension in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
         (extension-names (awesome-tab-get-extensions))
         match-extension)
    ;; Read extension need to kill.
    (setq match-extension (ido-completing-read "Just keep buffers suffix with: " extension-names))
    ;; Kill all buffers match extension in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer)
       (let ((filename (buffer-file-name buffer)))
         (and filename (not (string-equal (file-name-extension filename) match-extension)))
         )))
    ;; Switch to next group if last file killed.
    (when (equal (length extension-names) 1)
      (awesome-tab-forward-group))
    ))

;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun awesome-tab-get-groups ()
  ;; Refresh groups.
  (set awesome-tab-tabsets-tabset (awesome-tab-map-tabsets 'awesome-tab-selected-tab))
  (mapcar #'(lambda (group)
              (format "%s" (cdr group)))
          (awesome-tab-tabs awesome-tab-tabsets-tabset)))

(defun awesome-tab-get-extensions ()
  ;; Refresh groups.
  (set awesome-tab-tabsets-tabset (awesome-tab-map-tabsets 'awesome-tab-selected-tab))
  (let ((extension-names '()))
    (mapc #'(lambda (buffer)
              (with-current-buffer buffer
                (when (string-equal current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
                  (when (buffer-file-name buffer)
                    (add-to-list 'extension-names (file-name-extension (buffer-file-name buffer))))
                  )))
          (buffer-list))
    extension-names))

(defmacro awesome-tab-kill-buffer-match-rule (match-rule)
  `(save-excursion
     (mapc #'(lambda (buffer)
               (with-current-buffer buffer
                 (when (string-equal current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
                   (when (funcall ,match-rule buffer)
                     (kill-buffer buffer))
                   )))
           (buffer-list))))

;;;;;;;;;;;;;;;;;;;;;;; Default configurations ;;;;;;;;;;;;;;;;;;;;;;;

;; Uniquify tab name when open multiple buffers with same filename.
(setq uniquify-separator "/")
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(dolist (hook awesometab-hide-tabs-hooks)
  (add-hook hook '(lambda () (setq-local header-line-format nil))))

;; Rules to control buffer's group rules.
(defvar awesome-tab-groups-hash (make-hash-table :test 'equal))

(defun awesome-tab-project-name ()
  (let ((project-name (cdr (project-current))))
    (if project-name
        (format "Project: %s" (expand-file-name project-name))
      awesome-tab-common-group-name)))

(defun awesome-tab-get-group-name (buf)
  (let ((group-name (gethash buf awesome-tab-groups-hash)))
    ;; Return group name cache if it exists for improve performance.
    (if group-name
        group-name
      ;; Otherwise try get group name with `project-current'.
      ;; `project-current' is very slow, it will slow down Emacs if you call it when switch buffer.
      (with-current-buffer buf
        (let ((project-name (awesome-tab-project-name)))
          (puthash buf project-name awesome-tab-groups-hash)
          project-name)))))

(defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
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
     (awesome-tab-get-group-name (current-buffer))))))

;; Helm source for switching group in helm.
(defvar helm-source-awesome-tab-group nil)

(defun awesome-tab-build-helm-source ()
  (interactive)
  (setq helm-source-awesome-tab-group
        (when (featurep 'helm)
          (require 'helm)
          (helm-build-sync-source "Awesome-Tab Group"
            :candidates #'awesome-tab-get-groups
            :action '(("Switch to group" . awesome-tab-switch-group))))))

;; Ivy source for switching group in ivy.
(defvar ivy-source-awesome-tab-group nil)

(defun awesome-tab-build-ivy-source ()
  (interactive)
  (setq ivy-source-awesome-tab-group
        (when (featurep 'ivy)
          (require 'ivy)
          (ivy-read
           "Awesome-Tab Groups:"
           (awesome-tab-get-groups)
           :action #'awesome-tab-switch-group))))

(defun awesome-tab-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(defvar awesome-tab-last-focus-buffer nil
  "The last focus buffer.")

(defvar awesome-tab-last-focus-buffer-group nil
  "The group name of last focus buffer.")

(defun awesome-tab-remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun awesome-tab-insert-after (list aft-el el)
  "Insert EL after AFT-EL in LIST."
  (push el (cdr (member aft-el list)))
  list)

(defun awesome-tab-insert-before (list bef-el el)
  "Insert EL before BEF-EL in LIST."
  (nreverse (awesome-tab-insert-after (nreverse list) bef-el el)))

(defun awesome-tab-adjust-buffer-order ()
  "Put the two buffers switched to the adjacent position after current buffer changed."
  ;; Don't trigger by awesome-tab command, it's annoying.
  ;; This feature should trigger by search plugins, such as ibuffer, helm or ivy.
  (unless (string-prefix-p "awesome-tab" (format "%s" this-command))
    ;; Just continue when buffer changed.
    (when (and (not (eq (current-buffer) awesome-tab-last-focus-buffer))
               (not (minibufferp)))
      (let* ((current (current-buffer))
             (previous awesome-tab-last-focus-buffer)
             (current-group (first (funcall awesome-tab-buffer-groups-function))))
        ;; Record last focus buffer.
        (setq awesome-tab-last-focus-buffer current)

        ;; Just continue if two buffers are in same group.
        (when (eq current-group awesome-tab-last-focus-buffer-group)
          (let* ((bufset (awesome-tab-get-tabset current-group))
                 (current-group-tabs (awesome-tab-tabs bufset))
                 (current-group-buffers (mapcar 'car current-group-tabs))
                 (current-buffer-index (cl-position current current-group-buffers))
                 (previous-buffer-index (cl-position previous current-group-buffers)))

            ;; If the two tabs are not adjacent, swap the positions of the two tabs.
            (when (and current-buffer-index
                       previous-buffer-index
                       (> (abs (- current-buffer-index previous-buffer-index)) 1))
              (let* ((copy-group-tabs (copy-list current-group-tabs))
                     (previous-tab (nth previous-buffer-index copy-group-tabs))
                     (current-tab (nth current-buffer-index copy-group-tabs))
                     (base-group-tabs (awesome-tab-remove-nth-element previous-buffer-index copy-group-tabs))
                     (new-group-tabs
                      (if (> current-buffer-index previous-buffer-index)
                          (awesome-tab-insert-before base-group-tabs current-tab previous-tab)
                        (awesome-tab-insert-after base-group-tabs current-tab previous-tab))))
                (set bufset new-group-tabs)
                (awesome-tab-set-template bufset nil)
                (awesome-tab-display-update)
                ))))

        ;; Update the group name of the last access tab.
        (setq awesome-tab-last-focus-buffer-group current-group)
        ))))

(add-hook 'post-command-hook awesome-tab-adjust-buffer-order-function)

(provide 'awesome-tab)

;;; awesome-tab.el ends here
