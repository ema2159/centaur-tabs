;;; centaur-tabs-interactive.el --- centaur-tabs interactive functions and plugins support lexical-binding: t; -*-

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
;; This file contains centaur-tabs interactive functions and plugins support

;;; Code:
;;; Requires
(require 'centaur-tabs-elements)
(require 'centaur-tabs-functions)
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
	 (group-name (or groupname (centaur-tabs-completing-read "Groups: " groups))) )
    (catch 'done
      (mapc
       #'(lambda (group)
	   (when (equal group-name (car (car (cdr (cdr group)))))
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

(defun centaur-tabs-kill-unmodified-buffers-in-current-group ()
  "Kill all unmodified buffer in current group."
  (interactive)
  (let* ((current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t))))
	 (currentbuffer (current-buffer)))
    ;; Kill all buffers in current group.
    (centaur-tabs-kill-buffer-match-rule
     (lambda (buffer) (not (buffer-modified-p buffer))))
    ))


(defun centaur-tabs-kill-match-buffers-in-current-group ()
  "Kill all buffers match extension in current group."
  (interactive)
  (let* ((current-group-name (cdr (centaur-tabs-selected-tab (centaur-tabs-current-tabset t))))
	 (extension-names (centaur-tabs-get-extensions))
	 match-extension)
    ;; Read extension need to kill.
    (setq match-extension (centaur-tabs-completing-read "Kill buffers suffix with: " extension-names))
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
    (setq match-extension (centaur-tabs-completing-read "Just keep buffers suffix with: " extension-names))
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

(defun centaur-tabs-group-buffer-groups ()
  "Use centaur-tabs's own buffer grouping function."
  (interactive)
  (setq centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups)
  (centaur-tabs-display-update))

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

(defun centaur-tabs-group-by-projectile-project()
  "Group by projectile project."
  (interactive)
  (setq centaur-tabs-buffer-groups-function 'centaur-tabs-projectile-buffer-groups)
  (centaur-tabs-display-update))

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


(defun centaur-tabs-extract-window-to-new-frame()
  "Kill the current window in the current frame, and open the current buffer in a new frame."
  (interactive)
  (unless (centaur-tabs--one-window-p)
    (let ((buffer (current-buffer)))
      (delete-window)
      (display-buffer-pop-up-frame buffer nil))))

(defun centaur-tabs--copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  ;;; From https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
  (interactive)
  (let* ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                     (buffer-file-name)))
	 (filename (expand-file-name filename)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the kill ring." filename))))


(defun centaur-tabs-open-directory-in-external-application ()
  "Open the current directory in a external application."
  (interactive)
  (centaur-tabs--open-externally default-directory))

(defun centaur-tabs-open-in-external-application ()
  "Open the file of the current buffer according to its mime type."
  (interactive)
  (let ((path (if (buffer-file-name) (buffer-file-name) default-directory)))
    (centaur-tabs--open-externally path)))

(defun centaur-tabs--open-externally (file-or-path)
  "Open FILE-OR-PATH according to its mime type in an external application.
FILE-OR-PATH is expanded with `expand-file-name`.
Modified copy of `treemacs-visit-node-in-external-application`."
  (let ((path (expand-file-name file-or-path)))
    (pcase system-type
      ('windows-nt
       (declare-function w32-shell-execute "w32fns.c")
       (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t)))
      ('darwin
       (shell-command (format "open \"%s\"" path)))
      ('gnu/linux
       (let ((process-connection-type nil))
	 (start-process "" nil "xdg-open" path)))
      (_ (message "Don't know how to open files on %s." (symbol-name system-type))))))


(defun centaur-tabs--copy-directory-name-to-clipboard ()
  "Copy the current directory name to the clipboard."
  (interactive)
  (when default-directory
    (kill-new default-directory)
    (message "Copied directory name '%s' to the kill ring." (expand-file-name default-directory))))

(defun centaur-tabs--tab-submenu-groups-definition ()
  "Menu definition with a list of tab groups."
  (mapcar (lambda (s) `[,s  ,s]) (sort (centaur-tabs-get-groups) #'string<)))


(defun centaur-tabs--tab-submenu-tabs-definition ()
  "Menu definition with a list of tabs for the current group."
  (let* ((tabset (centaur-tabs-get-tabset centaur-tabs-last-focused-buffer-group))
	 (tabs-in-group (centaur-tabs-tabs tabset))
         (buffers (mapcar #'centaur-tabs-tab-value tabs-in-group))
	 (sorted-tabnames (sort (mapcar #'buffer-name buffers) #'string<)))
    (mapcar (lambda (s) `[,s  ,s]) sorted-tabnames)))



(defvar centaur-tabs--groups-submenu-key "Tab groups")
(defvar centaur-tabs--tabs-submenu-key "Go to tab of group")



(defun centaur-tabs--kill-this-buffer-dont-ask()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer))
  (centaur-tabs-display-update)
  (redisplay t))


(defun centaur-tabs--tab-menu-definition ()
  "Definition of the context menu of a tab."
  `(["Kill this buffer"  centaur-tabs--kill-this-buffer-dont-ask]
    ["Kill other buffers of group" centaur-tabs-kill-other-buffers-in-current-group]
    ["Kill unmodified buffers of group" centaur-tabs-kill-unmodified-buffers-in-current-group]
    "----"
    ["Split below" split-window-below]
    ["Split right" split-window-right]
    "----"
    ["Maximize tab" delete-other-windows
     :active (null (centaur-tabs--one-window-p))]
    ["Extract to new frame" centaur-tabs-extract-window-to-new-frame
     :active (null (centaur-tabs--one-window-p))]
    ["Duplicate in new frame" make-frame-command]
    "----"
    ["Copy filepath" centaur-tabs--copy-file-name-to-clipboard
     :active (buffer-file-name)]
    ["Copy directory path" centaur-tabs--copy-directory-name-to-clipboard
     :active default-directory]
    ["Open in external application" centaur-tabs-open-in-external-application
     :active (or (buffer-file-name) default-directory)]
    ["Open directory in dired" dired-jump
     :active (not (eq major-mode 'dired-mode))]
    ["Open directory externally" centaur-tabs-open-directory-in-external-application
     :active default-directory]
    "----"
    ,( append (list centaur-tabs--groups-submenu-key) (centaur-tabs--tab-submenu-groups-definition))
    ,( append (list centaur-tabs--tabs-submenu-key) (centaur-tabs--tab-submenu-tabs-definition))
    ))

(defun centaur-tabs--one-window-p ()
  "Like `one-window-p`, but taking into account side windows like treemacs."
  (let* ((mainwindow (window-main-window))
	 (child-count (window-child-count mainwindow)))
    (= 0 child-count)))

(defun centaur-tabs--get-tab-from-name (tabname)
  "Get the tab from the current group given de TABNAME."
  (let ((seq (centaur-tabs-tabs (centaur-tabs-get-tabset centaur-tabs-last-focused-buffer-group))))
    (cl-find-if
     (lambda (tab) (string= tabname (buffer-name (centaur-tabs-tab-value tab))))
     seq)))


(defun centaur-tabs--tab-menu (event)
  "Show a context menu for the clicked tab or button.  The clicked tab, identified by EVENT, is selected."
  (interactive "e" )

  (let ((click-on-tab-p (ignore-errors (centaur-tabs-get-tab-from-event event))))

    (when (not click-on-tab-p)
      (centaur-tabs--groups-menu))

    (when click-on-tab-p
      (centaur-tabs-do-select event)
      (redisplay t)

      (let*
	  ((menu (easy-menu-create-menu nil (centaur-tabs--tab-menu-definition)))
	   (choice (x-popup-menu t menu))
	   (action (lookup-key menu (apply 'vector choice)))
	   (action-is-command-p  (and (commandp action) (functionp action))))
	(when action-is-command-p
	  (call-interactively action))
	(when (not action-is-command-p)
	  (let* ((menu-key (first choice))
		 (choice-is-group-p (string= centaur-tabs--groups-submenu-key (symbol-name menu-key)))
		 (name (car (last choice)))
		 (name-as-string (symbol-name name)))
	    (if choice-is-group-p
		(centaur-tabs-switch-group name-as-string)
	      (switch-to-buffer name-as-string))))))))


(defun centaur-tabs--groups-menu ()
  "Show a popup menu with the centaur tabs groups."
  (interactive)

  (let*
      ((sorted-groups (centaur-tabs--tab-submenu-groups-definition))
       (menu (easy-menu-create-menu "Tab groups" (centaur-tabs--tab-submenu-groups-definition)))
       (choice (x-popup-menu t menu))
       (action (lookup-key menu (apply 'vector choice)))
       (action-is-command-p  (and (commandp action) (functionp action))))
    (when action-is-command-p
      (call-interactively action))
    (when (not action-is-command-p)
      (let ((group (car (last choice))))
	(centaur-tabs-switch-group (format "%s" group))))))


(provide 'centaur-tabs-interactive)

;;; centaur-tabs-interactive.el ends here
