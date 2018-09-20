;; awesome-tab.el --- Provide an out of box configuration to use tabbar in Emacs.

;; Filename: awesome-tab.el
;; Description: Provide an out of box configuration to use tabbar in Emacs.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-17 22:14:34
;; Version: 0.4
;; Last-Updated: 2018-09-20 15:25:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-tab.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `tabbar' `projectile'
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
;; Tabbar.el is cool, but very ugly defaultly.
;; Every emacser will waste too much time to customize it.
;;
;; So this extension provide an out of box configuration to use tabbar in Emacs.
;;

;;; Installation:
;;
;; You need install projectile (https://github.com/bbatsov/projectile) first.
;;
;; Then put tabbar.el and awesome-tab.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-tab)
;;
;; No need more.
;;
;; You can use below commands for you need:
;;
;; `tabbar-switch-group'
;; `tabbar-select-beg-tab'
;; `tabbar-select-end-tab'
;; `tabbar-forward-tab-other-window'
;; `tabbar-backward-tab-other-window'
;; `tabbar-kill-all-buffers-in-current-group'
;; `tabbar-kill-match-buffers-in-current-group'
;;
;; If you're helm fans, you need add below code in your helm config:
;;
;; (tabbar-build-helm-source)
;;
;; Then add `helm-source-tabbar-group' in `helm-source-list'
;;

;;; Customize:
;;
;; `tabbar-background-color'
;; `tabbar-active-color'
;; `tabbar-inactive-color'
;; `tabbar-hide-tab-rules'
;;

;;; Change log:
;;
;; 2018/09/20
;;      * Remove empty header line from magit buffers.
;;      * Add new function `tabbar-kill-match-buffers-in-current-group', it's handy in mixin mode, such as web-mode.
;;
;; 2018/09/18
;;      * Fix unselect tab height and add option `tabbar-hide-tab-rules'
;;      * Use `tabbar-groups-hash' store every buffer's project name avoid performance issue cause by `projectile-project-name'.
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
(require 'tabbar)
(require 'projectile)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;; Options ;;;;;;;;;;;;;;;;;;;;;;;
(defvar tabbar-background-color (face-attribute 'default :background))
(defvar tabbar-active-color "green3")
(defvar tabbar-inactive-color "dark green")
(defvar tabbar-hide-tab-rules
  '(("prefix" . "*")
    ("anyplace" . "^magit.*:\\s-")
    ))

;;;;;;;;;;;;;;;;;;;;;;; Theme ;;;;;;;;;;;;;;;;;;;;;;;
(defcustom tabbar-hide-header-button t
  "Hide header button at left-up corner.
Default is t."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (setq
              tabbar-scroll-left-help-function nil ;don't show help information
              tabbar-scroll-right-help-function nil
              tabbar-help-on-tab-function nil
              tabbar-home-help-function nil
              tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
              tabbar-scroll-left-button (quote (("") ""))
              tabbar-scroll-right-button (quote (("") "")))))
  :group 'tabbar)

(custom-set-variables
 '(tabbar-background-color tabbar-background-color)
 )

(custom-set-faces
 '(tabbar-default ((t (:height 1.3))))
 '(tabbar-selected ((t (:inherit tabbar-default :weight ultra-bold :width semi-expanded))))
 '(tabbar-unselected ((t (:inherit tabbar-default))))
 )

(dolist (face '(tabbar-selected
                tabbar-separator
                tabbar-unselected))
  (set-face-attribute face nil
                      :background tabbar-background-color
                      ))

(set-face-attribute 'tabbar-selected nil
                    :foreground tabbar-active-color
                    :overline tabbar-active-color
                    )

(set-face-attribute 'tabbar-unselected nil
                    :foreground tabbar-inactive-color
                    :overline tabbar-inactive-color
                    )

;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun tabbar-switch-group (&optional groupname)
  "Switch tab groups using ido."
  (interactive)
  (let* ((tab-buffer-list (mapcar
                           #'(lambda (b)
                               (with-current-buffer b
                                 (list (current-buffer)
                                       (buffer-name)
                                       (funcall tabbar-buffer-groups-function) )))
                           (funcall tabbar-buffer-list-function)))
         (groups (tabbar-get-groups))
         (group-name (or groupname (ido-completing-read "Groups: " groups))) )
    (catch 'done
      (mapc
       #'(lambda (group)
           (when (equal group-name (car (car (cdr (cdr group)))))
             (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list) )))

(defun tabbar-select-end-tab ()
  "Select end tab of current tabset."
  (interactive)
  (tabbar-select-beg-tab t))

(defun tabbar-select-beg-tab (&optional backward type)
  "Select beginning tab of current tabs.
If BACKWARD is non-nil, move backward, otherwise move forward.
TYPE is default option."
  (interactive)
  (let* ((tabset (tabbar-current-tabset t))
         (ttabset (tabbar-get-tabsets-tabset))
         (cycle (if (and (eq tabbar-cycle-scope 'groups)
                         (not (cdr (tabbar-tabs ttabset))))
                    'tabs
                  tabbar-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (tabbar-selected-tab tabset))
      (setq tabset (tabbar-tabs tabset)
            tab (car (if backward (last tabset) tabset)))
      (tabbar-click-on-tab tab type))))

(defun tabbar-backward-tab-other-window (&optional reversed)
  "Move to left tab in other window.
Optional argument REVERSED default is move backward, if reversed is non-nil move forward."
  (interactive)
  (other-window 1)
  (if reversed
      (tabbar-forward-tab)
    (tabbar-backward-tab))
  (other-window -1))

(defun tabbar-forward-tab-other-window ()
  "Move to right tab in other window."
  (interactive)
  (tabbar-backward-tab-other-window t))

(defun tabbar-kill-all-buffers-in-current-group ()
  "Kill all buffers in current group."
  (interactive)
  (let* ((groups (tabbar-get-groups))
         (current-group-name (cdr (tabbar-selected-tab (tabbar-current-tabset t)))))
    ;; Kill all buffers in current group.
    (save-excursion
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (string-equal current-group-name (cdr (tabbar-selected-tab (tabbar-current-tabset t))))
                    (kill-buffer buffer))))
            (buffer-list)))
    ;; Switch to next group.
    (tabbar-forward-group)
    ))

(defun tabbar-kill-match-buffers-in-current-group ()
  "Kill all buffers match extension in current group."
  (interactive)
  (let* ((groups (tabbar-get-groups))
         (current-group-name (cdr (tabbar-selected-tab (tabbar-current-tabset t))))
         (extension-names '())
         match-extension)
    ;; Read all extension names in current group.
    (save-excursion
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (string-equal current-group-name (cdr (tabbar-selected-tab (tabbar-current-tabset t))))
                    (when (buffer-file-name buffer)
                      (add-to-list 'extension-names (file-name-extension (buffer-file-name buffer))))
                    )))
            (buffer-list)))
    ;; Read extension need to kill.
    (setq match-extension (ido-completing-read "Kill buffer suffix with: " extension-names))
    ;; Kill all buffers match extension in current group.
    (save-excursion
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (string-equal current-group-name (cdr (tabbar-selected-tab (tabbar-current-tabset t))))
                    (let ((filename (buffer-file-name buffer)))
                      (when (and filename (string-equal (file-name-extension filename) match-extension))
                        (kill-buffer buffer)))
                    )))
            (buffer-list)))
    ;; Switch to next group if last file killed.
    (when (equal (length extension-names) 1)
      (tabbar-forward-group))
    ))

;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun tabbar-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun tabbar-get-groups ()
  ;; Refresh groups.
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab))
  (mapcar #'(lambda (group)
              (format "%s" (cdr group)))
          (tabbar-tabs tabbar-tabsets-tabset)))

;;;;;;;;;;;;;;;;;;;;;;; Default configurations ;;;;;;;;;;;;;;;;;;;;;;;

;; Loading tabbar mode.
(tabbar-mode t)

;; Uniquify tab name when open multiple buffers with same filename.
(setq uniquify-separator "/")
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

;; Some buffer's header line is empty that make its window insufficient of space to display all content
;; Feel free to add hook in below list. ;)
(dolist (hook (list
               'magit-status-mode-hook
               'magit-popup-mode-hook
               ))
  (add-hook hook '(lambda () (setq-local header-line-format nil))))

;; Rules to control buffers show in tabs.
(defun tabbar-filter-buffer-list ()
  "`tabbar-filter-buffer-list' control buffers show in tabs.

All buffer that start with * or magit-* won't display in tabbar.
Of course, you still can switch buffer by other emacs commands."
  (tabbar-filter
   (lambda (x)
     (let ((name (format "%s" x)))
       (eval
        `(and ,@ (mapcar #'(lambda (rule)
                             (let ((match-position (car rule))
                                   (match-regexp (cdr rule)))
                               (cond ((string-equal match-position "prefix")
                                      (not (string-prefix-p match-regexp name)))
                                     ((string-equal match-position "anyplace")
                                      (not (string-match match-regexp name)))
                                     ((string-equal match-position "suffix")
                                      (not (string-suffix-p match-regexp name)))
                                     )))
                         tabbar-hide-tab-rules))
        )))
   (delq nil
         (mapcar #'(lambda (b)
                     (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((buffer-file-name b) b)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      ((buffer-live-p b) b)))
                 (buffer-list)))))

(setq tabbar-buffer-list-function 'tabbar-filter-buffer-list)

;; Rules to control buffer's group rules.
(defvar tabbar-groups-hash (make-hash-table :test 'equal))

(defun tabbar-init-groups-name ()
  (interactive)
  (setq tabbar-groups-hash (make-hash-table :test 'equal)))

(defun tabbar-get-group-name (buf)
  (let ((group-name (gethash buf tabbar-groups-hash)))
    (if group-name
        group-name
      (tabbar-set-group-name buf))))

(defun tabbar-set-group-name (buf)
  (with-current-buffer buf
    (let ((project-name (projectile-project-name)))
      (puthash buf project-name tabbar-groups-hash)
      project-name)))

(defun tabbar-buffer-groups-by-mixin-rules ()
  "`tabbar-buffer-groups-by-mixin-rules' control buffers' group rules.

Group tabbar with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `projectile-project-p' with project name."
  (list
   (cond
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    ((memq major-mode '(magit-process-mode
                        magit-status-mode
                        magit-diff-mode
                        magit-log-mode
                        magit-file-mode
                        magit-blob-mode
                        magit-blame-mode
                        ))
     "Magit")
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs")
    (t
     (if (projectile-project-p)
         (tabbar-get-group-name (current-buffer))
       "Common"))
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-mixin-rules)

;; Helm source for switch group in helm.
(defvar helm-source-tabbar-group nil)

(defun tabbar-build-helm-source ()
  (interactive)
  (setq helm-source-tabbar-group
        (when (featurep 'helm)
          (require 'helm)
          (helm-build-sync-source
              "Tabbar Group"
            :candidates #'tabbar-get-groups
            :action '(("Switch to group" . tabbar-switch-group))))))

(provide 'awesome-tab)

;;; awesome-tab.el ends here
