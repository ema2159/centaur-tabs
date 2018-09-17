;;; awesome-tab.el --- Provide an out of box configuration to use tabbar in Emacs.

;; Filename: awesome-tab.el
;; Description: Provide an out of box configuration to use tabbar in Emacs.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-17 22:14:34
;; Version: 0.1
;; Last-Updated: 2018-09-17 22:14:34
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
;; Provide an out of box configuration to use tabbar in Emacs.
;;

;;; Installation:
;;
;; Put awesome-tab.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-tab)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET awesome-tab RET
;;

;;; Change log:
;;
;; 2018/09/17
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'tabbar)
(require 'projectile)

;;; Code:

(tabbar-mode t)                         ;多标签模式
(setq uniquify-separator "/")           ;分隔符
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ;反方向的显示重复的Buffer名字
(setq uniquify-after-kill-buffer-p t)   ;删除重复名字的Buffer后重命名

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

(defun tabbar-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun tabbar-filter-buffer-list ()
  (tabbar-filter
   (lambda (x)
     (let ((name (format "%s" x)))
       (and
        (not (string-prefix-p "*" name))
        (not (string-match "^magit.*:\\s-" name))
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

(defun tabbar-buffer-groups-by-mixin-rules ()
  "Mixin multiple rules.

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
         (projectile-project-name)
       "Common"))
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-mixin-rules)

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

(defun tabbar-get-groups ()
  ;; Refresh groups.
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab))
  (mapcar #'(lambda (group)
              (format "%s" (cdr group)))
          (tabbar-tabs tabbar-tabsets-tabset)))

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

(defvar helm-source-tabbar-group nil)

(defun tabbar-build-helm-source ()
  (interactive)
  (setq helm-source-tabbar-group
        (when (featurep 'helm)
          (require 'helm)
          (helm-build-sync-source "Tabbar Group"
                                  :candidates #'tabbar-get-groups
                                  :action '(("Switch to group" . tabbar-switch-group))))))

(custom-set-variables
 '(tabbar-background-color "black")
 )

(custom-set-faces
 '(tabbar-default ((t (:height 1.3))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "black" :foreground "green3" :overline "green3" :weight ultra-bold :width semi-expanded))))
 '(tabbar-selected-face ((t (:inherit tabbar-default-face :background "black" :foreground "grey" :box (:line-width -1 :color "grey" :style released-button)))))
 '(tabbar-separator ((t (:background "black" :distant-foreground "red" :foreground "brown" :height 0.1 :width condensed))))
 '(tabbar-unselected ((t (:background "black" :foreground "dark green" :overline "dark green" :height 1.3))))
 '(tabbar-unselected-face ((t (:inherit tabbar-default-face :background "black" :foreground "white" :box (:line-width -1 :color "black" :style pressed-button)))))
 )

(provide 'awesome-tab)

;;; awesome-tab.el ends here
