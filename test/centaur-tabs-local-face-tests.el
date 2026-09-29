;;; centaur-tabs-local-face-tests.el --- Icon backgrounds -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'face-remap)
(require 'centaur-tabs)

(ert-deftest centaur-tabs-icon-uses-displaying-buffer-face-remap ()
  (let ((centaur-tabs-icon-type 'nerd-icons)
        (target (generate-new-buffer " *centaur-icon-target*")))
    (unwind-protect
        (cl-letf (((symbol-function 'centaur-tabs--icon-for-buffer)
                   (lambda (&rest _) (propertize "X" 'face '(:family "Test Icon Font"))))
                  ((symbol-function 'char-displayable-p) (lambda (_) t))
                  ((symbol-function 'face-background)
                   (lambda (&rest _) "black")))
          (with-temp-buffer
            (let ((cookie (face-remap-add-relative
                           'centaur-tabs-selected '(:background "white"))))
              (unwind-protect
                  (let* ((icon (centaur-tabs-icon (cons target nil)
                                                  'centaur-tabs-selected t))
                         (face (get-text-property 0 'face icon)))
                    (should (equal (plist-get face :background) "white"))
                    (should (equal (plist-get (plist-get face :inherit) :family)
                                   "Test Icon Font")))
                (face-remap-remove-relative cookie))))
          (with-temp-buffer
            (let ((icon (centaur-tabs-icon (cons target nil)
                                           'centaur-tabs-selected t)))
              (should (equal (plist-get (get-text-property 0 'face icon) :background)
                             "black")))))
      (kill-buffer target))))

;;; centaur-tabs-local-face-tests.el ends here
