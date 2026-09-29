;;; centaur-tabs-theme-cache-tests.el --- Theme cache regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'centaur-tabs)

(ert-deftest centaur-tabs-theme-change-invalidates-unvisited-tabsets ()
  "A global theme change must invalidate even cached inactive tabsets."
  (let* ((main-buffer (generate-new-buffer " *ct-theme-main*"))
         (other-buffer (generate-new-buffer " *ct-theme-other*"))
         (centaur-tabs-buffer-list-function
          (lambda () (list main-buffer other-buffer)))
         (centaur-tabs-buffer-groups-function
          (lambda () (list (if (eq (current-buffer) other-buffer)
                                "other" "main"))))
         (centaur-tabs-set-icons nil)
         (centaur-tabs-show-new-tab-button nil)
         (centaur-tabs-show-navigation-buttons nil))
    (unwind-protect
        (progn
          (centaur-tabs-mode 1)
          (switch-to-buffer main-buffer)
          (centaur-tabs-line)
          (let ((main (centaur-tabs-current-tabset t)))
            (switch-to-buffer other-buffer)
            (centaur-tabs-line)
            (let ((other (centaur-tabs-current-tabset t)))
              (should-not (eq main other))
              (centaur-tabs-set-template main '("old-main"))
              (centaur-tabs-set-template other '("old-other"))
              (should (centaur-tabs-template main))
              (should (centaur-tabs-template other))
              ;; The buffer-list hook need not expose cached tabsets after a
              ;; workspace/group switch, but their templates still exist.
              (dolist (buffer (list main-buffer other-buffer))
                (with-current-buffer buffer
                  (setq centaur-tabs-current-tabset nil)))
              (centaur-tabs--after-load-theme)
              (should-not (centaur-tabs-template main))
              (should-not (centaur-tabs-template other)))))
      (centaur-tabs-mode -1)
      (dolist (buffer (list main-buffer other-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'centaur-tabs-theme-cache-tests)
;;; centaur-tabs-theme-cache-tests.el ends here
