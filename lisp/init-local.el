;; -*- lexical-binding: t; -*-

(require-package 'zenburn-theme)

(custom-set-variables '(custom-enabled-themes '(zenburn))
                      '(calendar-week-start-day 1))


;;; Org

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)"  "AREA(a)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "TODO")

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit))

(setq org-journal-dir (expand-file-name "journal" org-directory)
      org-journal-file-type 'weekly
      org-journal-enable-agenda-integration t)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(global-set-key (kbd "C-c s") 'org-journal-search)
(global-set-key (kbd "C-c b") 'org-journal-previous-entry)
(require-package 'org-journal)

(provide 'init-local)
;;; init-local.el ends here
