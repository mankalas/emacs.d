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

(defun sanityinc/org-dir-files (subdir regexp)
  "Return the files matching REGEXP in SUBDIR of `org-directory'.
Yields nil rather than signalling when the directory is absent, so that
an org tree which hasn't synced yet doesn't take startup down with it."
  (let ((dir (expand-file-name subdir org-directory)))
    (when (file-directory-p dir)
      (directory-files dir t regexp))))

(setq-default org-agenda-files
              (append
               ;; All .org files in the main directory
               (sanityinc/org-dir-files "." "\\.org$")
               (sanityinc/org-dir-files "roam" "\\.directory$")
;               (sanityinc/org-dir-files "journal" ".*")
               ))

(setq org-journal-dir (expand-file-name "journal" org-directory)
      org-journal-file-type 'weekly
      org-journal-enable-agenda-integration t)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(global-set-key (kbd "C-c s") 'org-journal-search)
(global-set-key (kbd "C-c b") 'org-journal-previous-entry)
(global-set-key (kbd "C-c b") 'org-journal-next-entry)
(require-package 'org-journal)

(provide 'init-local)
;;; init-local.el ends here
