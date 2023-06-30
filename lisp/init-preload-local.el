(setq org-directory "~/Dropbox/org/"
      org-agenda-files (mapcar (lambda (f) (concat org-directory f)) '("gtd.org" "tickler.org" "inbox.org"))
      org-default-notes-file (concat org-directory "inbox.org")
      org-refile-targets '((nil :maxlevel . 5)   (org-agenda-files :maxlevel . 5) ((concat org-directory "someday.org") :maxlevel . 5)))

(provide 'init-preload-local)
