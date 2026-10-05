;; -*- lexical-binding: t; -*-

(setq org-directory (expand-file-name "~/Documents/org/")
      org-default-notes-file (concat org-directory "inbox.org")
      org-refile-targets '((nil :maxlevel . 5)   (org-agenda-files :maxlevel . 5) ((concat org-directory "someday.org") :maxlevel . 5)))

(provide 'init-preload-local)
