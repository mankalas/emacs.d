;;; init-roam.el --- Roam-mode config -*- lexical-binding: t -*-
;;; Commentary:


;;; Code:

(maybe-require-package 'org-roam)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Dropbox/org/roam") ;; your note folder
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

(provide 'init-roam)
;;; init-roam.el ends here
