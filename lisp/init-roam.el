;;; init-roam.el --- Roam-mode config -*- lexical-binding: t -*-
;;; Commentary:


;;; Code:

(maybe-require-package 'org-roam)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/org/roam") ;; your note folder
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("m" "meeting" entry
      "* %<%Y-%m-%d %H:%M> %^{Meeting} :meeting:\n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\nWith: %?\nRe: \n** Notes\n** Decisions\n** Actions\n"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n m" . init-roam-capture-meeting))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(defun init-roam-capture-meeting ()
  "Capture meeting notes into today's roam daily."
  (interactive)
  (require 'org-roam-dailies)
  (org-roam-dailies-capture-today nil "m"))

(provide 'init-roam)
;;; init-roam.el ends here
