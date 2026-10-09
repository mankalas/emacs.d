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


;;; org-mcp

;; Only these files are readable and writable over MCP.  The archives
;; are included so that `org-archive-subtree' can write to them.
(when (maybe-require-package 'org-mcp)
  (setq org-mcp-allowed-files
        (mapcar (lambda (f) (expand-file-name f org-directory))
                '("inbox.org" "gtd.org" "execute.org" "delegation.org"
                  "inbox.org_archive" "execute.org_archive")))
  ;; Clients register the tools themselves through emacs-mcp-stdio.sh
  ;; (--init-function=org-mcp-enable), which isn't autoloaded, so load
  ;; the package and start accepting requests once init is done.
  (add-hook 'after-init-hook
            (lambda ()
              (require 'org-mcp)
              (mcp-server-lib-start))))

(provide 'init-local)
;;; init-local.el ends here
