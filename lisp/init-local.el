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

;; Set `org-modules' rather than adding to it: the default value pulls in
;; `ol-gnus', `ol-docview' and `ol-eww', so opening any org file loads the
;; whole of Gnus and doc-view.  That costs about 750ms of startup, and none
;; of those link types are ones we store or follow.
(with-eval-after-load 'org
  (setq org-modules '(org-habit))
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
                '("inbox.org" "gtd.org"
                  "inbox.org_archive" "gtd.org_archive" "execute.org_archive")))
  ;; Clients register the tools themselves through emacs-mcp-stdio.sh
  ;; (--init-function=org-mcp-enable), which isn't autoloaded, so load
  ;; the package and start accepting requests once init is done.  Do it
  ;; on an idle timer rather than `after-init-hook': org-mcp requires
  ;; `org', `org-agenda', `org-archive' and `org-id' at load time, which
  ;; is around half a second we would otherwise spend before the first
  ;; frame is usable.  No client connects in the first second.
  (run-with-idle-timer 1 nil
                       (lambda ()
                         (require 'org-mcp)
                         (mcp-server-lib-start))))

(provide 'init-local)
;;; init-local.el ends here
