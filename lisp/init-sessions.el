;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

;; Restore histories and registers after saving

(setq-default history-length 100)
(add-hook 'after-init-hook 'savehist-mode)

(require-package 'session)

(setq session-save-file (locate-user-emacs-file ".session"))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
(setq session-save-file-coding-system 'utf-8)

(add-hook 'after-init-hook 'session-initialize)

(provide 'init-sessions)
;;; init-sessions.el ends here
