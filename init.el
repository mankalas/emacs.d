;;; init.el --- Load the full configuration

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)


(setq mac-command-modifier 'meta)

;; Melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

;; Custom
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; PATH
(require-package 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))


(when (or (memq window-system '(mac ns x pgtk))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))


;; Desktop
(desktop-save-mode 1)

;; magit
(require-package 'magit)

;; theme
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;; GUI
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)
(setq visible-bell 1)
(fset 'yes-or-no-p 'y-or-n-p)

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

;; Editing
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;; Cut/copy the current line if no region is active
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;;; Duplicate line
(require-package 'move-dup)
(require 'move-dup)
(define-key global-map [M-up] 'move-dup-move-lines-up)
(define-key global-map [M-down] 'move-dup-move-lines-down)
(define-key global-map "\C-cd" 'move-dup-duplicate-down)
(define-key global-map "\C-cu" 'move-dup-duplicate-up)

;; Org

;;; GUI
(require-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;;; So long lines flow and adjust to the width of the window
(add-hook 'org-mode-hook 'visual-line-mode)

;;; Default dirs
(let ((default-directory "~/Dropbox/org/"))
  (setq org-default-notes-file (expand-file-name "notes.org"))
  (setq refile-file-path (expand-file-name "todo.org"))
  (setq events-file-path (expand-file-name "events.org"))
  (setq roadtrip-file-path (expand-file-name "roadtrip.org"))
  (setq org-journal-dir (expand-file-name "journal")))

;;; Journal
(require-package 'org-journal)
(require 'org-journal)
(define-key global-map "\C-cj" 'org-journal-new-entry)

;;; Capture templates
(setq org-capture-templates
      `(("t" "todo" entry (file refile-file-path)
         "* TODO %?")
	("n" "note" entry (file "")
	 "* %? :NOTE:") ; "" => `org-default-notes-file'
	("e" "event" entry (file events-file-path)
	 "* %?\nSCHEDULED: %T")
	("a" "arrival" entry (file roadtrip-file-path)
	 "** Arrival\n\n*** TODO Change car insurance address\n\n*** TODO Text Marina\n\n")))


;;; Keybindings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)

;;; Agenda
(setq org-agenda-files '("~/Dropbox/org/"))

;;; Refiling
(setq org-refile-use-cache nil)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;;; Org Babel
(setq org-confirm-babel-evaluate nil)
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (shell . t)
     )))

;; Search
(require-package 'anzu)
(require 'anzu)
(add-hook 'after-init-hook 'global-anzu-mode)
(setq anzu-mode-lighter "")
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

;; Projectile
(require-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; YAML
(require-package 'yaml-mode)
(require 'yaml-mode)

;; Ledger
(require-package 'ledger-mode)
(require 'ledger-mode)
(setq ledger-binary-path "/usr/local/bin/ledger")

;; LSP
(maybe-require-package 'eglot)
(maybe-require-package 'consult-eglot)

;; Haskell
(when (maybe-require-package 'haskell-mode)
  (add-hook 'haskell-mode-hook 'eglot-ensure)


;;; CamelCase handling
  (add-hook 'haskell-cabal-mode 'subword-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Source code helpers

  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  (when (maybe-require-package 'reformatter)
    (reformatter-define hindent
      :program "hindent"
      :lighter " Hin")

    (defalias 'hindent-mode 'hindent-on-save-mode)
    (add-hook 'haskell-mode-hook 'hindent-on-save-mode)

    (reformatter-define ormolu
      :program "ormolu"
      :lighter " Orm"))

  (with-eval-after-load 'haskell-mode
    (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
    (define-key haskell-mode-map (kbd "C-o") 'open-line)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))


  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'haskell-mode))


  (define-minor-mode stack-exec-path-mode
    "If this is a stack project, set `exec-path' to the path \"stack exec\" would use."
    nil
    :lighter ""
    :global nil
    (if stack-exec-path-mode
	(when (and (executable-find "stack")
                   (locate-dominating-file default-directory "stack.yaml"))
          (let ((stack-path (replace-regexp-in-string
                             "[\r\n]+\\'" ""
                             (shell-command-to-string (concat "stack exec -- sh -c "
                                                              (shell-quote-argument "echo $PATH"))))))
            (setq-local exec-path (seq-uniq (parse-colon-path stack-path) 'string-equal))
            (make-local-variable 'process-environment)
            (setenv "PATH" (string-join exec-path path-separator))))
      (kill-local-variable 'exec-path)
      (kill-local-variable 'process-environment)))

  (add-hook 'haskell-mode-hook 'stack-exec-path-mode)

  (when (maybe-require-package 'dhall-mode)
    (add-hook 'dhall-mode-hook 'stack-exec-path-mode)))
