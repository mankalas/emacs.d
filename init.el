;;; init.el

;; Start-up

;;; Produce backtraces when errors occur
(setq debug-on-error t)

;;; Add 'lisp' modules into load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Measure startup time
(require 'init-benchmarking)

;;; OSX keys
(defconst *is-a-mac* (eq system-type 'darwin))
(require 'init-osx-keys)

;;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH



;; Load configs for specific features and modes

;;; Extra packages which don't require any configuration
(maybe-require-package 'scratch)

(require 'init-themes)
(require 'init-gui-frames)


(provide 'init)
