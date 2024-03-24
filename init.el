;;; init.el

;; Start-up

;;; Produce backtraces when errors occur
(setq debug-on-error t)

;;; Add 'lisp' modules into load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Measure startup time
(require 'init-benchmarking)
