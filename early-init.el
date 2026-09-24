;;; early-init.el --- Emacs 27+ pre-initialisation config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; Native compilation is broken on recent macOS: libgccjit derives the
;; deployment target from the Darwin kernel version (darwin 27 => "18.0"),
;; but Apple's numbering jumped straight from macOS 15 to 26, so clang
;; rejects everything in the 16-25 gap and every .eln build dies with
;; "error invoking gcc driver".  Naming a version clang still accepts is
;; enough to get past it; the deployment target of an .eln is otherwise
;; irrelevant, so pin the highest pre-gap release rather than chase the
;; current one.
(defvar native-comp-driver-options)
(when (and (eq system-type 'darwin)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; `comp' is not loaded yet, so set the option rather than push onto it:
  ;; its `defcustom' will leave whatever we put here alone.
  (setq native-comp-driver-options
        (cons "-mmacosx-version-min=15.0"
              (bound-and-true-p native-comp-driver-options))))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
