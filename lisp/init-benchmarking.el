;;; init-benchmarking.el

(defun custom/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun custom/show-init-time ()
  (message "Init completed in %.2fms"
           (custom/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'custom/show-init-time)

(provide 'init-benchmarking)
