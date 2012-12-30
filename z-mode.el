;;; z.el --- Syntax highlighting module for Z

;; Copyright (C) 2012 Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;;; Code:

(add-to-list 'auto-mode-alist '("\\.zz\\'" . z-mode))

;;;###autoload
(define-derived-mode z-mode fundamental-mode "Z"
  "Major mode for Z files."
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((z-font-lock-keywords)))
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-end) ""))

(defconst z-keywords
  (eval-when-compile
    (regexp-opt
     '("defun" "defmacro" "fn" "if" "do")))
  "Z keywords.")

(defconst z-font-lock-keywords
  (list
   ;; Fontify keywords
   (cons
    (concat "\\<\\(" z-keywords "\\)\\>")
    '(1 font-lock-keyword-face))
   '("^-- .*" . font-lock-comment-face))
  "Subdued level highlighting for z mode.")

(provide 'z-mode)
;;; z.el ends here
