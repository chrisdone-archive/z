;;; z.el --- Syntax highlighting module for Z

;; Copyright (C) 2012 Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;;; Code:

(add-to-list 'auto-mode-alist '("\\.z\\'" . z-mode))

;;;###autoload
(define-derived-mode z-mode fundamental-mode "Z"
  "Major mode for Z files.")

(provide 'z-mode)
;;; z.el ends here
