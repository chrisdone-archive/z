;;; z.el --- Z mode

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; Author:    Chris Done <chrisdone@gmail.com>
;; Created:   26-jan-2014
;; Version:   0.0.0
;; Keywords:  development
;; Stability: unstable

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Z.

;;; Code:

(define-derived-mode z-mode fundamental-mode
  "Z" "Major mode for writing Z code.")
