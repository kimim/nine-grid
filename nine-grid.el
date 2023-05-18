;;; nine-grid.el --- Move frame to 1 of nine grids

;; Copyright (c) 2023 Kimi Ma <kimi.im@outlook.com>

;; Author:  Kimi Ma <kimi.im@outlook.com>
;; URL: https://github.com/kimim/nine-grid
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Because Emacs cannot show continuous pages at the same time, it is
;; difficult to keep track of the remaining text in current page, this
;; minor mode add a posframe indicator to tell you the line from
;; scroll up.
;;

;;; Code:

(defun nine-grid-set-frame (top left height width)
  (let ((frame (selected-frame)))
    (set-frame-position frame left top)
    (set-frame-height frame height)
    (set-frame-width frame width)))

(defun nine-grid-screen-width ()
  (/ (display-pixel-width)
     (frame-char-width)))

(defun nine-grid-screen-height ()
  (/ (display-pixel-height)
     (frame-char-height)))

(defun nine-grid-xy (x y)
  (interactive)
  (when window-system
    ;; top, left ... must be integer
    (let* ((width (floor (/ (nine-grid-screen-width) 3.1)))
           (height (floor (/ (nine-grid-screen-height) 3.5)))
           (top (/ (* y (display-pixel-height)) 3))
           (left (/ (* x (display-pixel-width)) 3)))
      (nine-grid-set-frame
       top left height width))))

(defun nine-grid (&optional n)
  (interactive)
  (let ((n (or n 5)))
    (let* ((x (mod (1- n) 3))
           (y (/ (1- n) 3)))
      (nine-grid-xy x y))))

(defun nine-grid-0 ()
  (interactive)
  (when window-system
    ;; top, left ... must be integer
    (let* ((width (floor (* 0.8 (nine-grid-screen-width))))
           (height (floor (* 0.8 (nine-grid-screen-height))))
           (top (/ (display-pixel-height) 10))
           (left (/ (display-pixel-width) 10)))
      (nine-grid-set-frame
       top left height width))))

(defvar nine-grid-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x y 0") #'nine-grid-0)
    (define-key map (kbd "C-x y 1") (lambda () (interactive) (nine-grid 1)))
    (define-key map (kbd "C-x y 2") (lambda () (interactive) (nine-grid 2)))
    (define-key map (kbd "C-x y 3") (lambda () (interactive) (nine-grid 3)))
    (define-key map (kbd "C-x y 4") (lambda () (interactive) (nine-grid 4)))
    (define-key map (kbd "C-x y 5") (lambda () (interactive) (nine-grid 5)))
    (define-key map (kbd "C-x y 6") (lambda () (interactive) (nine-grid 6)))
    (define-key map (kbd "C-x y 7") (lambda () (interactive) (nine-grid 7)))
    (define-key map (kbd "C-x y 8") (lambda () (interactive) (nine-grid 8)))
    (define-key map (kbd "C-x y 9") (lambda () (interactive) (nine-grid 9)))
    map)
  "Keymap for `nine-grid-minor-mode'.")

;;;###autoload
(define-minor-mode nine-grid-minor-mode
  "Minor mode to move frame to one of nine grids.
\\{nine-grid-minor-mode-map}"
  :init-value nil
  :lighter " 9g"
  :keymap nine-grid-minor-mode-map)

;;;###autoload
(defun turn-on-nine-grid-minor-mode ()
  "Turn on `nine-grid-minor-mode'."
  (interactive)
  (nine-grid-minor-mode +1))

;;;###autoload
(define-globalized-minor-mode nine-grid-mode
  nine-grid-minor-mode
  turn-on-nine-grid-minor-mode)

(provide 'nine-grid)

;;; nine-grid.el ends here
