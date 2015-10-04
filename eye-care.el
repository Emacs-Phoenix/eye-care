;;; eye-care.el --- uglify HTML, CSS and JavaScript/JSON by js-beautify

;; Copyright (C) 2015 Aby Chan  <abchan@outlook.com>

;; Author: Aby Chan <abychan@outlook.com>
;; Version: 0.1
;; URL: https://github.com/Emacs-Phoenix/eye-care

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; For more information, See URL https://github.com/Emacs-Phoenix/eye-care.

;;; Commentary:
;;nil now

;;; Code:


(defvar eye-care-audibe nil)

(defvar eye-care-display-format 'window
  "How eye-care reminders should be displayed.
The options are:
window - use a separate window
echo   - use the echo area
nil    - no visible reminder.")

(defconst eye-care-buffer-name "*eye-care-buf*"
  "Name of the appointments buffer.")

(defvar eye-care-disp-window-function 'eye-care-disp-window)
(defvar eye-care-delete-window-function 'eye-care-delete-window)

(defvar eye-care-display-duration 10)

;;; Functions

(defun eye-care-display-message (string mins)
  "Display a reminder about an eye care."
  (if eye-care-audibe (beep 1))
  (and (listp mins)
       (= (length mins) 1)
       (setq mins (car mins)
             string (car string)))
  (cond ((eq eye-care-display-format 'window)
         (let ((time (format-time-string "%a %b %c "))
               err)
           (condition-case err
               (funcall eye-care-disp-window-function
                        (if (listp mins)
                            (mapcar 'number-to-string mins)
                          (number-to-string mins))
                        time string)
             (wrong-type-argument
              (if (not (listp mins))
                  (signal (car err) (cdr err))
                (message "Argtype error in `eye-care-window-function'")

                (funcall eye-care-disp-window-function
                         (number-to-string (car mins)) time
                         (car string))))))
         (run-at-time (format "%d sec" eye-care-display-duration)
                      nil
                      eye-care-delete-window-function))))


(defun eye-care-disp-window (min-to-app new-time eye-care-msg)
  "Display appointment due in MIN-TO-APP (a string) minutes.
NEW-TIME is a string giving the current date.
Displays the appointment message EYE-CARE-MSG in a separate buffer.
The arguments may also be lists, where each element relates to a
separate appointment."
  (let ((this-window (selected-window))
        (eye-care-disp-buf (get-buffer-create eye-care-buffer-name)))
    (when (minibufferp)
      (other-window 1)
      (and (minibufferp) (display-multi-frame-p) (other-frame 1)))
    (if (cdr (assq 'unsplittable (frame-parameters)))
        ;; In an unsplittable frame, use something somewhere else.
        (progn
          (set-buffer eye-care-disp-buf)
          (display-buffer eye-care-disp-buf))
      (unless (or (special-display-p (buffer-name eye-care-disp-buf))
                  (same-window-p (buffer-name eye-care-disp-buf)))
        ;; By default, split the bottom window and use the lower part.
        (eye-care-select-lowest-window)
        ;;Split the window,unless it's too small to do so.
        (when (>= (window-height) (* 2 window-min-height))
          (select-window (split-window))))
      (switch-to-buffer eye-care-disp-buf))
    (or (listp min-to-app)
        (setq min-to-app (list min-to-app)
              eye-care-msg (list eye-care-msg)))
    (setq buffer-read-only nil
          buffer-undo-list t)
    (erase-buffer)
    ;;insert text
    (insert (car eye-care-msg))
    (shrink-window-if-larger-than-buffer (get-buffer-window eye-care-disp-buf t))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (raise-frame)
    (select-window this-window)))

(defun eye-care-select-lowest-window ()
  "Select the lowest window on the frame."
  (let ((lowest-window (selected-window))
        ;;window-edges ret => left top right bottom (current-window)
        ;;(nth 3 => get bottom
        (bottom-edge (nth 3 (window-edges)))
        next-bottom-edge)
    ;;walk(traversal) all windows
    (walk-windows (lambda (w)
                    (when (< bottom-edge (setq next-bottom-edge
                                               (nth 3 (window-edges))))
                      (setq bottom-edge next-bottom-edge
                            lowest-window w))) 'nomini)
    (select-window lowest-window)))

(defun eye-care-delete-window ()
  "Function called to undisplay eye-care messages.
Usually just deletes the appointment buffer."
  (let ((window (get-buffer-window eye-care-buffer-name t)))
    (and window
         (or (eq window (frame-root-window (window-frame window)))
             (delete-window window))))
  (kill-buffer eye-care-buffer-name)
  (if eye-care-audibe
      (beep 1)))

(eye-care-display-message "hi" 1)

(provide 'eye-care)

;;; eye-care.el ends here
