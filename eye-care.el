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

(defvar eye-care-display-duration 60)

(defvar eye-care-timer nil)

(defvar eye-care-interval (* 30 60))

;;; Functions

(defun eye-care-check ()
  "Wrapper show eye-care-display-message."
  (eye-care-display-message "撸代码有段时间了，请休息一下眼睛，健康要紧！\nPlease rest eyes"))

(defun eye-care-display-message (string)
  "Display a reminder about an eye care."
  (if eye-care-audibe (beep 1))
  (cond ((eq eye-care-display-format 'window)
         (let ((time (format-time-string "%a %b %c "))
               err)
           (message time)
           ;;show
           (funcall eye-care-disp-window-function string))
         ;;定时关闭buffer
         (run-at-time (format "%d sec" eye-care-display-duration)
                      nil
                      eye-care-delete-window-function))))


(defun eye-care-disp-window (eye-care-msg)
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
    (setq eye-care-msg (list eye-care-msg))
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
                                               (nth 3 (window-edges w))))
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

;;;###autoload
(defun eye-care-activate (&optional arg)
  "Toggle eye care"
  (interactive "P")
  (let ((eye-care-active eye-care-timer))
    (when eye-care-timer
      (cancel-timer eye-care-timer)
      (setq eye-care-timer nil))
    (if eye-care-active
        (progn
          (message "start eye-care")
          (setq eye-care-timer (run-at-time t eye-care-interval 'eye-care-check)))
      (message "eye care disabled"))))

;;;###autoload
(defun eye-care-start ()
  "Start eye care."
  (interactive)
  (if eye-care-timer
      (message "Eye care already running!")
    (progn
      (when eye-care-timer
        (cancel-timer eye-care-timer)
        (setq eye-care-timer nil))
      (when (eq eye-care-display-duration eye-care-interval)
        (setq eye-care-display-duration
              (- eye-care-display-duration 1)))
      (message "start eye care!")
      (setq eye-care-timer (run-at-time t eye-care-interval 'eye-care-check)))))

;;;###autoload
(defun eye-care-stop ()
  "Stop eye care."
  (interactive)
  (message "stop eye care!")
  (when eye-care-timer
    (cancel-timer eye-care-timer)
    (setq eye-care-timer nil)))

;;;###autoload
(defun set-eye-care-interval (interval)
  "Set eye care `interval'"
  (interactive "nEnter eye care interval mins:")
  (setq eye-care-interval (* interval 60)))

;;;###autoload
(defun set-eye-care-duration (duration)
  "Set eye care `duration'"
  (interactive "nEnter eye care message duration seconds:")
  (setq eye-care-display-duration duration))

(provide 'eye-care)

;;; eye-care.el ends here
