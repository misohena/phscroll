;;; phscroll.el --- Partial horizontal scroll     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Usage:

;; 1. (require 'phscroll)
;; 2. Select region you want to hscroll.
;; 3. M-x phscroll-region

;; Under `org-mode', use `org-phscroll-mode' instead.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; TODO:
;; - [X] truncate-lines によって動作を変える。truncate-linesのときは何もしない。切り替えを検出できるか
;; - [X] remove-overlaysのところ
;; - phscroll-update-area-displayが遅い
;; - [X] 既存の水平スクロール操作に対応?(scroll-left, scroll-right)
;; - [X] 複数ウィンドウの挙動、特に左右に分割した場合で左右のサイズが異なる場合はどうしようもない。最小幅を使うしか？　何もしない方が良い？ →area毎に幅を持って変化をチェックする。

;;;; Customization

(defgroup phscroll nil
  "Creates horizontally scrolling areas in a buffer."
  :prefix "phscroll-"
  :group 'convenience)

(defcustom phscroll-use-fringe t
  "Specifies how to indicate the possibility of scrolling left or right.
Use fringe when non-nil.
If it is nil, it is indicated by the < and > characters."
  :type 'boolean
  :group 'phscroll)

(defvar phscroll-margin-right-additional 0) ;;for stability

(defun phscroll-margin-right ()
  (+ phscroll-margin-right-additional
     (if phscroll-use-fringe 1 2)))

(defcustom phscroll-scroll-left-right-move-point t
  "If non-nil, move point with left/right scroll commands."
  :type 'boolean
  :group 'phscroll)

(defcustom phscroll-scroll-left-right-reverse-direction nil
  "If non-nil, reverse direction of left/right scroll command."
  :type 'boolean
  :group 'phscroll)

(defcustom phscroll-calculate-in-pixels nil
  "Experimental."
  :type 'boolean
  :group 'phscroll)

;;;; Logging

(eval-and-compile
  (defcustom phscroll-log-generate nil
    "Control log code generation at compile time."
    :group 'phscroll :type '(choice boolean regexp))

  (defun phscroll-log-generate-p (format-string)
    (if (stringp phscroll-log-generate)
        (string-match-p phscroll-log-generate format-string)
      phscroll-log-generate)))

(defcustom phscroll-log-output nil
  "Control log message output at runtime."
  :group 'phscroll :type '(choice boolean regexp))

(defsubst phscroll-log-output-p (format-string)
  (if (stringp phscroll-log-output)
      (string-match-p phscroll-log-output format-string)
    phscroll-log-output))

(defmacro phscroll-log-expr (format-string expr &optional no-log-expr)
  (if (phscroll-log-generate-p format-string)
      expr
    no-log-expr))

(defmacro phscroll-log (format-string &rest args)
  "Log a message if enabled by `phscroll-log-generate' and
`phscroll-log-output'."
  (when (phscroll-log-generate-p format-string)
    `(when (phscroll-log-output-p ,format-string)
       (message ,(concat "Phscroll " format-string) ,@args))))

(defun phscroll-log-watch (regexp)
  "Enable log output for logs whose format-string matches REGEXP.

The format-string is the first argument passed to the `phscroll-log' macro.

If an empty string is specified, all log output is enabled.

To output logs, `phscroll-log-generate' must be set to non-nil before
loading the library."
  (interactive
   (list (read-regexp
          "Regexp to match format-string of phscroll-log (empty for all)")))
  (message "Log output enabled")
  (setq phscroll-log-output
        (if (and (stringp regexp) (not (string-empty-p regexp)))
            regexp
          t)))

(defun phscroll-log-unwatch ()
  "Disable log output."
  (interactive)
  (setq phscroll-log-output nil)
  (message "Log output disabled"))


;;;; Basic Commands

(defvar-local phscroll-truncate-lines nil) ;; to detect truncate-lines change

(define-minor-mode phscroll-mode
  "Partial horizontal scroll mode.

Users do not normally need to enable this mode directly.
`phscroll-region' enables this mode automatically.
Under `org-mode', use `org-phscroll-mode' instead."
  :global nil
  (cond
   (phscroll-mode
    (setq-local phscroll-truncate-lines truncate-lines)
    (add-hook 'post-command-hook #'phscroll-on-post-command nil t)
    (add-hook 'window-scroll-functions #'phscroll-on-window-scroll nil t)
    ;;(add-hook 'window-size-change-functions #'phscroll-on-window-size-changed nil t)
    (add-hook 'pre-redisplay-functions #'phscroll-on-pre-redisplay nil t)
    )
   (t
    (remove-hook 'post-command-hook #'phscroll-on-post-command t)
    (remove-hook 'window-scroll-functions #'phscroll-on-window-scroll t)
    ;;(remove-hook 'window-size-change-functions #'phscroll-on-window-size-changed t)
    (remove-hook 'pre-redisplay-functions #'phscroll-on-pre-redisplay t)
    )))

(defun phscroll-region (beg end)
  "Make a partial horizontal scrolling area.

The area to be created has a range from the line containing BEG
to the line containing END (however, if END is the beginning of
the line, that line is not included).

Any existing overlapping areas are destroyed."
  (interactive "r")
  (setq beg (phscroll-line-begin beg))
  (setq end (if (save-excursion (goto-char end) (bolp))
                end
              (phscroll-line-end end)))

  (when (< beg end)
    ;; turn on phscroll-mode
    (unless phscroll-mode
      (phscroll-mode))

    ;; create overlay and area object
    (let* ((overlap-areas (phscroll-enum-area beg end))
           (area (car overlap-areas)))
      (if area
          ;; reuse area
          (progn
            ;; reuse area
            (phscroll-area-move area beg end)
            (phscroll-update-area-display area)
            ;; destroy other overlap areas
            (cl-loop for ooa in (cdr overlap-areas)
                     do (phscroll-area-destroy ooa)))
        ;; create new area
        (setq area (phscroll-area-create beg end))
        (phscroll-update-area-display area t))
      area)))

(defun phscroll-delete-at (&optional pos)
  "Delete the area at POS."
  (interactive "d")
  (phscroll-area-destroy (phscroll-get-area-at pos)))

(defun phscroll-delete-all ()
  "Destroy all areas in the current buffer."
  (interactive)
  (cl-loop for area in (phscroll-enum-area)
           do (phscroll-area-destroy area)))

(defun phscroll-update-at (&optional pos)
  "Redisplay the area at POS."
  (interactive "d")
  (phscroll-update-area-display (phscroll-get-area-at pos) t))

(defun phscroll-merge-region (beg end &optional no-update)
  "Merge all areas that overlap the region BEG..END.

If NO-UPDATE is non-nil, this function will not call
`phscroll-update-area-display'."
  (interactive "r")
  (let* ((areas (sort (phscroll-enum-area beg end)
                      (lambda (a1 a2)
                        (< (phscroll-area-begin a1) (phscroll-area-begin a2)))))
         (result (car areas)))
    (setq areas (cdr areas))
    (while areas
      (phscroll-area-merge result (car areas))
      (setq areas (cdr areas)))
    (when (and result (not no-update))
      (phscroll-update-area-display result))
    result))

(defun phscroll-cover-region (beg end)
  "Cover the region BEG..END with a single phscroll area."
  (interactive "r")

  (if-let ((area (phscroll-merge-region beg end t)));;no-update=t
      (let ((area-beg (phscroll-area-begin area))
            (area-end (phscroll-area-end area)))
        (when (or (< beg area-beg) (< area-end end))
          (phscroll-area-move area
                              (min beg area-beg)
                              (max end area-end)))
        (phscroll-update-area-display area)
        area)
    (phscroll-region beg end)))

(defun phscroll-remove-region (beg end)
  "Remove phscroll areas from the region BEG..END.

Areas that overlap the region will be deleted, moved, or splitted."
  (interactive "r")

  (dolist (area (phscroll-enum-area beg end))
    (let ((area-beg (phscroll-area-begin area))
          (area-end (phscroll-area-end area)))
      (cond
       ;; keep area
       ((<= area-end beg) nil)
       ((<= end area-beg) nil)
       ;; delete area
       ((and (<= beg area-beg) (<= area-end end))
        (phscroll-area-destroy area))
       ;; split area
       ((and (< area-beg beg) (< end area-end))
        (phscroll-area-split area end)
        (phscroll-area-move area area-beg beg)
        (phscroll-update-area-display area);;unnecessary
        )
       ;; move area
       ((< area-beg beg)
        (phscroll-area-move area area-beg beg)
        (phscroll-update-area-display area);;unnecessary
        )
       ((< end area-end)
        (phscroll-area-move area end area-end)
        (phscroll-update-area-display area);;unnecessary
        )))))



;;;; Area Objects


(defun phscroll-area-create (beg end &optional evaporate
                                 init-scroll-column
                                 init-updated-ranges
                                 init-window-width)
  "Create a new scroll area object covering BEG to END.

EVAPORATE is the same as the overlay property of the same name.

INIT-SCROLL-COLUMN, INIT-UPDATED-RANGES, INIT-WINDOW-WIDTH are
for special cases."
  (phscroll-log "Area: Create %s~%s" beg end)
  (let* ((ov (make-overlay beg end))
         (area (list
                'phscroll ;;0
                (or init-scroll-column 0)  ;; 1:scroll-column
                ov ;; 2:overlay
                (append (list (cons -1 -1))
                        init-updated-ranges);; 3:updated-ranges
                (or init-window-width -1) ;; 4:window-width when last update-area-display
                )))
    (overlay-put ov 'phscroll t)
    (overlay-put ov 'phscroll-area area)
    (overlay-put ov 'modification-hooks (list #'phscroll-on-modified))
    (when evaporate
      (overlay-put ov 'evaporate t))
    (phscroll-area-set-keymap area)
    area))

(defun phscroll-area-destroy (area)
  "Destroy AREA, an scroll area object."
  (when area
    (let ((beg (phscroll-area-begin area))
          (end (phscroll-area-end area))
          (ov (phscroll-area-overlay area)))
      (phscroll-log "Area: Destroy %s~%s" beg end)
      (phscroll-remove-line-veils-region beg end)
      (delete-overlay ov))))


(defun phscroll-area-move (area beg end &optional update)
  "Set the endpoints of AREA to BEG and END.

Call `phscroll-update-area-display' if UPDATE is non-nil.

Return AREA."
  (when (and area (<= beg end))
    (let* ((ov (phscroll-area-overlay area))
           (old-beg (overlay-start ov))
           (old-end (overlay-end ov)))
      ;; If area range changed
      (when (or (/= old-beg beg)
                (/= old-end end))
        (phscroll-log "Area: Move %s~%s update:%s (Old:%s~%s)"
                      beg end update old-beg old-end)
        ;; Remove ranges outside the new area from updated ranges list
        ;; and remove left and right overlays
        (if (or (<= old-end beg) (<= end old-beg))
            ;; If there is no overlap between old and new, simply remove old
            (progn
              (phscroll-area-clear-updated-ranges area)
              (phscroll-remove-line-veils-region old-beg old-end))
          ;; If overlap, remove protruding part
          (when (< old-beg beg)
            (phscroll-area-remove-updated-range old-beg beg area)
            (phscroll-remove-line-veils-region old-beg beg))
          (when (< end old-end)
            (phscroll-area-remove-updated-range end old-end area)
            (phscroll-remove-line-veils-region end old-end)))
        ;; Shift relative positions in updated ranges list
        (when (/= old-beg beg)
          (phscroll-area-shift-updated-ranges-after old-beg (- old-beg beg)
                                                    area))

        ;; Move overlay
        (move-overlay ov beg end)

        ;; Update new area range
        (when update
          (phscroll-update-area-display area)))))
  area)

(defun phscroll-area-merge (to-area from-area &optional update)
  "Merge FROM-AREA to TO-AREA.

Call `phscroll-update-area-display' if UPDATE is non-nil.

Return TO-AREA."
  (when (and to-area from-area)
    (let* ((to-ov (phscroll-area-overlay to-area))
           (to-beg (overlay-start to-ov))
           (to-end (overlay-end to-ov))
           (from-ov (phscroll-area-overlay from-area))
           (from-beg (overlay-start from-ov))
           (from-end (overlay-end from-ov))
           (overlap-beg (max to-beg from-beg))
           (overlap-end (min to-end from-end))
           (new-beg (min to-beg from-beg))
           (new-end (max to-end from-end)))
      (phscroll-log "Area: Merge %s~%s and %s~%s"
                    to-beg to-end from-beg from-end)

      (if (/= (phscroll-area-get-window-width to-area)
              (phscroll-area-get-window-width from-area))
          (progn
            (phscroll-area-move to-area new-beg new-end)
            (move-overlay from-ov new-end new-end)
            (delete-overlay from-ov))
        ;; same window width

        ;; Remove updated-ranges in overlapping area from to-area and from-area.
        ;; If overlap, they may be broken.
        (when (< overlap-beg overlap-end)
          (phscroll-area-remove-updated-range overlap-beg overlap-end to-area)
          (phscroll-area-remove-updated-range overlap-beg overlap-end from-area))

        ;; Shift relative positions in updated-ranges
        (phscroll-area-shift-updated-ranges-after to-beg (- to-beg new-beg) to-area)
        (phscroll-area-shift-updated-ranges-after from-beg (- from-beg new-beg) from-area)

        ;; Concat updated-ranges list
        (phscroll-area-updated-ranges-set
         to-area
         (let* ((lower-ranges (phscroll-area-updated-ranges-get
                               (if (< to-beg from-beg) to-area from-area)))
                (upper-ranges (phscroll-area-updated-ranges-get
                               (if (< to-beg from-beg) from-area to-area)))
                (last-lower (car (last lower-ranges)))
                (first-upper (car upper-ranges)))

           ;; Merge last of lower ranges and first of upper ranges
           ;;..(last-beg . last-end=first-beg)(last-end=first-beg . first-end)..
           ;;..(last-beg . first-end)..
           (when (and last-lower
                      first-upper
                      (= (cdr last-lower) (car first-upper)))
             (setcdr last-lower (cdr first-upper))
             (setq upper-ranges (cdr upper-ranges)))

           ;; Concat list
           (nconc lower-ranges upper-ranges)))

        ;; Move overlay
        (move-overlay to-ov new-beg new-end)
        (move-overlay from-ov new-end new-end)

        ;; Delete from-ov
        (delete-overlay from-ov)))
    (when update
      (phscroll-update-area-display to-area)))
  to-area)

(defun phscroll-area-split (area pos)
  "Split AREA at POS.

AREA will be the first half of the divided area.

Return a new area that is the second half of the divided area."
  (when area
    (let* ((area-ov (phscroll-area-overlay area))
           (area-beg (overlay-start area-ov))
           (area-end (overlay-end area-ov)))
      (when (and (< area-beg pos) (< pos area-end))
        (phscroll-log "Area: Split %s~%s at %s" area-beg area-end pos)

        (let ((prev-r (phscroll-area-updated-ranges-head area))
              (rel-pos (- pos area-beg))
              new-updated-ranges)
          ;; Find first range r.end > pos
          (while (and (cdr prev-r) (<= (cdadr prev-r) rel-pos))
            (setq prev-r (cdr prev-r)))
          ;; If r.beg < pos, split r
          (if (< (caadr prev-r) rel-pos)
              (progn
                (setq new-updated-ranges
                      (cons
                       (cons rel-pos (cdadr prev-r))
                       (cddr prev-r)))
                (setf (cdadr prev-r) rel-pos)
                (setf (cddr prev-r) nil))
            ;; pos <= r.beg and r.end
            (setq new-updated-ranges (cdr prev-r))
            (setcdr prev-r nil))
          ;; Move overlay first half
          (move-overlay area-ov area-beg pos)

          ;; Create a area second half
          (let ((new-area (phscroll-area-create
                           pos area-end
                           (overlay-get area-ov 'evaporate)
                           (phscroll-get-scroll-column area)
                           new-updated-ranges
                           (phscroll-area-get-window-width area))))
            ;; Shift updated ranges
            (phscroll-area-shift-updated-ranges-after pos (- area-beg pos) new-area)
            new-area))))))

;;;;; Area Scroll Position

(defconst phscroll-interactive-scroll-commands
  '(phscroll-set-scroll-column
    phscroll-scroll-left
    phscroll-scroll-right
    phscroll-recenter
    phscroll-recenter-left-right
    phscroll-mwheel-scroll-left
    phscroll-mwheel-scroll-right))

(defun phscroll-get-scroll-column (&optional area)
  "Return the horizontal scroll position (in columns) of AREA.

If AREA is nil, use the scroll area containing the current point."
  (nth 1 (or area (phscroll-get-current-area))))

(defun phscroll-set-scroll-column (pos &optional area)
  "Set the horizontal scroll position (in columns) of AREA to POS.

If AREA is nil, use the scroll area containing the current point."
  (interactive "nColumn: ")
  (when (null area)
    (setq area (phscroll-get-current-area)))
  (when (< pos 0)
    (setq pos 0))
  (when (and area (not (= (phscroll-get-scroll-column area) pos)))
    (setcar (nthcdr 1 area) pos)
    (phscroll-update-area-display area t)))

(defun phscroll-add-scroll-column (delta &optional area)
  "Add DELTA to the horizontal scroll position (in columns) of AREA.

If AREA is nil, use the scroll area containing the current point."
  (when (null area)
    (setq area (phscroll-get-current-area)))
  (when area
    (phscroll-set-scroll-column
     (+ (phscroll-get-scroll-column area) delta)
     area)))

(defun phscroll-scroll-left (&optional arg area)
  "Scroll AREA left by ARG columns.

If ARG is nil, use the value returned by `phscroll-scroll-left-right-unit'.

If AREA is nil, use the scroll area containing the current point."
  (interactive "P")
  (phscroll-scroll-left-right-internal
   (if arg (prefix-numeric-value arg) (phscroll-scroll-left-right-unit))
   area))

(defun phscroll-scroll-right (&optional arg area)
  "Scroll AREA right by ARG columns.

If ARG is nil, use the value returned by `phscroll-scroll-left-right-unit'.

If AREA is nil, use the scroll area containing the current point."
  (interactive "P")
  (phscroll-scroll-left-right-internal
   (- (if arg (prefix-numeric-value arg) (phscroll-scroll-left-right-unit)))
   area))

(defun phscroll-scroll-left-right-unit ()
  "Return the default horizontal scroll amount.

The amount is determined by taking into account the window width and the
margin to preserve."
  (max 1
       (- (phscroll-window-width-at (point) nil)
          (phscroll-margin-right)
          4)))

(defun phscroll-scroll-left-right-internal (delta area)
  "Scroll AREA by DELTA columns.

This is the implementation of `phscroll-scroll-left' and
`phscroll-scroll-right'.

Respects the customization variables
`phscroll-scroll-left-right-reverse-direction' and
`phscroll-scroll-left-right-move-point'."
  (when phscroll-scroll-left-right-reverse-direction
    (setq delta (- delta)))
  (when (null area)
    (setq area (phscroll-get-current-area)))

  (when area
    (when (and phscroll-scroll-left-right-move-point
               (<= (phscroll-area-begin area) (point))
               (< (point) (phscroll-area-end area)))
      (cond
       ((> delta 0)
        (goto-char
         (+ (phscroll-line-begin)
            (phscroll-string-length
             (car (phscroll-substring-over-width
                   (phscroll-current-line-string)
                   (+ (phscroll-column (point))
                      delta)))))))
       ((< delta 0)
        (goto-char
         (+ (phscroll-line-begin)
            (phscroll-string-length
             (car (phscroll-substring-over-width
                   (phscroll-current-line-string)
                   (max 0 (+ (phscroll-column (point)) delta)))))))))))

  (phscroll-add-scroll-column delta area))

(defun phscroll-column (pos)
  "Return the horizontal position of POS."
  ;; TODO: `current-column'との違いは？ `phscroll-ignored-invisibility-specs'の扱いくらい？
  (phscroll-string-width
   (phscroll-buffer-substring
    (phscroll-line-begin pos) pos)))

(defun phscroll-show-point (pos)
  "If POS is within a scroll area, horizontally scroll the area so that POS
is visible within the window."
  (let ((area (phscroll-get-area-at pos)))
    (when area
      (let ((scroll-column (phscroll-get-scroll-column area))
            (pos-column (phscroll-column pos))
            (window-width (phscroll-window-width-at pos nil)))
        (cond
         ((< pos-column scroll-column)
          (phscroll-set-scroll-column pos-column area))
         ((> pos-column (+ scroll-column window-width))
          (phscroll-set-scroll-column (- pos-column window-width) area)))))))

(defun phscroll-scroll-point (pos)
  "If POS is within a scroll area, horizontally scroll the area so that POS
is visible within the window."
  (let ((area (phscroll-get-area-at pos)))
    (when area
      (let ((scroll-column (phscroll-get-scroll-column area))
            (pos-column (phscroll-column pos))
            (window-width (phscroll-window-width-at pos nil))
            (step (if (= hscroll-step 0)
                      (/ (1+ (phscroll-window-width-at pos nil)) 2)
                    hscroll-step)))
        (cond
         ((< pos-column (+ scroll-column hscroll-margin))
          (phscroll-set-scroll-column
           (max 0 (- pos-column hscroll-margin step)) area))
         ((> pos-column (+ scroll-column (- window-width hscroll-margin)))
          (phscroll-set-scroll-column
           (+ (- pos-column window-width) hscroll-margin step) area)))))))

(defun phscroll-recenter (&optional arg)
  "If there is a scroll area at the current point, horizontally scroll it
so that the point is at the position within the window specified by ARG.

If ARG is 0 or a positive integer, scroll so that the point is ARG
columns from the left edge of the window.

If ARG is a negative integer, scroll so that the point is ARG columns
from the right edge of the window.

If ARG is nil, scroll so that the point is centered in the window."
  (interactive "P")
  (let* ((pos (point))
         (pos-column (phscroll-column pos))
         (area (phscroll-get-area-at pos)))
    (when area
      (phscroll-set-scroll-column
       (- pos-column
          (if arg
              (let ((n (prefix-numeric-value arg)))
                (if (>= n 0) n (+ (phscroll-window-width-at pos nil) n)))
            (/ (phscroll-window-width-at pos nil) 2)))
       area))))

(defvar phscroll-recenter-last-op nil)
(defcustom phscroll-recenter-positions '(center left right)
  "Cycling order for `phscroll-recenter-left-right'.
Like a recenter-top-bottom."
  :type '(repeat (choice
                  (const :tag "Left" left)
                  (const :tag "Center" center)
                  (const :tag "Right" right)
                  (integer :tag "Column number")
                  (float :tag "Percentage")))
  :group 'phscroll)

(defun phscroll-recenter-left-right (&optional arg)
  "Scroll horizontally to place the point at the left, right, or center of
the window, in the same way as `recenter-top-bottom' works vertically."
  ;; The foloweing code was copied and modified from
  ;; `recenter-top-bottom' in window.el
  (interactive "P")
  (cond
   (arg (phscroll-recenter arg))
   (t
    (setq phscroll-recenter-last-op
          (car (or
                (if (eq this-command last-command)
                    (cdr (member phscroll-recenter-last-op
                                 phscroll-recenter-positions)))
                phscroll-recenter-positions)))
    (let* ((win-width (phscroll-window-width-at (point) nil))
           (this-scroll-margin
            (min (max 0 hscroll-margin)
                 (truncate (/ win-width 4.0)))))
      (cond ((eq phscroll-recenter-last-op 'center)
             (phscroll-recenter))
            ((eq phscroll-recenter-last-op 'left)
             (phscroll-recenter this-scroll-margin))
            ((eq phscroll-recenter-last-op 'right)
             (phscroll-recenter (- -1 this-scroll-margin)))
            ((integerp phscroll-recenter-last-op)
             (phscroll-recenter phscroll-recenter-last-op))
            ((floatp phscroll-recenter-last-op)
             (phscroll-recenter (round (* phscroll-recenter-last-op win-width)))))))))

(defun phscroll-recenter-top-bottom (&optional _arg)
  "Run `recenter-top-bottom' and `phscroll-recenter' together.

The prefix argument ARG is not used."
  (interactive "P")
  (call-interactively #'recenter-top-bottom)
  (phscroll-recenter))


;;;;; Area Overlay

(defun phscroll-area-overlay (&optional area)
  "Return the overlay object representing the entire scroll area associated
with AREA.

If AREA is nil, use the scroll area containing the current point."
  (nth 2 (or area (phscroll-get-current-area))))

;;;;; Area Range

(defun phscroll-area-begin (&optional area)
  "Return the beginning position of AREA.

If AREA is nil, use the scroll area containing the current point."
  (overlay-start (phscroll-area-overlay (or area (phscroll-get-current-area)))))

(defun phscroll-area-end (&optional area)
  "Return the end position of AREA.

If AREA is nil, use the scroll area containing the current point."
  (overlay-end (phscroll-area-overlay (or area (phscroll-get-current-area)))))

;;;;; Area Finding

(defun phscroll-area-from-overlay (ov)
  "Return the scroll area object associated with the overlay object OV, or
nil if none."
  (overlay-get ov 'phscroll-area))

(defun phscroll-get-area-at (pos)
  "Return the scroll area object at POS, or nil if none."
  (let (area
        (overlays (overlays-at pos)))
    (while (and overlays
                (null (setq area (phscroll-area-from-overlay (car overlays)))))
      (setq overlays (cdr overlays)))
    area))

(defun phscroll-get-current-area ()
  "Return the scroll area object at the current point, or nil if none."
  (phscroll-get-area-at (point)))

(defun phscroll-enum-area (&optional beg end)
  "Return a list of scroll area objects in the region from BEG to END."
  (interactive "r")

  (let* ((overlays (overlays-in (or beg (point-min)) (or end (point-max)))))
    (cl-loop for ov in overlays
             if (overlay-get ov 'phscroll-area)
             collect (overlay-get ov 'phscroll-area))))

(defun phscroll-update-all-area ()
  "Update the display of all scroll area objects in the current buffer."
  (save-restriction
    (widen)
    (cl-loop for area in (phscroll-enum-area)
             do (phscroll-update-area-display area t))))

(defun phscroll-invalidate-all-area ()
  "Invalidate the display of all scroll area objects in the current buffer.

Invalidation means marking them as needing a display update."
  (save-restriction
    (widen)
    (cl-loop for area in (phscroll-enum-area)
             do (phscroll-area-clear-updated-ranges area))))

(defun phscroll-invalidate-region (beg end)
  "Invalidate the display of scroll area objects in the region from BEG to END.

Invalidation means marking them as needing a display update."
  (dolist (area (phscroll-enum-area beg end))
    (phscroll-area-remove-updated-range beg end area)))

(defun phscroll-areas-in-window (&optional window)
  "Return the scroll area objects within the visible range of WINDOW."
  (phscroll-enum-area
   (min (phscroll-window-start window))
   (max (phscroll-window-end window))))

(defun phscroll-update-areas-in-window (&optional redraw window)
  "Update the display of scroll area objects within the visible range of WINDOW.

If REDRAW is non-nil, also re-update already updated portions within the range."
  ;; (phscroll-log "Update Areas In Window: %s %s" redraw window)
  (cl-loop for area in (phscroll-areas-in-window window)
           do (phscroll-update-area-display area redraw window)))

;;;;; Updated Range List Management

;; The updated range list has the following format:
;;   ((-1 . -1) (beg0 . end0) (beg1 . end1) ... (begN . endN))
;;
;; Note: The leading (-1 . -1) is a dummy cons cell to simplify
;;       the implementation; the actual list starts from its cdr.

;; Constraints on the updated range list:
;; - Positions (begN, endN) are stored as relative offsets from
;;   the beginning of the area
;; - Ranges are always sorted in ascending order of position
;; - Ranges never overlap; adjacent ranges are always merged into one

(defun phscroll-area-updated-ranges-head (area)
  "Return the cons cell whose cdr is the updated range list of AREA."
  (nth 3 area))

(defun phscroll-area-updated-ranges-get (area)
  "Return the updated range list of AREA."
  (cdr (phscroll-area-updated-ranges-head area)))

(defun phscroll-area-updated-ranges-set (area ranges)
  "Set the updated range list of AREA to RANGES."
  (when area
    (setcdr (nth 3 area) ranges))
  area)

(defun phscroll-area-clear-updated-ranges (area)
  "Clear the updated range list of AREA."
  (phscroll-area-updated-ranges-set area nil))

(defun phscroll-area-add-updated-range (beg end area)
  "Add the updated range BEG to END to AREA."
  ;; 1. check and normalize range (beg, end), if empty then exit
  (when (and (< beg end) area)
    (let ((area-begin (phscroll-area-begin area))
          (area-end (phscroll-area-end area)))
      (when (< beg area-begin) (setq beg area-begin))
      (when (> end area-end) (setq end area-end))
      (when (< beg end)

        ;; convert to relative position
        (setq beg (- beg area-begin))
        (setq end (- end area-begin))

        ;;(message "add-updated-range %s %s to area %s" beg end area)

        ;; 2. find first range r1.end [>=] beg
        (let ((prev-r1 (phscroll-area-updated-ranges-head area)))
          (while (and (cdr prev-r1) (< (cdadr prev-r1) beg))
            (setq prev-r1 (cdr prev-r1)))

          ;; 3. if not found, append to last
          (if (null (cdr prev-r1))
              (setcdr prev-r1 (cons (cons beg end) nil))
            ;; 4. if r1.beg [>] end, insert before r1
            (if (< end (caadr prev-r1))
                (setcdr prev-r1 (cons (cons beg end) (cdr prev-r1)))
              ;; else,
              (let ((prev-r2 prev-r1))
                ;; 5. find last range r2.begin [<=] end
                (while (and (cddr prev-r2) (<= (caaddr prev-r2) end))
                  (setq prev-r2 (cdr prev-r2)))

                ;; 6. replace r1~r2 to (union r1~r2, beg~end)
                (setcdr prev-r1
                        (cons
                         (cons
                          (min (caadr prev-r1) beg)
                          (max (cdadr prev-r2) end))
                         (cddr prev-r2))))))))))
  area)

(defun phscroll-area-remove-updated-range (beg end area)
  "Remove the range BEG to END from the updated range list of AREA, marking
it as not yet updated."
  ;; 1. check and normalize range (beg, end), if empty then exit
  (when (and (< beg end) area)
    (let ((area-begin (phscroll-area-begin area))
          (area-end (phscroll-area-end area)))
      (when (< beg area-begin) (setq beg area-begin))
      (when (> end area-end) (setq end area-end))
      (when (< beg end)

        ;; convert to relative position
        (setq beg (- beg area-begin))
        (setq end (- end area-begin))

        ;; 2. find first range r1.end [>] beg
        (let ((prev-r1 (phscroll-area-updated-ranges-head area)))
          (while (and (cdr prev-r1) (<= (cdadr prev-r1) beg))
            (setq prev-r1 (cdr prev-r1)))

          ;; 3. if not found, do nothing
          (if (null (cdr prev-r1))
              nil
            ;; 4. if r1.beg [>=] end, do nothing
            (if (<= end (caadr prev-r1))
                nil
              ;; else,
              (let ((prev-r2 prev-r1))
                ;; 5. find last range r2.begin [<] end
                (while (and (cddr prev-r2) (< (caaddr prev-r2) end))
                  (setq prev-r2 (cdr prev-r2)))

                ;; 6. subtract beg~end from each r1~r2
                (let ((prev-r prev-r1)
                      (end-prev-r (cdr prev-r2)))
                  (while (not (eq prev-r end-prev-r))
                    (cond
                     ;; if beg <= r.beg and r.end <= end, remove r from list
                     ((and (<= beg (caadr prev-r)) (<= (cdadr prev-r) end))
                      (if (eq (cdr prev-r) end-prev-r) ;;remove last range
                          (setq end-prev-r prev-r)) ;;set prev-r to end
                      (setcdr prev-r (cddr prev-r))
                      ;; keep prev-r
                      )
                     ;; set r to (end . r.end)
                     ((and (<= beg (caadr prev-r)) (< end (cdadr prev-r)))
                      (setcar (cadr prev-r) end)
                      (setq prev-r (cdr prev-r)))
                     ;; set r to (r.beg . beg)
                     ((and (< (caadr prev-r) beg) (<= (cdadr prev-r) end))
                      (setcdr (cadr prev-r) beg)
                      (setq prev-r (cdr prev-r)))
                     ;; divide r to (r.beg . beg) [reuse](end . r.end)
                     ((and (< (caadr prev-r) beg) (< end (cdadr prev-r)))
                      (setcdr prev-r
                              (cons (cons (caadr prev-r) beg)
                                    (cdr prev-r)))
                      (setcar (caddr prev-r)
                              end)
                      (setq prev-r (cddr prev-r)))))))))))))
  area)

;; Example:
;; (setq test-area (phscroll-area-create 10 22))
;; (phscroll-area-add-updated-range 12 14 test-area)
;; (phscroll-area-add-updated-range 16 18 test-area)
;; (phscroll-area-add-updated-range 20 22 test-area)
;; (phscroll-area-add-updated-range 13 16 test-area)
;; (phscroll-area-remove-updated-range 13 21 test-area)
;; (phscroll-area-remove-updated-range 10 22 test-area)

(defun phscroll-area-needs-update-range (beg end area)
  "Return non-nil if there are any not-yet-updated portions of AREA within
the region from BEG to END."
  (when (and (< beg end) area)
    (let ((area-begin (phscroll-area-begin area))
          (area-end (phscroll-area-end area)))
      (when (< beg area-begin) (setq beg area-begin))
      (when (> end area-end) (setq end area-end))
      (when (< beg end)

        ;; convert to relative position
        (setq beg (- beg area-begin))
        (setq end (- end area-begin))

        (not
         (cl-find-if
          (lambda (range) (and (<= (car range) beg) (<= end (cdr range))))
          (phscroll-area-updated-ranges-head area)))))))

;; Example:
;;(phscroll-area-needs-update-range 21 23 test-area)

(defun phscroll-area-shift-updated-ranges-after (pos delta area)
  "Shift all positions at or after POS in the updated range list of AREA by
DELTA."
  (when (and area (/= delta 0))
    (let ((area-begin (phscroll-area-begin area))
          (r (cdr (phscroll-area-updated-ranges-head area))))

      ;; convert to relative position
      (setq pos (- pos area-begin))

      (while (and r (< (cdar r) pos))
        (setq r (cdr r)))
      (when r
        (when (<= pos (caar r)) (setcar (car r) (+ (caar r) delta)))
        (when (<= pos (cdar r)) (setcdr (car r) (+ (cdar r) delta)))
        (setq r (cdr r))
        (while r
          (setcar (car r) (+ (caar r) delta))
          (setcdr (car r) (+ (cdar r) delta))
          (setq r (cdr r))))))
  area)

;; Example:
;;(phscroll-area-shift-updated-ranges-after 51 10 test-area)

;;;;; Area Window Width

;; The window width at layout time is recorded for each area.
;; This allows each area to be displayed with a different width
;; when multiple windows have different widths.
;; If the window width has changed since the last update, all ranges
;; are invalidated and redrawn.
;; If the same area is displayed in multiple windows with different widths,
;; the layout may be corrupted, but this is accepted as a limitation.

(defun phscroll-area-get-window-width (area)
  "Return the window width at the time AREA was last updated.

Always returns an integer, but may return an invalid value such as -1 if
the area has not yet been updated."
  (nth 4 area))

(defun phscroll-area-set-window-width (area width)
  "Set the window width at the time AREA was last updated to WIDTH."
  (setcar (nthcdr 4 area) width))

(defun phscroll-area-set-window-width-and-changed-p (area window)
  "Update the recorded window width of AREA to the current width of WINDOW.

Return t if the width has changed, nil otherwise."
  (let ((new-width (phscroll-window-width window))) ;; TODO: find minimum width of (phscroll-window-width-at line-pos window) ?
    (when (not (= new-width (phscroll-area-get-window-width area)))
      (phscroll-area-set-window-width area new-width)
      t)))



;;;; Event Handlers

;;;;; Hooks

;; (defun phscroll-on-window-size-changed (&optional _frame)
;;   ;;(message "window-size-changed width beg=%s end=%s width=%s" (window-start) (window-end) (window-width))
;;   ;;(phscroll-update-all-area) ;;Too slow
;;   ;;(phscroll-update-areas-in-window) ;;Do not use. window-start and window-end are not updated
;; ;;  (phscroll-invalidate-all-area) ;;Use phscroll-area-set-window-width-and-changed-p
;;   )

(defun phscroll-on-post-command ()
  "Update scroll areas after each command execution.

This function is registered in `post-command-hook' by `phscroll-mode'."
  (phscroll-log "Overview: On post-command point=%s" (point))
  ;;(message "on post command window-end=%s" (window-end))

  ;; Make the current point visible.
  ;;(phscroll-show-point (point))
  (unless (cl-find this-command phscroll-interactive-scroll-commands)
    (phscroll-scroll-point (point)))

  ;; Update the display of the scroll area that is in the visible range.
  ;;(phscroll-update-area-display (phscroll-get-area-at (point)))
  (phscroll-log "Overview: Update by post-command point=%s" (point))
  (phscroll-update-areas-in-window nil nil))

(defun phscroll-on-window-scroll (window _new-display-start-pos)
  "Update scroll areas when a window is scrolled.

This function is registered in `window-scroll-functions' by `phscroll-mode'."
  ;; Note: The value of `window-end' is not valid !!
  (phscroll-log
   "Overview: Update by window-scroll (%s~%s) %s new-start=%s"
   (window-start window) (window-end window) window
   _new-display-start-pos)
  (phscroll-update-areas-in-window nil window))

(defun phscroll-on-pre-redisplay (&optional window)
  "Update scroll areas just before a window is redisplayed.

This function is registered in `pre-redisplay-functions' by `phscroll-mode'."
  (phscroll-check-truncate-lines)
  (phscroll-log "Overview: Update by pre-redisplay (%s~%s) %s"
                (window-start window) (window-end window) window)
  (phscroll-update-areas-in-window nil window))

(defvar-local phscroll-update-area-display-on-modified t
  "When nil, display updates are not performed on text modification.
See: `phscroll-on-modified'.")

(defun phscroll-on-modified (ov after beg end &optional before-length)
  "Called when text is modified within a scroll area.

Updates the updated range list according to the modification and
refreshes the scroll display.

This function is set in the modification-hooks property of the overlay
covering the area."
  ;;(message "modified %s %s %s %s" after beg end before-length)
  (when after
    (let* ((after-length (- end beg))
           (delta-length (- after-length before-length))
           (area (phscroll-area-from-overlay ov)))
      (when area
        (if (= (phscroll-area-begin area) (phscroll-area-end area))
            ;; Destroy the empty scroll area
            (phscroll-area-destroy area)
          ;; Update modified range
          (cond
           ((> delta-length 0)
            (phscroll-area-shift-updated-ranges-after beg delta-length area)
            (phscroll-area-remove-updated-range (phscroll-line-begin beg)
                                                (phscroll-line-end end)
                                                area))

           ((< delta-length 0)
            (phscroll-area-remove-updated-range (phscroll-line-begin beg)
                                                (phscroll-line-end
                                                 (+ beg before-length))
                                                area)
            (phscroll-area-shift-updated-ranges-after end delta-length area))

           (t
            (phscroll-area-remove-updated-range (phscroll-line-begin beg)
                                                (phscroll-line-end end)
                                                area)))
          ;; TODO: do pre-redisplay only?
          (when phscroll-update-area-display-on-modified
            (phscroll-log "Overview: Update by phscroll-on-modified")
            (phscroll-update-area-display area)))))))


(defun phscroll-check-truncate-lines ()
  "Handle changes to the value of the `truncate-lines' variable."
  (when (not (equal truncate-lines phscroll-truncate-lines))
    (setq phscroll-truncate-lines truncate-lines)
    (when phscroll-truncate-lines
      ;; Remove left and right overlays
      (cl-loop for area in (phscroll-enum-area)
               do (let ((beg (phscroll-area-begin area))
                        (end (phscroll-area-end area)))
                    (phscroll-remove-line-veils-region beg end))))
    ;; Invalidate and redraw all areas
    (phscroll-log "Overview: Update by phscroll-check-truncate-lines")
    (phscroll-update-all-area)))

;;;;; Mouse Wheel

;; Provides Shift+mouse wheel horizontal scrolling, similar to
;; `mouse-wheel-mode'."

(defcustom phscroll-mwheel-scroll-amount-horizontal nil
  "Amount to scroll phscroll areas horizontally."
  :type '(choice (const :tag "Use `mouse-wheel-scroll-amount-horizontal'" nil)
                 (integer 4))
  :group 'phscroll)

(defun phscroll-mwheel-scroll-left (event)
  "Scroll left using the mouse wheel."
  (interactive "e")
  (phscroll-mwheel-scroll-left-right-internal event 1))

(defun phscroll-mwheel-scroll-right (event)
  "Scroll right using the mouse wheel."
  (interactive "e")
  (phscroll-mwheel-scroll-left-right-internal event -1))

(defun phscroll-mwheel-scroll-left-right-internal (event dir)
  "Perform horizontal scrolling using the mouse wheel.

This is the implementation of `phscroll-mwheel-scroll-left' and
`phscroll-mwheel-scroll-right'."
  (interactive "e")
  (when-let* ((point (posn-point (event-start event)))
              (window (posn-window (event-start event))))
    (with-current-buffer (window-buffer window)
      (when-let* ((area (phscroll-get-area-at point)))
        (phscroll-scroll-right
         (* dir
            (or phscroll-mwheel-scroll-amount-horizontal
                (and (boundp 'mouse-wheel-scroll-amount-horizontal)
                     mouse-wheel-scroll-amount-horizontal)
                4))
         area)))))

;;;;; Area Local Keymap

(defvar phscroll-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x<" #'phscroll-scroll-left)
    (define-key map "\C-x>" #'phscroll-scroll-right)
    (define-key map (kbd "C-S-l") #'phscroll-recenter-left-right)
    (define-key map (kbd "C-l") #'phscroll-recenter-top-bottom)
    ;; Shift + Mouse Wheel
    (define-key map [(shift wheel-up)] #'phscroll-mwheel-scroll-left)
    (define-key map [(shift wheel-down)] #'phscroll-mwheel-scroll-right)
    (when (boundp 'mouse-wheel-down-event)
      (define-key map (vector (list 'shift mouse-wheel-down-event))
                  #'phscroll-mwheel-scroll-left))
    (when (boundp 'mouse-wheel-up-event)
      (define-key map (vector (list 'shift mouse-wheel-up-event))
                  'phscroll-mwheel-scroll-right))
    map))

(defun phscroll-area-set-keymap (area)
  (when area
    (overlay-put (phscroll-area-overlay area) 'keymap phscroll-keymap)))



;;;; Window Utilities


(defvar phscroll-use-window-end-update nil) ;; TODO:

(defun phscroll-window-start (window)
  ;; TODO: pre-redisplay-functions 内では正しい値を返さない？
  (window-start window))

(defun phscroll-window-end (window)
  ;; TODO: pre-redisplay-functions 内では正しい値を返さない。徐々に増えていく場合がある。
  (max
   (or (window-end window phscroll-use-window-end-update) 0)
   ;; window-startからwindow行数だけ進んだ場所。
   ;; 不可視の行がある場合は正しくないが、再描画が終わるまで待つよりは良い場合がある。不可視の行を判定すればたどり着けるかもしれないが、テキストプロパティやオーバーレイを取得しながらだとおそらくかなり遅い。
   (save-excursion
     (goto-char (window-start window))
     (forward-line (window-body-height window))
     (point))))

(defun phscroll-window-width-at (pos window)
  (-
   (phscroll-window-width window)
   ;; Count wrap-prefix width.
   ;; org-indent uses this.
   (length (get-text-property pos 'wrap-prefix))
   ;; Count before-string width at the beginning of the line.
   ;; org-table-overlay-coordinates uses this.
   (let ((bol (phscroll-line-begin pos)))
     (cl-loop for ov in (overlays-at bol)
              sum (length (and (equal (overlay-start ov) bol)
                               (overlay-get ov 'before-string)))))))

(defun phscroll-window-width (&optional window)
  (max
   0
   (-
    (window-body-width window)
    (phscroll-margin-right)
    ;; line numbers
    (if (fboundp 'line-number-display-width) ;;Emacs 26.1 or later
        (ceiling
         (save-excursion ;;なぜかpointが変わることがある。redisplay中だから？
           (if window
               (with-selected-window window
                 (line-number-display-width 'columns))
             (line-number-display-width 'columns))))
      0))))



;;;; Area Display


(defvar-local phscroll-fontify-range nil
  "Fontifying range (BEG . END).")

(defun phscroll-update-area-display (area &optional redraw window)
  "Update the display of scroll AREA.

If REDRAW is non-nil, clear the updated range list of AREA before updating.

WINDOW is the window to display in; nil means the selected window."
  (when (and area (not phscroll-truncate-lines))
    (if (or (phscroll-area-set-window-width-and-changed-p area window) ;;First, update area.window-width
            redraw)
        (phscroll-area-clear-updated-ranges area))

    (let* ((scroll-column (phscroll-get-scroll-column area))
           (area-begin (phscroll-area-begin area))
           (area-end (phscroll-area-end area))
           ;; If in fontify, do not use window-begin and window-end.
           ;; area-begin, area-end are already windowed.
           (update-begin (max area-begin
                              (point-min) ;;narrowed
                              (if phscroll-fontify-range
                                  (phscroll-line-begin
                                   (car phscroll-fontify-range))
                                (phscroll-line-begin
                                 (phscroll-window-start window)))))
           (update-end (min area-end
                            (point-max) ;;narrowed
                            (if phscroll-fontify-range
                                (cdr phscroll-fontify-range)
                              (phscroll-window-end window)))))

      ;; Skip folded region in outline-mode or org-mode
      ;; (windowの表示範囲内を更新するとき、折りたたまれて見えない所ま
      ;; で全て更新しようとすると膨大な行を更新しようとして長時間フリー
      ;; ズすることがあるのでそれを防止する)
      ;; TODO: areaの途中だけが折りたたまれているような状況に対応する。
      ;; 実際にはareaがorg-modeのテーブルであればそのような状況はそう
      ;; そう無い。あったとしても、折りたたまれている行数が少なければ
      ;; パフォーマンスへの影響は少ない。
      (setq update-begin (phscroll-skip-folded-region update-begin update-end))

      (when (and (< update-begin update-end)
                 (phscroll-area-needs-update-range update-begin update-end area))
        (phscroll-log "Overview: Update Area: %s~%s update-range=%s~%s window-width=%s redraw=%s"
                      area-begin area-end
                      update-begin update-end
                      (window-width window) redraw)

        (let ((actual-end
               (phscroll-update-area-lines-display area scroll-column
                                                   update-begin update-end
                                                   window)))
          ;; Mark [update-begin, actual-end) as updated
          (phscroll-area-add-updated-range update-begin actual-end area))))))

(defcustom phscroll-update-area-max-time 10.0
  "Maximum time (in seconds) for updating the display of phscroll areas.
If a single display update takes this long or longer, the update is aborted."
  :group 'phscroll
  :type '(float :tag "Seconds"))

(defcustom phscroll-update-area-max-lines 'window-height
  "Maximum number of lines for updating the display of phscroll areas.
If the number of lines updated in a single display update reaches this
value or more, the update is aborted."
  :group 'phscroll
  :type '(choice
          (integer :tag "Lines")
          (const :tag "Window height * 2" window-height)
          (cons :tag "Window Height * n" (const window-height) number)))

(defun phscroll-update-area-max-lines (window)
  "Return the maximum number of lines for updating the display of phscroll
areas.
See: `phscroll-update-area-max-lines' variable."
  (pcase phscroll-update-area-max-lines
    ((and (pred integerp) n) n)
    ('window-height
     (* (window-height window 'ceiling) 2))
    (`(window-height . ,(and (pred integerp) n))
     (* (window-height window 'ceiling) n))
    (_ 1000)))

(defun phscroll-update-area-lines-display (area
                                           scroll-column
                                           update-begin update-end window)
  "Update the display of the region from UPDATE-BEGIN to UPDATE-END within
scroll AREA."
  (save-excursion
    ;; Ensure that the update range is within the point movable range.
    (when (< update-begin (point-min))
      (setq update-begin (point-min)))
    (when (> update-end (point-max))
      (setq update-end (point-max)))

    (goto-char (phscroll-line-begin update-begin))

    (let ((start-time (float-time))
          (line-count 0)
          (updated-line-count 0)
          (updated-line-limit (phscroll-update-area-max-lines window)))

      (while (and (< (point) update-end)
                  (< (- (float-time) start-time) phscroll-update-area-max-time)
                  (< updated-line-count updated-line-limit))
        (let ((line-begin (phscroll-line-begin))
              (line-end (phscroll-line-end)))
          (when (phscroll-area-needs-update-range line-begin line-end area)
            ;;(message "update line %d" (point))
            (phscroll-remove-line-veils line-begin line-end)
            (save-excursion
              (phscroll-update-current-line-display line-begin line-end
                                                    scroll-column window))
            (cl-incf updated-line-count))
          ;; goto next line
          (cl-incf line-count)
          (forward-line)))

      (phscroll-log "Updated %d/%d lines in %f[ms]"
                    updated-line-count line-count
                    (* 1000 (- (float-time) start-time)))

      ;; Return the end of the actual updated range
      (point))))

(defun phscroll-update-current-line-display (line-begin line-end
                                                        scroll-column window)
  "Update the display of a single line within a scroll area.

The current point is assumed to be at the beginning of the line.

SCROLL-COLUMN is the number of columns to be hidden on the left side of
the window.

WINDOW is the window to display in; nil means the selected window."
  ;; | line                            |
  ;; | left(9) | middle(14)   | right  |  <part(limit)
  ;; |ABCDEFGHI|JKLMNOPQRSTUVW|XYZ01234|
  ;; |あいうえおかきくけこ
  (let* ((window-width (phscroll-window-width-at (point) window))
         ;; current line
         (line-str (phscroll-buffer-substring line-begin line-end))
         (line-overlays (phscroll-get-overlay-cache line-begin line-end))
         ;; left invisible
         (left-limit-width scroll-column)
         (left-str-width (phscroll-substring-over-width
                          line-str left-limit-width line-overlays)) ;; TODO: phscroll-calculate-in-pixels case
         (left-str (car left-str-width))
         (left-width (cdr left-str-width)) ;; 0 ~ left-limit-width + overflow
         (left-overflow (max 0 (- left-width scroll-column))) ;; 0 ~ overflow
         (left-len (phscroll-string-length left-str))
         ;; middle visible
         (middle-limit-width (- window-width left-overflow))
         (middle-str-width
          (if phscroll-calculate-in-pixels
              (phscroll-truncate-string-to-width-px
               (phscroll-substring line-str left-len) middle-limit-width)
            (phscroll-truncate-string-to-width
             (phscroll-substring line-str left-len) middle-limit-width
             line-overlays)))
         (middle-str (car middle-str-width))
         (middle-width (cdr middle-str-width))
         (middle-shortage (- middle-limit-width middle-width))
         (middle-len (phscroll-string-length middle-str)))

    (phscroll-log "Update Line: %s~%s len:%s:%s width:%s:%s"
                  line-begin line-end
                  left-len middle-len
                  left-width middle-width)

    (phscroll-add-line-veils
     line-begin line-end
     left-len
     left-overflow
     middle-len
     middle-shortage)))

(defun phscroll-remove-line-veils (line-begin line-end)
  (phscroll-remove-line-veils-region line-begin
                                     ;;include line-break
                                     (1+ line-end)))

(defalias 'phscroll-add-line-veils
  ;;#'phscroll-add-line-veils--textprop) ;; Experimental
  #'phscroll-add-line-veils--overlay)

(defalias 'phscroll-remove-line-veils-region
  ;;#'phscroll-remove-line-veils-region--textprop) ;; Experimental
  #'phscroll-remove-line-veils-region--overlay)

(defun phscroll-add-line-veils--overlay (line-begin
                                         line-end
                                         left-len
                                         left-overflow
                                         middle-len
                                         middle-shortage)
  (when (> left-len 0)
    (let ((ov (make-overlay line-begin (+ line-begin left-len))))
      (overlay-put ov 'display (if phscroll-use-fringe
                                   '(left-fringe left-arrow)
                                 "<"))
      (overlay-put ov 'after-string (make-string left-overflow ?\s))
      (overlay-put ov 'phscroll t)
      ;;(overlay-put ov 'phscroll-left t)
      (overlay-put ov 'phscroll-line-veil t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'priority 10)))

  (when (< (+ line-begin left-len middle-len) line-end)
    (let ((ov (make-overlay (+ line-begin left-len middle-len) line-end)))
      (overlay-put ov 'display
                   (if phscroll-use-fringe
                       '(right-fringe right-arrow)
                     (concat (make-string middle-shortage ?\s) ">")))
      (overlay-put ov 'phscroll t)
      ;;(overlay-put ov 'phscroll-right t)
      (overlay-put ov 'phscroll-line-veil t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'priority 10))))

(defun phscroll-remove-line-veils-region--overlay (begin end)
  (remove-overlays begin end 'phscroll-line-veil t))


(defun phscroll-add-line-veils--textprop (line-begin
                                          line-end
                                          left-len
                                          left-overflow
                                          middle-len
                                          middle-shortage) ;; Experimental
  ;; TODO: Save old display properties
  ;; TODO: Apply `phscroll-use-fringe'
  (with-silent-modifications
    (when (> left-len 0)
      (let ((beg line-begin)
            (end (+ line-begin left-len)))
        (put-text-property beg end
                           'display
                           (concat
                            "<" (make-string left-overflow ?\s)))
        (put-text-property beg end 'phscroll-line-veil t)))

    (when (< (+ line-begin left-len middle-len) line-end)
      (let ((beg (+ line-begin left-len middle-len))
            (end line-end))
        (put-text-property beg end
                           'display
                           (concat (make-string middle-shortage ?\s) ">"))
        (put-text-property beg end 'phscroll-line-veil t)))))

(defun phscroll-remove-line-veils-region--textprop (begin end) ;; Experimental
  (with-silent-modifications
    (remove-text-properties begin end
                            '(display nil phscroll-line-veil nil))))

;;;;; Folded Lines

(defconst phscroll-folding-invisibility-specs
  ;; See:
  ;; - `org-fold-initialize'
  ;; - `org-string-width-invisibility-spec'
  ;; - `outline-flag-region'
  '(outline org-fold-drawer org-fold-block org-fold-outline))

(defun phscroll-folding-invisibility-spec-p (prop-value)
  (or (memq prop-value phscroll-folding-invisibility-specs)
      ;; When PROP-VALUE has multiple specs.
      (and (listp prop-value)
           (seq-some (lambda (x) (memq x phscroll-folding-invisibility-specs))
                     prop-value))))

(defun phscroll-folded-overlay-p (ov)
  (phscroll-folding-invisibility-spec-p (overlay-get ov 'invisible)))

(defun phscroll-skip-folded-region (pos limit)
  ;; テキストプロパティも考慮する場合:
  ;; (while (and (< pos limit)
  ;;             (phscroll-folding-invisibility-spec-p
  ;;              (get-char-property pos 'invisible)))
  ;;   (setq pos (next-single-char-property-change pos 'invisible nil limit)))
  ;; オーバーレイのみを考慮する場合:
  (while (and (< pos limit)
              (let ((folded-ov (seq-find #'phscroll-folded-overlay-p
                                         (overlays-at pos))))
                (when folded-ov
                  (setq pos (max (1+ pos) (overlay-end folded-ov)))
                  (phscroll-log "Skip folded region %s~%s"
                                (overlay-start folded-ov)
                                (overlay-end folded-ov))
                  t))))
  ;; テキストプロパティを考慮すると、折りたたまれた範囲に巨大な
  ;; phscrollエリアがあった時に、それがウィンドウの中にあると
  ;; post-command-hookで毎回大きな時間を消費してしまう。
  pos)

;;;; Text Utilities

;;;;; Beginning and End of lines

(defun phscroll-line-begin (&optional pos)
  "Return the position of the beginning of the line.

If POS is specified, return the position for the line containing POS;
otherwise return the position for the line containing the current point."
  (let ((inhibit-field-text-motion t))
    (if pos
        (save-excursion (goto-char pos) (line-beginning-position))
      (line-beginning-position))))

(defun phscroll-line-end (&optional pos)
  "Return the position of the end of the line.

If POS is specified, return the position for the line containing POS;
otherwise return the position for the line containing the current point."
  (let ((inhibit-field-text-motion t))
    (if pos
        (save-excursion (goto-char pos) (line-end-position))
      (line-end-position))))

;;;;; Text Operation Like a String

(defun phscroll-buffer-substring (beg end)
  ;;ignore-overlay: (buffer-substring-no-properties beg end)
  (cons beg end)
  )

(defun phscroll-current-line-string ()
  (phscroll-buffer-substring (phscroll-line-begin) (phscroll-line-end)))

(defun phscroll-string-length (str)
  ;;ignore-overlay: (length str)
  (- (cdr str) (car str))
  )

(defun phscroll-substring (str &optional from to)
  ;;ignore-overlay: (substring str from to)
  (cons
   (if (null from) (car str) (+ (car str) from))
   (if (null to) (cdr str) (+ (car str) to)))
  )


;;;;; Overlay Cache for Text Width Calculation

(defun phscroll-ovc-create (type pvalue ov)
  (list type
        pvalue
        ov
        (overlay-start ov)
        (overlay-end ov)
        (overlay-get ov 'priority)))
(defun phscroll-ovc-type (ovc) (car ovc))
(defun phscroll-ovc-pvalue (ovc) (nth 1 ovc))
(defun phscroll-ovc-ov (ovc) (nth 2 ovc))
(defun phscroll-ovc-beg (ovc) (nth 3 ovc))
(defun phscroll-ovc-end (ovc) (nth 4 ovc))
(defun phscroll-ovc-priority (ovc) (or (nth 5 ovc) 0))
(defun phscroll-ovc-less (ovc1 ovc2)
  (let ((dstart (- (phscroll-ovc-beg ovc1) (phscroll-ovc-beg ovc2))))
    (or (< dstart 0)
        (and (= dstart 0)
             (> (phscroll-ovc-priority ovc1)
                (phscroll-ovc-priority ovc2))))))

(defun phscroll-get-overlay-cache (beg end)
  "Return a list of objects holding information about overlays between BEG
and END that affect text width calculation.

Each object is created by `phscroll-ovc-create' and accessed via
functions prefixed with `phscroll-ovc-'.

The list is sorted by overlay start position, priority, and other
properties, allowing sequential processing from the beginning.

Overlays that do not affect text width calculation are excluded."
  (phscroll-log "phscroll-get-overlay-cache %s~%s" beg end)
  (let* ((overlays (overlays-in beg end))
         (iter overlays))
    ;; filter overlays
    (while iter
      (let* ((ov (car iter))
             pvalue)
        (cond
         ;; ignore phscroll's overlay
         ((overlay-get ov 'phscroll)
          (setcar iter nil))
         ((overlay-get ov 'phscroll-ignore)
          (setcar iter nil))
         ;; 'display
         ((setq pvalue (overlay-get ov 'display))
          (setcar iter (phscroll-ovc-create 'display pvalue ov))) ;;reuse a cons cell of overlays list
         ;; 'invisible
         ((and (setq pvalue (overlay-get ov 'invisible))
               (not (phscroll-ignored-invisibility-spec-p pvalue))) ;;ignore outline invisible overlay
          (setcar iter (phscroll-ovc-create 'invisible pvalue ov))) ;;reuse a cons cell of overlays list
         ;; not supported type
         (t
          (setcar iter nil))))
      (setq iter (cdr iter)))

    ;; delete nil
    (setq overlays (delq nil overlays))
    (if overlays
        ;; sort by overlay-start and priority
        (sort overlays #'phscroll-ovc-less)
      ;; If there is no valid overlay, return a dummy symbol to
      ;; represent it to suppress rescanning.
      'empty)))

(defun phscroll-get-overlay-at (pos cache)
  (when (consp cache)
    ;;discard ovc.end <= pos
    (while (and cache (<= (phscroll-ovc-end (car cache)) pos))
      (setq cache (cdr cache)))

    (when (and cache
               (<= (phscroll-ovc-beg (car cache)) pos)) ;; ovc.beg <= pos
      (car cache))))

;;;;; Invisibility Specs

(defconst phscroll-ignored-invisibility-specs
  phscroll-folding-invisibility-specs
  "List of invisibility specs to ignore when calculating text width.

Contents folded by outline-mode or org-mode are invisible, but their
width must be calculated as if they were visible.")

(defun phscroll-ignored-invisibility-spec-p (prop-value)
  "Return non-nil if the invisible property value specified by PROP-VALUE
should be ignored."
  (or (memq prop-value phscroll-ignored-invisibility-specs)
      ;; When PROP-VALUE has multiple specs.
      (and (listp prop-value)
           (seq-some (lambda (x) (memq x phscroll-ignored-invisibility-specs))
                     prop-value))))

;;;;; Character Width Calculation

(defun phscroll-char-width-next (pos cache)
  "Return the width of the character at POS and the position of the next
character.

CACHE is the list returned by `phscroll-get-overlay-cache'.

Return value format: (COLUMNS . NEXT-POSITION)

Overlays and text properties are taken into account as much as possible."
  (let (ovc display invisible)
    (cond
     ;; overlays
     ((setq ovc (phscroll-get-overlay-at pos cache))
      (pcase (phscroll-ovc-type ovc)
        ('display (cons
                   (phscroll-display-property-width (phscroll-ovc-pvalue ovc))
                   (phscroll-ovc-end ovc)))
        ('invisible (cons
                     (phscroll-invisible-property-width
                      (phscroll-ovc-pvalue ovc)
                      (phscroll-ovc-beg ovc)
                      (phscroll-ovc-end ovc))
                     (phscroll-ovc-end ovc)))
        (_ (cons 1 (1+ pos)))))

     ;; display text property
     ((setq display (get-text-property pos 'display))
      (cons
       (phscroll-display-property-width display)
       (1+ pos)))
     ;; invisible text property
     ((setq invisible (get-text-property pos 'invisible))
      (cons
       (phscroll-invisible-property-width invisible pos (1+ pos))
       (1+ pos)))
     ;; normal character
     (t
      (cons
       (char-width (char-after pos))
       (1+ pos))))))

(defun phscroll-display-property-width (display)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Property.html#Display-Property
  ;; TODO: support more formats
  (cond
   ;; string
   ((stringp display)
    (string-width display))
   ;; list
   ((listp display)
    (cond
     ((eq (car display) 'space)
      (let* ((props (cdr display)))
        (+
         (phscroll-resolve-space-pixel-spec-chars (plist-get props :width))
         (let ((factor (plist-get props :relative-width)))
           (if (numberp factor) (ceiling factor) 0))))) ;; TODO: (* factor (char-width <first char>))
     (t 0)))
   ;; unknown
   (t 0)))

(defun phscroll-resolve-space-pixel-spec-chars (spec)
  (cond
   ((null spec) 0)
   ((integerp spec) spec)
   ((floatp spec) (ceiling spec))
   ((symbolp spec)
    (phscroll-resolve-space-pixel-spec-chars (ignore-errors (symbol-value spec))))
   ((and (consp spec) (null (cdr spec)))
    (ceiling
     (/
      (phscroll-resolve-space-pixel-spec-chars (car spec))
      (default-font-width))))
   (t 0)))

(defun phscroll-invisible-property-width (invisible beg end)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Invisible-Text.html
  ;; TODO: Detect changes in buffer-invisibility-spec.
  (pcase (invisible-p invisible) ;; Consider buffer-invisibility-spec.
    ('nil (string-width (buffer-substring-no-properties beg end)))
    ('t 0)
    (_
     (if (and (>= beg (point-min)) (invisible-p (1- beg)))
         ;; Not first position in invisible region.
         0
       ;; Calculate the width of the current ellipsis string.
       (string-width
        (apply #'string (seq-map #'glyph-char
                                 (or (display-table-slot buffer-display-table 4)
                                     [?. ?. ?.]))))))))

;; Text Width Calculation

(defun phscroll-text-width (beg end &optional cache-arg)
  "Return the width in columns of the text between BEG and END.

CACHE-ARG is the list returned by `phscroll-get-overlay-cache'; if nil,
this function calls `phscroll-get-overlay-cache' internally."
  (let ((width 0)
        (pos beg)
        (cache (or cache-arg (phscroll-get-overlay-cache beg end))))
    (while (< pos end)
      (let ((width-next (phscroll-char-width-next pos cache)))
        (setq width (+ width (car width-next)))
        (setq pos (cdr width-next))))
    width))

(defun phscroll-string-width (str &optional cache-arg)
  "Return the width in columns of STR.

STR must be a value returned by `phscroll-buffer-substring' or
`phscroll-current-line-string'.

CACHE-ARG is the list returned by `phscroll-get-overlay-cache'; if nil,
this function calls `phscroll-get-overlay-cache' internally."
  ;;ignore-overlay: (string-width str)
  (phscroll-text-width (car str) (cdr str) cache-arg))

(defun phscroll-truncate-string-to-width (str end-column &optional cache-arg)
  "Return the substring of STR that fits within END-COLUMN columns from the
beginning.

STR must be a value returned by `phscroll-buffer-substring' or
`phscroll-current-line-string'.

CACHE-ARG is the list returned by `phscroll-get-overlay-cache'; if nil,
this function calls `phscroll-get-overlay-cache' internally."
  ;;ignore-overlay: (truncate-string-to-width str end-column)
  (phscroll-log "truncate-string-to-width str=%s end-column=%s cache-arg=%s" str end-column cache-arg)
  (let* ((width 0)
         (prev-width 0)
         (beg (car str))
         (end (cdr str))
         (pos beg)
         (prev-pos pos)
         (cache (or cache-arg (phscroll-get-overlay-cache beg end))))
    (while (and (< pos end) (<= width end-column))
      (let ((width-next (phscroll-char-width-next pos cache)))
        (setq prev-pos pos)
        (setq prev-width width)
        (setq width (+ width (car width-next)))
        (setq pos (cdr width-next))))
    (if (> width end-column)
        (cons (cons beg prev-pos) prev-width)
      (cons (cons beg pos) width))))

(defun phscroll-substring-over-width (str end-column &optional cache-arg)
  "Return the substring of STR that covers at least END-COLUMN columns from
the beginning.

STR must be a value returned by `phscroll-buffer-substring' or
`phscroll-current-line-string'.

CACHE-ARG is the list returned by `phscroll-get-overlay-cache'; if nil,
this function calls `phscroll-get-overlay-cache' internally.

Return value format: (RESULT-STR . RESULT-STR-WIDTH)"
  ;;ignore-overlay:
  ;; (let ((len (phscroll-string-length str))
  ;;       (i 0))
  ;;   (while (and (< i len) (< (phscroll-string-width (phscroll-substring str 0 i)) end-column))
  ;;     (setq i (1+ i)))
  ;;   (phscroll-substring str 0 i))
  (let* ((width 0)
         (beg (car str))
         (end (cdr str))
         (pos beg)
         (cache (or cache-arg (phscroll-get-overlay-cache beg end))))
    (while (and (< pos end) (< width end-column))
      (let ((width-next (phscroll-char-width-next pos cache)))
        (setq width (+ width (car width-next)))
        (setq pos (cdr width-next))))
    (cons (cons beg pos) width)))


;; Text Width Calculation (In Pixels)

(defun phscroll-truncate-string-to-width-px (str end-column)
  (let* ((beg (car str))
         (end (cdr str))
         (pos-and-width (phscroll-find-max-pos-fits-in-width-px
                         beg end (* (frame-char-width) end-column)))
         (pos (car pos-and-width))
         (width (cdr pos-and-width)))

    (cons (cons beg pos)
          (/ (+ width (frame-char-width) -1) (frame-char-width)))))

(defun phscroll-find-max-pos-fits-in-width-px (beg end width &optional window)
  ;; Temporarily unveil folded text
  (let ((overlays (cl-loop for ov in (overlays-in beg end)
                           for invis = (overlay-get ov 'invisible)
                           when (phscroll-ignored-invisibility-spec-p invis)
                           collect (progn
                                     ;; TODO: (if (listp invis) (seq-difference invis phscroll-ignored-invisibility-specs) nil)?
                                     (overlay-put ov 'invisible nil)
                                     (cons ov invis)))))
    (unwind-protect
        (let ((lower beg)
              (upper (1+ end)) ;; Possible answers include END
              (prefix-width (car (window-text-pixel-size nil beg beg 1000000)))
              lower-width)

          (while (> (- upper lower) 1)
            (let* ((pos (/ (+ lower upper) 2)) ;;floor (Never test UPPER)
                   (pos-width (- (car (window-text-pixel-size window beg pos 1000000))
                                 prefix-width)))
              (if (<= pos-width width)
                  (setq lower pos
                        lower-width pos-width)
                (setq upper pos))))
          (cons lower (or lower-width 0)))
      ;; Recover invisible property
      (dolist (ov-invis overlays)
        (overlay-put (car ov-invis) 'invisible (cdr ov-invis))))))




;; Example:
;;asjflasjfl;asjfl;asjfl;asjflasjlf;kajsl;fkjasl;fjasl;fjaslkfjals;fjasklfjasldf
;;1ちまはりちまとりれはちてりはくちれくはれちとくはちくとれはくちとれはちはちとまはりちとまはりとちれは
;;ちまはりちまafasfsdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff$
;; とりれはちてりはくちれくはれちとくはちくとれはくちとれはちはちとまはりちとまはりとちれは
;;ちまはりちまとりれはちてりはくちれくはれちとくはちくとれはくちとれはちはちとまはりちとまはりとちれは



(provide 'phscroll)
;;; phscroll.el ends here
