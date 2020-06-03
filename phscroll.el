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

;;; 1. (require 'phscroll)
;;; 2. Select region you want to hscroll.
;;; 3. M-x phscroll-region

;;; Code:


;; TODO:
;; - [X] truncate-lines によって動作を変える。truncate-linesのときは何もしない。切り替えを検出できるか
;; - [X] remove-overlaysのところ
;; - phscroll-update-area-displayが遅い
;; - [X] 既存の水平スクロール操作に対応?(scroll-left, scroll-right)
;; - 複数ウィンドウの挙動、特に左右に分割した場合で左右のサイズが異なる場合はどうしようもない。最小幅を使うしか？　何もしない方が良い？

(define-minor-mode phscroll-mode

  "Partial horizontal scroll mode"
  :global nil
  (cond
   (phscroll-mode
    (setq phscroll-truncate-lines truncate-lines)
    (add-hook 'post-command-hook #'phscroll-on-post-command nil t)
    (add-hook 'window-size-change-functions #'phscroll-on-window-size-changed nil t)
    (add-hook 'pre-redisplay-functions #'phscroll-on-pre-redisplay nil t)
    )
   (t
    (remove-hook 'post-command-hook #'phscroll-on-post-command t)
    (remove-hook 'window-size-change-functions #'phscroll-on-window-size-changed t)
    (remove-hook 'pre-redisplay-functions #'phscroll-on-pre-redisplay t)
    )))

(defun phscroll-region (beg end)
  "Make a partial horizontal scroll area."
  (interactive "r")
  (setq beg (phscroll-line-begin beg))
  (setq end (phscroll-line-end end))

  (when (< beg end)
    ;; turn on phscroll-mode
    (if (not phscroll-mode)
        (phscroll-mode))

    ;; create overlay and area object
    (let* ((overlap-areas (phscroll-enum-area beg end))
           (area (car overlap-areas)))
      (if area
          ;; reuse area
          (let* ((ov (phscroll-area-overlay area))
                 (area-beg (overlay-start ov))
                 (area-end (overlay-end ov)))
            ;; if area range changed
            (when (or (not (= area-beg beg))
                      (not (= area-end end)))
              ;; maintain updated-ranges
              ;; (adjust related position & remove ranges out of area)
              (cond
               ((< beg area-beg)
                (phscroll-area-shift-updated-ranges-after area-beg (- area-beg beg) area))
               ((> beg area-beg)
                (phscroll-area-remove-updated-range area-beg beg area)
                (phscroll-area-shift-updated-ranges-after area-beg (- area-beg beg) area))
               ((< end area-end)
                (phscroll-area-remove-updated-range end area-end area)))

              ;; move overlay
              (move-overlay ov beg end)
              ;; update new area range
              (phscroll-update-area-display area))
            ;; destroy other overlap areas
            (loop for ooa in (cdr overlap-areas)
                  do (phscroll-area-destroy ooa)))
        ;; create new area
        (setq area (phscroll-area-create beg end))
        (phscroll-update-area-display area t))
      area)))

(defun phscroll-delete-at (&optional pos)
  "Delete area."
  (interactive "d")
  (phscroll-area-destroy (phscroll-get-area-at pos)))

(defun phscroll-delete-all ()
  (interactive)
  (loop for area in (phscroll-enum-area)
        do (phscroll-area-destroy area)))



;;
;; Area object
;;

(defun phscroll-area-create (beg end)
  (let* ((ov (make-overlay beg end))
         (area (list
                'phscroll ;;0
                0  ;; 1:scroll-column
                ov ;; 2:overlay
                (list (cons -1 -1)) ;; 3:updated-ranges
                )))
    (overlay-put ov 'phscroll t)
    (overlay-put ov 'phscroll-area area)
    (overlay-put ov 'modification-hooks (list #'phscroll-on-modified))
    (phscroll-area-set-keymap area)
    area))

(defun phscroll-area-destroy (area)
  (when area
    (let ((beg (phscroll-area-begin area))
          (end (phscroll-area-end area))
          (ov (phscroll-area-overlay area)))
      (remove-overlays beg end 'phscroll-left t)
      (remove-overlays beg end 'phscroll-right t)
      (delete-overlay ov))))

;; Area Scroll Position

(setq phscroll-interactive-scroll-commands
      '(phscroll-set-scroll-column
        phscroll-scroll-left
        phscroll-scroll-right))

(defun phscroll-get-scroll-column (&optional area)
  (nth 1 (or area (phscroll-get-current-area))))

(defun phscroll-set-scroll-column (pos &optional area)
  (interactive "nColumn: ")
  (if (null area)
      (setq area (phscroll-get-current-area)))
  (if (< pos 0)
      (setq pos 0))
  (when (and area (not (= (phscroll-get-scroll-column area) pos)))
    (setcar (nthcdr 1 area) pos)
    (phscroll-update-area-display area t)))

(defun phscroll-add-scroll-column (delta &optional area)
  (if (null area)
      (setq area (phscroll-get-current-area)))
  (when area
    (phscroll-set-scroll-column
     (+ (phscroll-get-scroll-column area) delta)
     area)))

(defun phscroll-scroll-left (&optional arg area)
  (interactive "P")
  (phscroll-add-scroll-column
   (if arg (prefix-numeric-value arg) (- (window-width) 2))
   area))

(defun phscroll-scroll-right (&optional arg area)
  (interactive "P")
  (phscroll-add-scroll-column
   (- (if arg (prefix-numeric-value arg) (- (window-width) 2)))
   area))


(defun phscroll-show-point (pos)
  (let ((area (phscroll-get-area-at pos)))
    (if area
        (let ((scroll-column (phscroll-get-scroll-column area))
              (pos-column (phscroll-string-width
                           (phscroll-buffer-substring
                            (phscroll-line-begin pos) pos)))
              (window-width (phscroll-window-width pos)))
          (cond
           ((< pos-column scroll-column)
            (phscroll-set-scroll-column pos-column area))
           ((> pos-column (+ scroll-column window-width))
            (phscroll-set-scroll-column (- pos-column window-width) area)))))))

(defun phscroll-scroll-point (pos)
  (let ((area (phscroll-get-area-at pos)))
    (if area
        (let ((scroll-column (phscroll-get-scroll-column area))
              (pos-column (phscroll-string-width
                           (phscroll-buffer-substring
                            (phscroll-line-begin pos) pos)))
              (window-width (phscroll-window-width pos))
              (step (if (= hscroll-step 0)
                        (/ (1+ (phscroll-window-width pos)) 2)
                      hscroll-step)))
          (cond
           ((< pos-column (+ scroll-column hscroll-margin))
            (phscroll-set-scroll-column (max 0 (- pos-column hscroll-margin step)) area))
           ((> pos-column (+ scroll-column (- window-width hscroll-margin)))
            (phscroll-set-scroll-column (+ (- pos-column window-width) hscroll-margin step) area)))))))


;; Area Overlay

(defun phscroll-area-overlay (&optional area)
  (nth 2 (or area (phscroll-get-current-area))))

;; Area Range

(defun phscroll-area-begin (&optional area)
  (overlay-start (phscroll-area-overlay (or area (phscroll-get-current-area)))))

(defun phscroll-area-end (&optional area)
  (overlay-end (phscroll-area-overlay (or area (phscroll-get-current-area)))))

;; Area Finding

(defun phscroll-get-area-at (pos)
  (let (area
        (overlays (overlays-at pos)))
    (while (and overlays
                (null (setq area (overlay-get (car overlays) 'phscroll-area))))
      (setq overlays (cdr overlays)))
    area))

(defun phscroll-get-current-area ()
  (phscroll-get-area-at (point)))

(defun phscroll-enum-area (&optional beg end)
  (interactive "r")

  (let* ((overlays (overlays-in (or beg (point-min)) (or end (point-max)))))
    (loop for ov in overlays
          if (overlay-get ov 'phscroll-area)
          collect (overlay-get ov 'phscroll-area))))

(defun phscroll-update-all-area ()
  (save-restriction
    (widen)
    (loop for area in (phscroll-enum-area)
          do (phscroll-update-area-display area t))))

(defun phscroll-areas-in-window (&optional window)
  (phscroll-enum-area
   (min (window-start window) (point))
   (max (window-end window) (point))))

(defun phscroll-update-areas-in-window (&optional redraw window)
  (loop for area in (phscroll-areas-in-window window)
        do (phscroll-update-area-display area redraw window)))

;;
;; Updated Range Management
;;
;; updated-ranges:
;; ((-1 . -1) (beg0 . end0) (beg1 . end1) ... (begN . endN))

(defun phscroll-area-updated-ranges-head (area)
  (nth 3 area))

(defun phscroll-area-clear-updated-ranges (area)
  (when area
    (setcdr (nth 3 area) nil))
  area)

(defun phscroll-area-add-updated-range (beg end area)
  ;; 1. check and normalize range (beg, end), if empty then exit
  (when (and (< beg end) area)
    (let ((area-begin (phscroll-area-begin area))
          (area-end (phscroll-area-end area)))
      (if (< beg area-begin) (setq beg area-begin))
      (if (> end area-end) (setq end area-end))
      (when (< beg end)

        ;; convert to relative position
        (setq beg (- beg area-begin))
        (setq end (- end area-begin))

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
  ;; 1. check and normalize range (beg, end), if empty then exit
  (when (and (< beg end) area)
    (let ((area-begin (phscroll-area-begin area))
          (area-end (phscroll-area-end area)))
      (if (< beg area-begin) (setq beg area-begin))
      (if (> end area-end) (setq end area-end))
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

;; (setq test-area (phscroll-area-create 10 22))
;; (phscroll-area-add-updated-range 12 14 test-area)
;; (phscroll-area-add-updated-range 16 18 test-area)
;; (phscroll-area-add-updated-range 20 22 test-area)
;; (phscroll-area-add-updated-range 13 16 test-area)
;; (phscroll-area-remove-updated-range 13 21 test-area)
;; (phscroll-area-remove-updated-range 10 22 test-area)

(defun phscroll-area-needs-update-range (beg end area)
  (when (and (< beg end) area)
    (let ((area-begin (phscroll-area-begin area))
          (area-end (phscroll-area-end area)))
      (if (< beg area-begin) (setq beg area-begin))
      (if (> end area-end) (setq end area-end))
      (when (< beg end)

        ;; convert to relative position
        (setq beg (- beg area-begin))
        (setq end (- end area-begin))

        (not
         (find-if
          (lambda (range) (and (<= (car range) beg) (<= end (cdr range))))
          (phscroll-area-updated-ranges-head area)))))))

;;(phscroll-area-needs-update-range 21 23 test-area)

(defun phscroll-area-shift-updated-ranges-after (pos delta area)
  (when area
    (let ((area-begin (phscroll-area-begin area))
          (r (cdr (phscroll-area-updated-ranges-head area))))

      ;; convert to relative position
      (setq pos (- pos area-begin))

      (while (and r (< (cdar r) pos))
        (setq r (cdr r)))
      (when r
        (if (<= pos (caar r)) (setcar (car r) (+ (caar r) delta)))
        (if (<= pos (cdar r)) (setcdr (car r) (+ (cdar r) delta)))
        (setq r (cdr r))
        (while r
          (setcar (car r) (+ (caar r) delta))
          (setcdr (car r) (+ (cdar r) delta))
          (setq r (cdr r))))))
  area)

;;(phscroll-area-shift-updated-ranges-after 51 10 test-area)


;;
;; Scroll
;;



;;
;; Event Handlers
;;

(defun phscroll-on-window-size-changed (&optional frame)
  ;;(message "window-size-changed width=%s" (window-width))
  (phscroll-update-all-area))

(defun phscroll-on-post-command ()
  ;(message "on post command window-end=%s" (window-end))
  ;;(phscroll-show-point (point))

  (unless (find this-command phscroll-interactive-scroll-commands)
    (phscroll-scroll-point (point)))
  ;;(phscroll-update-area-display (phscroll-get-area-at (point)))
  (phscroll-update-areas-in-window nil nil)
  )

(defun phscroll-on-pre-redisplay (&optional window)
  ;;(message "redisplay window=%s width=%s window-end=%s %s" window (window-width) (window-end) (window-end window))
  (phscroll-check-truncate-lines)
  (phscroll-update-areas-in-window nil window))

(defun phscroll-on-modified (ov after beg end &optional before-length)
  ;;(message "modified %s %s %s %s" after beg end before-length)
  (when after
    (let* ((after-length (- end beg))
           (delta-length (- after-length before-length))
           (area (phscroll-get-area-at beg)))
      (when area
        (cond
         ((> delta-length 0)
          (phscroll-area-shift-updated-ranges-after beg delta-length area)
          (phscroll-area-remove-updated-range (phscroll-line-begin beg) (phscroll-line-end end) area))

         ((< delta-length 0)
          (phscroll-area-remove-updated-range (phscroll-line-begin beg) (phscroll-line-end (+ beg before-length)) area)
          (phscroll-area-shift-updated-ranges-after end delta-length area))

         (t
          (phscroll-area-remove-updated-range (phscroll-line-begin beg) (phscroll-line-end end) area)))

        ;;;@todo do pre-redisplay only?
        (phscroll-update-area-display area)))))


(defvar-local phscroll-truncate-lines nil)

(defun phscroll-check-truncate-lines ()
  (when (not (equal truncate-lines phscroll-truncate-lines))
    (setq phscroll-truncate-lines truncate-lines)
    (if phscroll-truncate-lines
        ;; remove left, right overlays
        (loop for area in (phscroll-enum-area)
              do (let ((beg (phscroll-area-begin area))
                       (end (phscroll-area-end area)))
                   (remove-overlays beg end 'phscroll-left t)
                   (remove-overlays beg end 'phscroll-right t))))
    ;; remove update-ranges and redraw
    (phscroll-update-all-area)))


(setq phscroll-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-x<" 'phscroll-scroll-left)
        (define-key map "\C-x>" 'phscroll-scroll-right)
        map))
(defun phscroll-area-set-keymap (area)
  (if area
      (overlay-put (phscroll-area-overlay area) 'keymap phscroll-keymap)))




;;
;; Area Display
;;

(defvar phscroll-margin-right 4)

(defun phscroll-window-width (pos)
  (- (window-width) phscroll-margin-right (length (get-text-property pos 'wrap-prefix))))

(defun phscroll-update-area-display (area &optional redraw window)
  (when (and area (not phscroll-truncate-lines))
    (if redraw (phscroll-area-clear-updated-ranges area))

    (let* ((scroll-column (phscroll-get-scroll-column area))
           (area-begin (phscroll-area-begin area))
           (area-end (phscroll-area-end area))
           (update-begin (max area-begin (phscroll-line-begin (min (window-start window) (point)))))
           (update-end (min area-end (max (window-end window) (point)))))

      (when (and (< update-begin update-end)
                 (phscroll-area-needs-update-range update-begin update-end area))
        ;; for each lines
        (save-excursion
          (goto-char update-begin)
          (beginning-of-line)
          (while (< (point) update-end)
            (let ((line-begin (line-beginning-position))
                  (line-end (line-end-position)))
              (when (phscroll-area-needs-update-range line-begin line-end area)
                ;;(message "update line %d" (point))
                (remove-overlays line-begin (1+ line-end) 'phscroll-left t) ;;include line-break
                (remove-overlays line-begin (1+ line-end) 'phscroll-right t) ;;include line-break
                (phscroll-update-current-line-display scroll-column))
              (forward-line))))

        ;; add updated range
        (phscroll-area-add-updated-range update-begin update-end area)
        ))))

(defun phscroll-update-current-line-display (scroll-column)
  ;; | line                            |
  ;; | left(9) | middle(14)   | right  |  <part(limit)
  ;; |ABCDEFGHI|JKLMNOPQRSTUVW|XYZ01234|
  ;; |あいうえおかきくけこ
  (let* ((window-width (phscroll-window-width (point)))
         ;; current line
         (line-str (phscroll-current-line-string))
         (line-len (phscroll-string-length line-str))
         (line-begin (line-beginning-position))
         (line-end (line-end-position))
         ;; left invisible
         (left-limit-width scroll-column)
         (left-str (phscroll-substring-over-width line-str left-limit-width))
         (left-width (phscroll-string-width left-str)) ;; 0 ~ left-limit-width + overflow
         (left-overflow (max 0 (- left-width scroll-column))) ;; 0 ~ overflow
         (left-len (phscroll-string-length left-str))
         ;; middle visible
         (middle-limit-width (- window-width left-overflow))
         (middle-str (phscroll-truncate-string-to-width (phscroll-substring line-str left-len) middle-limit-width))
         (middle-width (phscroll-string-width middle-str))
         (middle-shortage (- middle-limit-width middle-width))
         (middle-len (phscroll-string-length middle-str)))

    (when (> scroll-column 0)
      (let ((ov (make-overlay line-begin (+ line-begin left-len))))
        (overlay-put ov 'display (concat "<" (make-string left-overflow ?\s)))
        (overlay-put ov 'phscroll t)
        (overlay-put ov 'phscroll-left t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 10)))

    (when (< (+ line-begin left-len middle-len) line-end)
      (let ((ov (make-overlay (+ line-begin left-len middle-len) line-end)))
        (overlay-put ov 'display (concat (make-string middle-shortage ?\s) ">"))
        (overlay-put ov 'phscroll t)
        (overlay-put ov 'phscroll-right t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 10)))
    ))


;;
;; Text Width Utilities
;;

(defun phscroll-line-begin (&optional pos)
  (if pos
      (save-excursion (goto-char pos) (line-beginning-position))
    (line-beginning-position)))

(defun phscroll-line-end (&optional pos)
  (if pos
      (save-excursion (goto-char pos) (line-end-position))
    (line-end-position)))

(defun phscroll-char-width-next (pos)
  (let* (display
         invisible
         (overlays (overlays-at pos t))
         (display-ov (find-if (lambda (ov) (and (overlay-get ov 'display) (null (overlay-get ov 'phscroll)))) overlays))) ;;exclude 'phscroll overlays
    (cond
     ;; display (overlay or text property)
     ((setq display (or (if display-ov (overlay-get display-ov 'display))
                        (get-text-property pos 'display)))
      (cons
       ;; width
       (cond
        ;;;@todo support more formats
        ;; string
        ((stringp display)
         (string-width display))
        ;; list
        ((listp display)
         (cond
          ((eq (car display) 'space)
           (let* ((props (cdr display))
                  (width (plist-get props :width))
                  (relative-width (plist-get props :relative-width)))
             (+
              (if (integerp width) width 0)
              (if (integerp relative-width) relative-width 0))))
          (t 0)))
        ;; unknown
        (t 0))
       ;; next pos
       (if display-ov (overlay-end display-ov) (1+ pos))))

     ;; invisible
     ((progn (setq invisible (get-char-property-and-overlay pos 'invisible))
             (car invisible))
      (let ((ov (cdr invisible)))
        (cons
         0
         (if ov (overlay-end ov) (1+ pos)))))
     ;; normal character
     (t
      (cons
       (char-width (char-after pos))
       (1+ pos))))))

(defun phscroll-text-width (beg end)
  (let ((width 0)
        (pos beg))
    (while (< pos end)
      (let ((width-next (phscroll-char-width-next pos)))
        (setq width (+ width (car width-next)))
        (setq pos (cdr width-next))))
    width))

(defun phscroll-buffer-substring (beg end)
  ;;ignore-overlay: (buffer-substring-no-properties beg end)
  (cons beg end)
  )

(defun phscroll-current-line-string ()
  (phscroll-buffer-substring (line-beginning-position) (line-end-position)))

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

(defun phscroll-string-width (str)
  ;;ignore-overlay: (string-width str)
  (phscroll-text-width (car str) (cdr str))
  )

(defun phscroll-truncate-string-to-width (str end-column)
  ;;ignore-overlay: (truncate-string-to-width str end-column)
  (let* ((len (phscroll-string-length str))
         (width 0)
         (beg (car str))
         (end (cdr str))
         (pos beg)
         (prev-pos pos))
    (while (and (< pos end) (<= width end-column))
      (let ((width-next (phscroll-char-width-next pos)))
        (setq prev-pos pos)
        (setq width (+ width (car width-next)))
        (setq pos (cdr width-next))))
    (cons beg
          (if (> width end-column) prev-pos pos))))

(defun phscroll-substring-over-width (str end-column)
  ;;ignore-overlay:
  ;; (let ((len (phscroll-string-length str))
  ;;       (i 0))
  ;;   (while (and (< i len) (< (phscroll-string-width (phscroll-substring str 0 i)) end-column))
  ;;     (setq i (1+ i)))
  ;;   (phscroll-substring str 0 i))
  (let* ((len (phscroll-string-length str))
         (width 0)
         (beg (car str))
         (end (cdr str))
         (pos beg))
    (while (and (< pos end) (< width end-column))
      (let ((width-next (phscroll-char-width-next pos)))
        (setq width (+ width (car width-next)))
        (setq pos (cdr width-next))))
    (cons beg pos)))




;; Example:
;;asjflasjfl;asjfl;asjfl;asjflasjlf;kajsl;fkjasl;fjasl;fjaslkfjals;fjasklfjasldf
;;1ちまはりちまとりれはちてりはくちれくはれちとくはちくとれはくちとれはちはちとまはりちとまはりとちれは
;;ちまはりちまafasfsdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff$
;; とりれはちてりはくちれくはれちとくはちくとれはくちとれはちはちとまはりちとまはりとちれは
;;ちまはりちまとりれはちてりはくちれくはれちとくはちくとれはくちとれはちはちとまはりちとまはりとちれは



(provide 'phscroll)
;;; phscroll.el ends here
