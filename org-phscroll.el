;;; org-phscroll.el --- Apply horizontal scroll to org-mode table -*- lexical-binding: t; -*-

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

;;; Enable wide table horizontal scrolling even if org-startup-truncated is nil.

;;; Usage:

;;; (load "org-phscroll.el")

;;; Code:

(require 'org)
(require 'phscroll)

(defvar org-phscroll-buffer-name-regexp-to-disable
  '(;; see: org-src-font-lock-fontify-block in org-src.el
    "\\` *\\*org-src-fontification:.*\\*\\'"
    ;; see: org-html-fontify-code in ox-html.el
    "\\` *\\*temp\\*-[0-9]+\\'"))

(defvar-local org-phscroll-disabled nil)
(defvar-local org-phscroll-font-lock-keywords (list '(org-phscroll--fontify)))

(define-minor-mode org-phscroll-mode
  "Apply phscroll to org-table."
  :type 'boolean
  :init-value nil
  :group 'org-phscroll

  (if org-phscroll-mode
      (progn
        ;; Do not anything in some buffers.
        (setq-local org-phscroll-disabled
                    (not
                     (null
                      (seq-some
                       (lambda (regexp) (string-match-p regexp (buffer-name)))
                       org-phscroll-buffer-name-regexp-to-disable))))
        ;; (when org-phscroll-disabled
        ;;   (message "Disable org-phscroll-mode in %s" (buffer-name)))
        (unless org-phscroll-disabled
          ;; Do not update on modification-hooks. Update on font-lock.
          (setq-local phscroll-update-area-display-on-modified nil)
          (phscroll-mode 1)
          (font-lock-add-keywords nil org-phscroll-font-lock-keywords t)
          (font-lock-flush)))
    (unless org-phscroll-disabled
      (font-lock-remove-keywords nil org-phscroll-font-lock-keywords)
      (setq-local phscroll-update-area-display-on-modified t)
      (phscroll-delete-all) ;;@todo Keep manually added areas (Identify areas created by org-phscroll, and delete(remove-region) only areas created by org-phscroll when fontify)
      (phscroll-mode -1))))

(defun org-phscroll--fontify (limit)
  (when org-phscroll-mode
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)

          ;; Invalidate areas that overlaps range to fontify.
          ;; Because the text width may have changed due to the
          ;; fontify of the previous keyword (for example, the
          ;; org-link value of the invisible property).
          (phscroll-invalidate-region (point) limit)

          ;; Create/Delete/Move phscroll areas.
          (goto-char (phscroll-line-begin))
          (let* ((start (point))
                 ;; ("|" or "+-[+-]") ... not whitespace
                 (table-re "^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)")
                 ;; [^|+]
                 ;; +([^-] or $)
                 ;; +-([^+-] or $)
                 ;; +-[+-] whitespace...$
                 ;; | whitespace...$
                 ;; $
                 (not-table-re "^[ \t]*\\([^|+ \t]\\|\\+\\([^-]\\|$\\|-\\([^-+]\\|$\\|[-+]\\s-*$\\)\\)\\||\\s-*$\\|$\\)")
                 (phscroll-fontify-range (cons start limit))
                 not-table-beg)
            ;;(message "org-phscroll--fontify start=%s limit=%s line-end=%s" start limit (phscroll-line-end));;debug

            (while (progn
                     (setq not-table-beg (point))
                     (and (< (point) limit)
                          (re-search-forward table-re limit t)))

              (let* ((not-table-end (phscroll-line-begin))
                     (table-beg not-table-end)
                     (table-end (if (re-search-forward not-table-re limit t)
                                    (goto-char (phscroll-line-begin (match-beginning 0))) ;; beginning of not table
                                  (goto-char limit)))) ;; end of fontify
                ;; not table
                (if (< not-table-beg not-table-end)
                    (phscroll-remove-region not-table-beg not-table-end))

                ;; table?
                ;; exclude comment, blocks
                (if (or (text-property-any table-beg table-end 'face 'org-table)
                        (eq (car-safe (get-text-property table-beg 'face)) 'org-table)) ;; for org-modern (org-table (:overline) (:height 0.1))
                    ;; table
                    (progn
                      ;; include previous line if previous line is in area
                      (if (and (= start table-beg)
                               (phscroll-enum-area (phscroll-line-begin (1- start))
                                                   (phscroll-line-end (1- start))))
                          (setq table-beg (phscroll-line-begin (1- start))))
                      ;; include next line if next line is in area
                      (if (phscroll-enum-area (phscroll-line-begin table-end)
                                              (phscroll-line-end table-end))
                          (setq table-end (min (point-max)
                                               (1+ (phscroll-line-end table-end)))))
                      ;; cover single area
                      (phscroll-cover-region table-beg table-end))
                  ;; not table
                  (phscroll-remove-region table-beg table-end))))

            ;; not table
            (if (< not-table-beg limit)
                (phscroll-remove-region not-table-beg limit))))))
    nil))

;; (defun org-phscroll--font-lock-set-keywords ()
;;   ;; See: org-set-font-lock-defaults
;;   (nconc
;;    org-font-lock-extra-keywords
;;    (list
;;     '(org-phscroll--fontify))))


;;;; Support for table column shrink/expand

(defun org-phscroll-invalidate-table (pos)
  (when-let ((area (phscroll-get-area-at pos)))
    (phscroll-area-clear-updated-ranges area)
    ;; @todo necessary?
    ;; (font-lock-unfontify-region
    ;;  (phscroll-area-begin area)
    ;;  (phscroll-area-end area))
    ))

(defun org-phscroll--table-shrink-columns (_columns beg _end &rest _)
  (org-phscroll-invalidate-table beg))

(defun org-phscroll--table-expand (&optional beg _end &rest _)
  (when (and (null beg)
             (org-at-table-p))
    (setq beg (org-table-begin)))

  (when beg
    (org-phscroll-invalidate-table beg)))

;;;; Support for org-table-overlay-coordinates

(defun org-phscroll--table-toggle-coordinate-overlays (&rest _)
  (org-phscroll-invalidate-table (point)))

;;;; Support for org-table-header-line-mode

(defun org-phscroll--after-table-header-line-mode (&rest _)
  ;; Cancel post-command-hook.
  ;; Use phscroll's post-command-hook only.
  (remove-hook 'post-command-hook #'org-table-header-set-header t))

(defun org-phscroll--table-row-get-visible-string (old-func &optional beg)
  (unless beg
    (setq beg (point)))
  (let ((area (phscroll-get-area-at beg)))
    (if (null area)
        ;; Call original
        (funcall old-func beg)
      ;; In phscroll area
      (save-excursion

        ;;@todo Is it possible to unify with around phscroll-char-width-next?
        ;;@todo Support more properties
        (let ((pos (phscroll-line-begin beg))
              (eol (phscroll-line-end beg))
              visible-strs)

          (while (< pos eol)
            (let (next-pos pvalue)
              ;; Overlay
              (let ((overlays (overlays-at pos t)))
                (while (and overlays (null next-pos))
                  (let ((ov (car overlays)))
                    (unless (or (overlay-get ov 'phscroll)
                                (overlay-get ov 'phscroll-ignore))
                      (cond
                       ;; Overlay's display
                       ((setq pvalue (overlay-get ov 'display))
                        (when (stringp pvalue)
                          (push pvalue visible-strs))
                        (setq next-pos (overlay-end ov)))
                       ;; Overlay's invisible
                       ((and (setq pvalue (overlay-get ov 'invisible))
                             (invisible-p pvalue))
                        (setq next-pos (overlay-end ov))))))
                  (setq overlays (cdr overlays))))
              ;; Text property
              (unless next-pos
                (cond
                 ;; Text's display
                 ((setq pvalue (get-text-property pos 'display))
                  (cond
                   ((stringp pvalue)
                    (push pvalue visible-strs))
                   ((and (consp pvalue)
                         (eq (car pvalue) 'space)
                         (eq (cadr pvalue) :relative-width)
                         (integerp (caddr pvalue)))
                    (push (make-string (caddr pvalue) (char-after pos)) visible-strs)))
                  (setq next-pos
                        (next-single-property-change pos 'display nil eol)))
                 ;; Text's invisible
                 ((and (setq pvalue (get-text-property pos 'invisible))
                       (invisible-p pvalue))
                  (setq next-pos
                        (next-single-property-change pos 'invisible nil eol)))
                 ;; Character
                 (t
                  (push (char-to-string (char-after pos)) visible-strs)
                  (setq next-pos (1+ pos)))))
              (setq pos next-pos)))

          (let* ((win-width (phscroll-window-width-at beg nil))
                 (scroll-column (phscroll-get-scroll-column area))
                 (visible-str (apply #'concat (nreverse visible-strs)))
                 (result (truncate-string-to-width
                          visible-str
                          (+ scroll-column win-width)
                          scroll-column ?\s)))
            ;;(message "result=%s" result)
            result))))))

(defun org-phscroll--around-post-command-in-header-line-mode (old-func &rest args)
  (if (and (boundp 'org-table-header-line-mode) ;; Org 9.4 or later
           org-table-header-line-mode)
      (progn
        ;; Delete overlay before phscroll process
        (when (overlayp org-table-header-overlay)
          (delete-overlay org-table-header-overlay)
          (setq org-table-header-overlay nil))
        ;; Call phscroll process
        (prog1 (apply old-func args)
          ;; Update header line
          (ignore-errors
            (org-table-header-set-header))
          (when org-table-header-overlay
            (overlay-put org-table-header-overlay 'phscroll-ignore t)
            (overlay-put org-table-header-overlay 'priority 20)
            (let ((win-start (overlay-start org-table-header-overlay)))
              (move-overlay org-table-header-overlay
                            (phscroll-line-begin win-start)
                            (phscroll-line-end win-start))))))
    ;; Not in header line mode
    (apply old-func args)))

;;;; Support for org-indent

(defun org-phscroll-invalidate-indent (beg end)
  ;;(message "invalidate %s %s" beg end)
  (dolist (area (phscroll-enum-area beg end))
    (phscroll-area-remove-updated-range beg end area)
    ;;(phscroll-area-clear-updated-ranges area)
    ))

(defun org-phscroll--indent-add-properties (beg end &optional _delay)
  (org-phscroll-invalidate-indent beg end))

;;;; Hook global functions

;;;###autoload
(defun org-phscroll-activate ()
  (interactive)
  (add-hook 'org-mode-hook
            #'org-phscroll-mode)
  ;; (add-hook 'org-font-lock-set-keywords-hook
  ;;           #'org-phscroll--font-lock-set-keywords)
  ;; for table shrink/expand
  (advice-add #'org-table--shrink-columns
              :after #'org-phscroll--table-shrink-columns)
  (advice-add #'org-table-expand
              :after #'org-phscroll--table-expand)
  ;; for org-table-overlay-coordinates
  (advice-add #'org-table-toggle-coordinate-overlays
              :after #'org-phscroll--table-toggle-coordinate-overlays)
  ;; for org-table-header-line-mode
  (advice-add #'org-table-header-line-mode
              :after #'org-phscroll--after-table-header-line-mode)
  (advice-add #'org-table-row-get-visible-string
              :around #'org-phscroll--table-row-get-visible-string)
  (advice-add #'phscroll-on-post-command
              :around #'org-phscroll--around-post-command-in-header-line-mode)
  ;; for indent
  (with-no-warnings ;;Some one not needs to (require 'org-indent)
    (advice-add #'org-indent-add-properties
                :after #'org-phscroll--indent-add-properties)))

;;;###autoload
(defun org-phscroll-deactivate ()
  (interactive)
  (remove-hook 'org-mode-hook
               #'org-phscroll-mode)
  ;; (remove-hook 'org-font-lock-set-keywords-hook
  ;;              #'org-phscroll--font-lock-set-keywords)
  ;; for table shrink/expand
  (advice-remove #'org-table--shrink-columns
                 #'org-phscroll--table-shrink-columns)
  (advice-remove #'org-table-expand
                 #'org-phscroll--table-expand)
  ;; for org-table-overlay-coordinates
  (advice-remove #'org-table-toggle-coordinate-overlays
                 #'org-phscroll--table-toggle-coordinate-overlays)
  ;; for org-table-header-line-mode
  (advice-remove #'org-table-header-line-mode
                 #'org-phscroll--after-table-header-line-mode)
  (advice-remove #'org-table-row-get-visible-string
                 #'org-phscroll--table-row-get-visible-string)
  (advice-remove #'phscroll-on-post-command
                 #'org-phscroll--around-post-command-in-header-line-mode)
  ;; for indent
  (with-no-warnings ;;Some one not needs to (require 'org-indent)
    (advice-remove #'org-indent-add-properties
                   #'org-phscroll--indent-add-properties)))


(with-eval-after-load "org"
  (org-phscroll-activate))


(provide 'org-phscroll)
;;; org-phscroll.el ends here
