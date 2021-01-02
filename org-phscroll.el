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

(require 'phscroll)

(define-minor-mode org-phscroll-mode
  "Apply phscroll to org-table."
  :init-value t
  (progn
    (message "body %s" org-phscroll-mode)
    (if org-phscroll-mode
        (font-lock-flush)
      (phscroll-delete-all))))

(defun org-phscroll--fontify-meta-lines-and-blocks (old-func limit)
  (let* ((start (point))
         ;; call original function
         (ret-val (funcall old-func limit))
         ;; ("|" or "+-[+-]") ... not whitespace
         (table-re "^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)")
         ;; [^|+]
         ;; +([^-] or $)
         ;; +-([^+-] or $)
         ;; +-[+-] whitespace...$
         ;; | whitespace...$
         (not-table-re "^[ \t]*\\([^|+ \t]\\|\\+\\([^-]\\|$\\|-\\([^-+]\\|$\\|[-+]\\s-*$\\)\\)\\||\\s-*$\\)")
         not-table-beg
         (phscroll-fontify-range (cons start limit)))
    ;;(message "fontify start=%s limit=%s point=%s line-end=%s" start limit (point) (line-end-position));;debug

    (when org-phscroll-mode
      (save-excursion
        (save-restriction
          (widen)

          (goto-char start)
          (beginning-of-line)
          (setq start (point))

          (while (progn
                   (setq not-table-beg (point))
                   (and (< (point) limit)
                        (re-search-forward table-re limit t)))
            ;; not table
            (let ((not-table-end (line-beginning-position)))
              (if (< not-table-beg not-table-end)
                  (phscroll-remove-region not-table-beg not-table-end)))

            ;; table
            (let ((table-beg (line-beginning-position))
                  (table-end (if (re-search-forward not-table-re limit t)
                                 (progn
                                   (beginning-of-line) ;; next not-table-beg
                                   (point))
                               (goto-char limit) ;; end of fontify
                               (point))))
              ;; exclude comment, blocks
              (if (text-property-any table-beg table-end 'face 'org-table)
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
              (phscroll-remove-region not-table-beg limit)))))
    ret-val))

(defun org-phscroll-activate ()
  (interactive)
  (advice-add #'org-fontify-meta-lines-and-blocks :around #'org-phscroll--fontify-meta-lines-and-blocks))

(defun org-phscroll-deactivate ()
  (interactive)
  (advice-remove #'org-fontify-meta-lines-and-blocks #'org-phscroll--fontify-meta-lines-and-blocks))


(with-eval-after-load "org"
  (org-phscroll-activate))


(provide 'org-phscroll)
;;; org-phscroll.el ends here
