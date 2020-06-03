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

(defun org-phscroll--fontify-meta-lines-and-blocks (old-func limit)
  (let ((start (point))
        ;; call original function
        (ret-val (funcall old-func limit))
        (table-re "^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"))
    (save-excursion
      (save-restriction
        (widen)

        (goto-char start)
        (while (and (< (point) limit) (re-search-forward table-re limit t))
          (if (null (phscroll-enum-area (max (1- (line-beginning-position)) (point-min))
                                        (line-end-position)))
              ;; if area does not already exists, search region of table
              (let ((beg (save-excursion
                           (let ((lastpos (line-beginning-position)))
                             (while (and (= (forward-line -1) 0) (looking-at table-re))
                               (setq lastpos (point)))
                             lastpos)))
                    (end (progn
                           (while (and (= (forward-line 1) 0) (looking-at table-re)))
                           (point))))
                ;; exclude comment, blocks
                (if (text-property-any beg end 'face 'org-table)
                    ;; create phscroll area
                    (phscroll-region beg end)))))))

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
