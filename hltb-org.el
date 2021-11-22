;;; hltb-org.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pedro Henrique Romano
;;
;; Author: Pedro Henrique Romano <https://github.com/phr>
;; Maintainer: Pedro Henrique Romano <mail@pedroromano.org>
;; Created: November 22, 2021
;; Modified: November 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/phr/hltb-org
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'hltb)
(require 'org)

(defun hltb-org-get-link-description (link)
  (if (string-match org-link-bracket-re link)
      (match-string 2 link)
    link))

(defun hltb-org-duration-to-string (duration)
  (let ((hours (/ duration 60))
        (minutes (% duration 60)))
    (format "%03d:%02d" hours minutes)))

(defun hltb-org-fill-properties ()
  (interactive)
  (if (eq (car (org-element-at-point)) 'headline)
      (goto-char (org-element-property :begin (org-element-at-point)))
    (org-up-heading-safe))
  (let ((title (org-element-property :title (org-element-at-point))))
    (unless title (error "Org heading not found"))
    (setq title (hltb-org-get-link-description title))
    (let ((entry (hltb-search title))
          (cover-already-exists-p (member "hltb-cover.jpg" (org-attach-file-list (org-attach-dir-get-create)))))
      (unless entry (error "HLTB entry not found for title: %s" title))
      (org-set-property "HLTB-ID" (number-to-string (hltb-entry-id entry)))
      (when-let ((duration (hltb-entry-main entry)))
        (org-set-property "HLTB-MAIN" (hltb-org-duration-to-string duration)))
      (when-let ((duration (hltb-entry-main+extra entry)))
        (org-set-property "HLTB-MAIN+EXTRA" (hltb-org-duration-to-string duration)))
      (when-let ((duration (hltb-entry-completionist entry)))
        (org-set-property "HLTB-COMPLETIONIST" (hltb-org-duration-to-string duration)))
      (when cover-already-exists-p (org-attach-delete-one "hltb-cover.jpg"))
      (url-copy-file (hltb-entry-img entry) (file-name-concat (org-attach-dir) "hltb-cover.jpg"))
      (unless cover-already-exists-p
        (open-line 1)
        (insert "[[attachment:hltb-cover.jpg]]")))))

(provide 'hltb-org)
;;; hltb-org.el ends here
