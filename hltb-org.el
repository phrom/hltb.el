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
;; Package-Requires: ((emacs "27.1"))
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
(require 'org-roam)
(require 'emacsql)

(defun hltb-org-get-link-description (link)
  (if (string-match org-link-bracket-re link)
      (match-string 2 link)
    link))

(defun hltb-org-duration-to-string (duration)
  (let ((hours (/ duration 60))
        (minutes (% duration 60)))
    (format "%03d:%02d" hours minutes)))

(defun hltb-org-roam-find-id (title)
  (when-let ((ids (mapcar #'car
                          (org-roam-db-query
                           [:select [nodes:id]
                            :from nodes
                            :where (= title $s1)]
                           title))))
    (car ids)))

(defun hltb-org-roam-id-get-create (title)
  (let ((id (hltb-org-roam-find-id title)))
    (if id
        id
      (let ((node (org-roam-node-create :title title)))
        (org-roam-capture- :node node)
        (hltb-org-roam-find-id title)))))

(defun hltb-org-make-link-list (l suffix)
  (string-join
   (mapcar (lambda (x)
             (let* ((title (concat x " " suffix))
                    (id (hltb-org-roam-id-get-create title)))
               (format "[[id:%s][%s]]" id title)))
           l)
   ", "))

(defun hltb-org-maybe-set-time (game accessor property)
  (when-let ((duration (funcall accessor game))
             (duration (hltb-time-median duration)))
        (org-set-property property (hltb-org-duration-to-string duration))))

(defun hltb-org-fill-properties ()
  (interactive)
  (if (eq (car (org-element-at-point)) 'headline)
      (goto-char (org-element-property :begin (org-element-at-point)))
    (org-up-heading-safe))
  (let ((title (org-element-property :title (org-element-at-point))))
    (unless title (error "Org heading not found"))
    (setq title (hltb-org-get-link-description title))
    (let* ((search-entry (hltb-search title))
           (game (hltb-game (hltb-search-entry-id search-entry) search-entry))
           (cover-already-exists-p (member "hltb-cover.jpg" (org-attach-file-list (org-attach-dir-get-create)))))
      (unless search-entry (error "HLTB entry not found for title: %s" title))
      (org-set-property "HLTB-ID" (number-to-string (hltb-game-id game)))
      (hltb-org-maybe-set-time game #'hltb-game-main "HLTB-MAIN")
      (hltb-org-maybe-set-time game #'hltb-game-main+extra "HLTB-MAIN+EXTRA")
      (hltb-org-maybe-set-time game #'hltb-game-completionist "HLTB-COMPLETIONIST")
      (org-set-property "HLTB-GENRE" (hltb-org-make-link-list (hltb-game-genre game) "(Game Genre)"))
      (org-set-property "HLTB-PUBLISHER" (hltb-org-make-link-list (hltb-game-publisher game) "(Game Publisher)"))
      (org-set-property "HLTB-DEVELOPER" (hltb-org-make-link-list (hltb-game-developer game) "(Game Developer)"))
      (when cover-already-exists-p (org-attach-delete-one "hltb-cover.jpg"))
      (url-copy-file (hltb-game-img game) (file-name-concat (org-attach-dir) "hltb-cover.jpg"))
      (unless cover-already-exists-p
        (open-line 1)
        (insert "[[attachment:hltb-cover.jpg]]")))))
      ; release-date

(provide 'hltb-org)
;;; hltb-org.el ends here
