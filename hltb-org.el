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
        (hltb-org-set-drawer-entry property (hltb-org-duration-to-string duration))))

(defun hltb-org-add-cover (game)
  (let ((cover-already-exists-p (member "hltb-cover.jpg" (org-attach-file-list (org-attach-dir-get-create)))))
    (when cover-already-exists-p (org-attach-delete-one "hltb-cover.jpg"))
    (url-copy-file (hltb-game-img game) (file-name-concat (org-attach-dir) "hltb-cover.jpg"))
    (unless cover-already-exists-p
      (open-line 1)
      (insert "[[attachment:hltb-cover.jpg]]"))))

(defun hltb-org-insert-drawer (drawer)
  (if-let ((found (search-forward (format ":%s:" drawer) nil t)))
      (progn
        (backward-char 6)
        found)
    (goto-char (plist-get (cadr (org-element-at-point)) :end))
    (insert ":HLTB:\n:END:\n")
    (previous-line 2)))

(defun hltb-org-delete-line ()
  (interactive)
  (let ((_ (beginning-of-line))
        (begin (point))
        (_ (end-of-line))
        (end (point)))
    (delete-region begin end)))

(defun hltb-org-set-drawer-entry (key value)
  (unless (eq (car (org-element-at-point)) 'drawer)
    (error "Point not in drawer"))
  (let* ((p (point))
         (found (search-forward (format ":%s:" key) nil t))
         (_ (goto-char p))
         (end (search-forward ":END:" nil t)))
    (if (and found (< found end))
        (progn
          (goto-char found)
          (hltb-org-delete-line)
          (insert (format ":%s: %s" key value)))
      (goto-char end)
      (beginning-of-line)
      (open-line 1)
      (insert (format ":%s: %s" key value))
      (goto-char p))))

(defun hltb-org-fill-properties ()
  (interactive)
  (if (eq (car (org-element-at-point)) 'headline)
      (goto-char (org-element-property :begin (org-element-at-point)))
    (org-up-heading-safe))
  (let ((title (org-element-property :title (org-element-at-point))))
    (unless title (error "Org heading not found"))
    (setq title (hltb-org-get-link-description title))
    (let* ((search-entry (hltb-search title))
           (game (hltb-game (hltb-search-entry-id search-entry) search-entry)))
      (unless search-entry (error "HLTB entry not found for title: %s" title))
      (save-excursion
        (hltb-org-insert-drawer "HLTB")
        (hltb-org-set-drawer-entry "ID" (number-to-string (hltb-game-id game)))
        (hltb-org-maybe-set-time game #'hltb-game-main "MAIN")
        (hltb-org-maybe-set-time game #'hltb-game-main+extra "MAIN+EXTRA")
        (hltb-org-maybe-set-time game #'hltb-game-completionist "COMPLETIONIST")
        (hltb-org-set-drawer-entry "GENRE" (hltb-org-make-link-list (hltb-game-genre game) "(Game Genre)"))
        (hltb-org-set-drawer-entry "PUBLISHER" (hltb-org-make-link-list (hltb-game-publisher game) "(Game Publisher)"))
        (hltb-org-set-drawer-entry "DEVELOPER" (hltb-org-make-link-list (hltb-game-developer game) "(Game Developer)")))
      (hltb-org-add-cover game))))
      ; release-date

(provide 'hltb-org)
;;; hltb-org.el ends here
