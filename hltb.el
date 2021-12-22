;;; hltb.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pedro Henrique Romano
;;
;;
;; Author: Pedro Henrique Romano <https://github.com/phr>
;; Maintainer: Pedro Henrique Romano <mail@pedroromano.org>
;; Created: November 19, 2021
;; Modified: November 19, 2021
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'dom)
(require 'org)
(require 'org-attach)
(require 'org-roam)
(require 'emacsql)

(defun hltb-url-retrieve-post (url form-data)
  (let ((url-request-method        "POST")
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data          form-data))
    (with-current-buffer (url-retrieve-synchronously url)
      (libxml-parse-html-region (point-min) (point-max)))))

(cl-defstruct hltb-search-entry
  id title img main main+extra completionist)

(cl-defstruct hltb-time
  polled average median rushed leisure)

(cl-defstruct hltb-game
  id title img main main+extra completionist platform genre developer publisher release-date)

(defun hltb-parse-time (time)
  (let* ((number (string-to-number time))
         (half (string-match-p "½" time))
         (hour-p (string-match-p "Hour" time))
         (result (if hour-p
                     (+ (* 60 number) (if half 30 0))
                   number)))
    (when (> result 0) result)))

(defun hltb-search (name)
  (let* ((response (hltb-url-retrieve-post "https://howlongtobeat.com/search_results?page=1"
                                           (format "queryString=%s&t=games&sorthead=popular&sortd=0&plat=&length_type=main&length_min=&length_max=&v=&f=&g=&detail=&randomize=0" name)))
         (number-found (caddar (dom-by-tag response 'h3)))
         (_ (when (string-match "We Found \\([0-9]+\\) Games" number-found)
              (string-to-number (match-string 1 number-found))))
         (candidates (mapcar (lambda (li)
                               (let* ((a (car (dom-by-tag li 'a)))
                                      (href (dom-attr a 'href))
                                      (img (car (dom-by-tag a 'img)))
                                      (tidbits (dom-by-class li "^search_list_tidbit")))
                                 (make-hltb-search-entry :id (when (string-match "game\\?id=\\([0-9]+\\)" href)
                                                               (string-to-number (match-string 1 href)))
                                                         :title (dom-attr a 'title)
                                                         :img (concat "https://howlongtobeat.com" (dom-attr img 'src))
                                                         :main (hltb-parse-time (dom-text (nth 1 tidbits)))
                                                         :main+extra (hltb-parse-time (dom-text (nth 3 tidbits)))
                                                         :completionist (hltb-parse-time (dom-text (nth 5 tidbits))))))
                             (dom-by-tag response 'li))))
    (if (cdr candidates)
        (let ((chosen (completing-read (format "Choose result for search \"%s\": " name) (mapcar #'hltb-search-entry-title candidates))))
          (cl-find-if (lambda (x) (string-equal (hltb-search-entry-title x) chosen)) candidates))
      (car candidates))))

(defun hltb-game-get-profile-info (profile-info name)
  (when-let ((element (cl-find-if (lambda (x) (string-match-p (regexp-quote name) (dom-text (dom-by-tag x 'strong))))
                                  profile-info)))
    (mapcar #'string-trim (split-string (dom-text element) ","))))

(defun hltb-time-table-parse (time)
  (let* ((split (split-string time " "))
         (hours (cl-find-if (lambda (x) (string-suffix-p "h" x)) split))
         (minutes (cl-find-if (lambda (x) (string-suffix-p "m" x)) split)))
    (+ (if hours (* 60 (string-to-number hours)) 0)
       (if minutes (string-to-number minutes) 0))))

(defun hltb-game-get-time (time-table heading)
  (when-let* ((element (cl-find-if (lambda (x) (string-match-p (regexp-quote heading) (dom-text (car (dom-by-tag x 'td)))))
                                  time-table))
              (tds (dom-by-tag element 'td)))
    (make-hltb-time :polled (string-to-number (dom-text (nth 1 tds)))
                    :average (hltb-time-table-parse (dom-text (nth 2 tds)))
                    :median (hltb-time-table-parse (dom-text (nth 3 tds)))
                    :rushed (hltb-time-table-parse (dom-text (nth 4 tds)))
                    :leisure (hltb-time-table-parse (dom-text (nth 5 tds))))))

(defun hltb-game (id &optional search-entry)
  (let* ((response (with-current-buffer (url-retrieve-synchronously (format "https://howlongtobeat.com/game?id=%s" id))
                     (libxml-parse-html-region (point-min) (point-max))))
         (title (if search-entry
                    (hltb-search-entry-title search-entry)
                  (string-trim (dom-text (dom-by-class response "^profile_header ")))))
         (img (if search-entry
                  (hltb-search-entry-img search-entry)
                (concat "https://howlongtobeat.com" (dom-attr (dom-child-by-tag (dom-by-class response "^game_image") 'img) 'src))))
         (time-table (dom-by-class response "^spreadsheet$"))
         (main (hltb-game-get-time time-table "Main Story"))
         (main+extra (hltb-game-get-time time-table "Main + Extras"))
         (completionist (hltb-game-get-time time-table "Completionist"))
         (profile-info (seq-drop-while (lambda (x) (string-match-p "large$" (dom-attr x 'class)))
                                       (dom-by-class response "^profile_info")))
         (platform (hltb-game-get-profile-info profile-info "Platform"))
         (genre (hltb-game-get-profile-info profile-info "Genre"))
         (developer (hltb-game-get-profile-info profile-info "Developer"))
         (publisher (hltb-game-get-profile-info profile-info "Publisher"))
         (release-date-na (hltb-game-get-profile-info profile-info "NA"))
         (release-date-eu (hltb-game-get-profile-info profile-info "EU"))
         (release-date-jp (hltb-game-get-profile-info profile-info "JP"))
         (release-date (list)))
    (when release-date-na (push (cons 'na release-date-na) release-date))
    (when release-date-eu (push (cons 'eu release-date-eu) release-date))
    (when release-date-jp (push (cons 'jp release-date-jp) release-date))
    (make-hltb-game :id id
                    :title title
                    :img img
                    :main main
                    :main+extra main+extra
                    :completionist completionist
                    :platform platform
                    :genre genre
                    :developer developer
                    :publisher publisher
                    :release-date release-date)))

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

(defun hltb-org-make-link (title)
  (let ((id (hltb-org-roam-id-get-create title)))
    (format "[[id:%s][%s]]" id title)))

(defun hltb-org-make-link-list (l suffix)
  (string-join
   (mapcar (lambda (x) (hltb-org-make-link (concat x " " suffix))) l)
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
      (beginning-of-line)
    (goto-char (plist-get (cadr (org-element-at-point)) :end))
    (insert ":HLTB:\n:END:\n")
    (forward-line -2)))

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
      (insert (format ":%s: %s" key value)))
    (goto-char p)))

(defun hltb-org-convert-date (date)
  (let ((month-day (split-string (car date) " ")))
    (format "%s-%s-%s"
            (cadr date)
            (pcase (car month-day)
              ("January" "01")
              ("February" "02")
              ("March" "03")
              ("April" "04")
              ("May" "05")
              ("June" "06")
              ("July" "07")
              ("August" "08")
              ("September" "09")
              ("October" "10")
              ("November" "11")
              ("December" "12"))
            (cadr month-day))))

(defun hltb-org-set-release-date (game prop name)
  (when-let ((release-date (alist-get prop (hltb-game-release-date game))))
    (hltb-org-set-drawer-entry name (hltb-org-make-link (hltb-org-convert-date release-date)))))

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
        (hltb-org-set-drawer-entry "DEVELOPER" (hltb-org-make-link-list (hltb-game-developer game) "(Game Developer)"))
        (hltb-org-set-release-date game 'na "RELEASE-DATE-NA")
        (hltb-org-set-release-date game 'eu "RELEASE-DATE-EU")
        (hltb-org-set-release-date game 'jp "RELEASE-DATE-JP"))
      (hltb-org-add-cover game))))

(provide 'hltb)
;;; hltb.el ends here
