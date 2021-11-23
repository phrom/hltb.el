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
         (number-found (when (string-match "We Found \\([0-9]+\\) Games" number-found)
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
        (let ((chosen (completing-read "Game: " (mapcar #'hltb-search-entry-title candidates))))
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

(ert-deftest single-result-test ()
  (let ((result (hltb-search "Railway Empire")))
    (should (equal result
                   (make-hltb-search-entry :title "Railway Empire"
                                    :img "https://howlongtobeat.com/games/52962_Railway_Empire.jpg"
                                    :id 52962
                                    :main 1170
                                    :main+extra 2790
                                    :completionist 7140)))))

(ert-deftest multiple-results-test ()
  (flet ((completing-read (_prompt _candidates) "Fallout 2"))
    (let ((result (hltb-search "Fallout")))
      (should (equal result
                     (make-hltb-search-entry :title "Fallout 2"
                                             :img "https://howlongtobeat.com/games/250px-PC_Game_Fallout_2.jpg"
                                             :id 3339
                                             :main 1830
                                             :main+extra 2880
                                             :completionist 4890))))))

(ert-deftest solo-time-test ()
  (let ((result (hltb-search "Bird Story")))
    (should (equal result
                   (make-hltb-search-entry :title "A Bird Story"
                                    :img "https://howlongtobeat.com/games/ABirdStory_header.jpg"
                                    :id 22526
                                    :main 90)))))

(ert-deftest time-in-minutes-test ()
  (let ((result (hltb-search "Stanley Parable Demo")))
    (should (equal result
                   (make-hltb-search-entry :title "The Stanley Parable Demo"
                                    :img "https://howlongtobeat.com/games/TheStanleyParable.png"
                                    :id 14118
                                    :main 28
                                    :main+extra 38
                                    :completionist 50)))))

(provide 'hltb)
;;; hltb.el ends here
