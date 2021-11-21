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

(cl-defstruct hltb-entry
  id title img main main+extra completionist)

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
                                 (make-hltb-entry :id (when (string-match "game\\?id=\\([0-9]+\\)" href)
                                                        (string-to-number (match-string 1 href)))
                                                  :title (dom-attr a 'title)
                                                  :img (concat "https://howlongtobeat.com" (dom-attr img 'src))
                                                  :main (hltb-parse-time (dom-text (nth 1 tidbits)))
                                                  :main+extra (hltb-parse-time (dom-text (nth 3 tidbits)))
                                                  :completionist (hltb-parse-time (dom-text (nth 5 tidbits))))))
                             (dom-by-tag response 'li))))
    (if (cdr candidates)
        (let ((chosen (completing-read "Game: " (mapcar #'hltb-entry-title candidates))))
          (cl-find-if (lambda (x) (string-equal (hltb-entry-title x) chosen)) candidates))
      (car candidates))))

(ert-deftest single-result-test ()
  (let ((result (hltb-search "Railway Empire")))
    (should (equal result
                   (make-hltb-entry :title "Railway Empire"
                                    :img "https://howlongtobeat.com/games/52962_Railway_Empire.jpg"
                                    :id 52962
                                    :main 1170
                                    :main+extra 2790
                                    :completionist 7140)))))

(ert-deftest multiple-results-test ()
  (cl-flet* ((completing-read (_prompt _candidates) "Fallout 2")
             (result (hltb-search "Bird Story")))
    (should (equal result
                   (make-hltb-entry :title "Fallout 2"
                                    :img "https://howlongtobeat.com/games/250px-PC_Game_Fallout_2.jpg"
                                    :id 3339
                                    :main 1830
                                    :main+extra 2880
                                    :completionist 4890)))))

(ert-deftest solo-time-test ()
  (let ((result (hltb-search "Bird Story")))
    (should (equal result
                   (make-hltb-entry :title "A Bird Story"
                                    :img "https://howlongtobeat.com/games/ABirdStory_header.jpg"
                                    :id 22526
                                    :main 90)))))

(ert-deftest time-in-minutes-test ()
  (let ((result (hltb-search "Stanley Parable Demo")))
    (should (equal result
                   (make-hltb-entry :title "The Stanley Parable Demo"
                                    :img "https://howlongtobeat.com/games/TheStanleyParable.png"
                                    :id 14118
                                    :main 28
                                    :main+extra 38
                                    :completionist 50)))))


;(setq result (hltb-search "Stanley Parable Demo"))
;(hltb-entry-main result)

(provide 'hltb)
;;; hltb.el ends here
