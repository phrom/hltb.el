;;; hltb-test.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'hltb)

(ert-deftest hltb-single-result-test ()
  (let ((result (hltb-search "Railway Empire")))
    (should (equal result
                   (make-hltb-search-entry :title "Railway Empire"
					   :img "https://howlongtobeat.com/games/52962_Railway_Empire.jpg"
					   :id 52962
					   :main 1170
					   :main+extra 2790
					   :completionist 7140)))))

(ert-deftest hltb-multiple-results-test ()
  (cl-flet ((completing-read (_prompt _candidates) "Fallout 2"))
    (let ((result (hltb-search "Fallout")))
      (should (equal result
                     (make-hltb-search-entry :title "Fallout 2"
                                             :img "https://howlongtobeat.com/games/250px-PC_Game_Fallout_2.jpg"
                                             :id 3339
                                             :main 1830
                                             :main+extra 2880
                                             :completionist 4890))))))

(ert-deftest hltb-solo-time-test ()
  (let ((result (hltb-search "Bird Story")))
    (should (equal result
                   (make-hltb-search-entry :title "A Bird Story"
					   :img "https://howlongtobeat.com/games/ABirdStory_header.jpg"
					   :id 22526
					   :main 90)))))

(ert-deftest hltb-time-in-minutes-test ()
  (let ((result (hltb-search "Stanley Parable Demo")))
    (should (equal result
                   (make-hltb-search-entry :title "The Stanley Parable Demo"
					   :img "https://howlongtobeat.com/games/TheStanleyParable.png"
					   :id 14118
					   :main 28
					   :main+extra 38
					   :completionist 50)))))

(provide 'ert-test)
