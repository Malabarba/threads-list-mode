
(ert-deftest weblist--get-in ()
  (should (eq (weblist--get-in '((1 . ((2 . 3)))) '(1 2))
              3)))

(ert-deftest weblist--namify-address ()
  (should (equal (weblist--namify-address '(match-end a home_me))
                 "Home me")))
