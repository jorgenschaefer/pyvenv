(ert-deftest pyvenv--env-diff ()
  ;; This test has a simple convention: lowercase strings are old values,
  ;; uppercase strings are new values.
  (should (equal (pyvenv--env-diff '() '())
                 '()))
  (should (equal (pyvenv--env-diff '((a . "a")) '())
                 '((a . nil))))
  (should (equal (pyvenv--env-diff '() '((a . "A")))
                 '((a . "A"))))

  (should (equal (pyvenv--env-diff '((a . "a")) '((b . "B")))
                 '((a . nil) (b . "B"))))
  (should (equal (pyvenv--env-diff '((c . "c")) '((b . "B")))
                 '((b . "B") (c . nil))))
  (should (equal (pyvenv--env-diff '((b . "b")) '((a . "A") (c . "C")))
                 '((a . "A") (b . nil) (c . "C")))))
