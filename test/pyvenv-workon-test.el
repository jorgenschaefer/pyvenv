(ert-deftest pyvenv-workon ()
  ;; Should expand the virtualenv directory then call activate.
  (mocker-let ((pyvenv-activate (dir)
                                ((:input-matcher (lambda (env)
                                                   (string-match "/test-env$"
                                                                 env))
                                  :output t))))
    (should (pyvenv-workon "test-env")))

  ;; Should not activate anything when the user just hits RET.
  (mocker-let ((pyvenv-activate (dir)
                                ((:min-occur 0 :max-occur 0))))
    (should-not (pyvenv-workon "")))

  ;; Some completion frameworks can return nil for the default, see
  ;; https://github.com/jorgenschaefer/elpy/issues/144
  (mocker-let ((pyvenv-activate (dir)
                                ((:min-occur 0 :max-occur 0))))
    (should-not (pyvenv-workon nil))))
