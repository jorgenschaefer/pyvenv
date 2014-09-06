(ert-deftest pyvenv-track-virtualenv-should-activate-venv-if-none-set ()
  (with-temp-buffer
    (mocker-let ((pyvenv-activate (env)
                                  ((:input '("/test/path")))))
      (let ((pyvenv-activate "/test/path")
            (pyvenv-virtual-env nil))

        (pyvenv-track-virtualenv)))))

(ert-deftest pyvenv-track-virtualenv-should-ask-if-activate-different-venv ()
  (with-temp-buffer
    (mocker-let ((pyvenv-activate (env)
                                  ((:input '("/test/path"))))
                 (y-or-n-p (arg) ((:input '("Switch to virtual env /test/path (currently /other/path)? ")
                                   :output t))))
      (let ((pyvenv-activate "/test/path")
            (pyvenv-virtual-env "/other/path"))

        (pyvenv-track-virtualenv)))))

(ert-deftest pyvenv-track-virtualenv-should-do-nothing-if-activate-same-venv ()
  (with-temp-buffer
    (let ((pyvenv-activate "/test/path")
          ;; activate takes precedence over workon
          (pyvenv-workon "foo")
          (pyvenv-virtual-env "/test/path"))
      (pyvenv-track-virtualenv)

      (should (equal pyvenv-virtual-env "/test/path")))))

(ert-deftest pyvenv-track-virtualenv-should-workon-venv-if-none-set ()
  (with-temp-buffer
    (mocker-let ((pyvenv-workon (env)
                                ((:input '("test-venv")))))
      (let ((pyvenv-workon "test-venv")
            (pyvenv-virtual-env nil))

        (pyvenv-track-virtualenv)))))

(ert-deftest pyvenv-track-virtualenv-should-ask-if-workon-different-venv ()
  (with-temp-buffer
    (mocker-let ((pyvenv-workon (env)
                                ((:input '("test-venv"))))
                 (y-or-n-p (arg) ((:input '("Switch to virtual env test-venv (currently other-venv)? ")
                                   :output t))))
      (let ((pyvenv-workon "test-venv")
            (pyvenv-virtual-env "/some/path/other-venv")
            (pyvenv-virtual-env-name "other-venv"))

        (pyvenv-track-virtualenv)))))

(ert-deftest pyvenv-track-virtualenv-should-do-nothing-if-workon-same-venv ()
  (with-temp-buffer
    (let ((pyvenv-workon "test-venv")
          (pyvenv-virtual-env-name "test-venv"))
      (pyvenv-track-virtualenv)

      (should (equal pyvenv-virtual-env-name "test-venv")))))
