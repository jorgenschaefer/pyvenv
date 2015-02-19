(ert-deftest pyvenv-hook-dir ()
  ;; Else, should return WORKON_HOME
  (let ((process-environment (cons "WORKON_HOME=/workon_home"
                                   process-environment)))
    (should (equal (pyvenv-workon-home)
                   "/workon_home")))
  ;; Else, should return ~/.virtualenvs
  (let ((process-environment (cons "WORKON_HOME"
                                   process-environment)))
    (should (equal (pyvenv-workon-home)
                   (expand-file-name "~/.virtualenvs"))))
  )
