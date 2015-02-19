(ert-deftest pyvenv-hook-dir ()
  ;; Should return VIRTUALENVWRAPPER_HOOK_DIR
  (let ((process-environment (cons "VIRTUALENVWRAPPER_HOOK_DIR=/hook_dir"
                                   process-environment)))
    (should (equal (pyvenv-hook-dir)
                   "/hook_dir")))
  ;; Else, should return WORKON_HOME
  (let ((process-environment (append '("VIRTUALENVWRAPPER_HOOK_DIR"
                                       "WORKON_HOME=/workon_home")
                                   process-environment)))
    (should (equal (pyvenv-hook-dir)
                   "/workon_home")))
  ;; Else, should return ~/.virtualenvs
  (let ((process-environment (append '("VIRTUALENVWRAPPER_HOOK_DIR"
                                       "WORKON_HOME")
                                   process-environment)))
    (should (equal (pyvenv-hook-dir)
                   (expand-file-name "~/.virtualenvs"))))
  )
