(ert-deftest pyvenv-virtualenv-list ()
  "Ensure all correct virtualenvs are returned"
  (with-temp-dir workon-home
    (let ((process-environment process-environment))
      (setq process-environment
            (cons (format "WORKON_HOME=%s/does-not-exist" workon-home)
                  process-environment))
      (should-error (pyvenv-virtualenv-list))
      (should (null (pyvenv-virtualenv-list t)))
      (setq process-environment
            (cons (format "WORKON_HOME=%s" workon-home)
                  (cdr process-environment)))
      (should (equal (pyvenv-virtualenv-list) nil))
      (make-directory (format "%s/no-venv-dir" workon-home))
      (write-region "" nil (format "%s/no-venv-file" workon-home))
      (dolist (name '("venv-a" "venv-B" "venv-C"))
        (make-directory (format "%s/%s" workon-home name))
        (make-directory (format "%s/%s/bin" workon-home name))
        (write-region "" nil (format "%s/%s/bin/activate" workon-home name)))
      (should (equal (pyvenv-virtualenv-list)
                     '("venv-a" "venv-B" "venv-C"))))))
