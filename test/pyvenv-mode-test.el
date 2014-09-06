(ert-deftest pyvenv-mode-should-add-functions ()
  (with-temp-buffer
    (pyvenv-mode 1)

    (should pyvenv-mode)
    (should (member '(pyvenv-mode pyvenv-mode-line-indicator)
                    mode-line-misc-info))
    (should (memq #'pyvenv-track-virtualenv
                  hack-local-variables-hook))))

(ert-deftest pyvenv-mode-should-remove-functions ()
  (with-temp-buffer
    (pyvenv-mode 1)
    (pyvenv-mode -1)

    (should-not pyvenv-mode)
    (should-not (memq '(pyvenv-mode pyvenv-mode-line-indicator)
                      mode-line-misc-info))
    (should-not (memq #'pyvenv-track-virtualenv
                      hack-local-variables-hook))))
