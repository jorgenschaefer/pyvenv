(ert-deftest pyvenv-deactivate ()
  "Should set variables back to the original values."
  (let* ((process-environment process-environment)
         (exec-path exec-path)
         (orig-process-environment process-environment)
         (orig-exec-path exec-path)
         (pre-deactivate-venv nil)
         (post-deactivate-venv nil)
         (pyvenv-pre-deactivate-hooks
          (list (lambda ()
                  (setq pre-deactivate-venv pyvenv-virtual-env))))
         (pyvenv-post-deactivate-hooks
          (list (lambda ()
                  (setq post-deactivate-venv pyvenv-virtual-env)))))
    (with-temp-virtualenv venv1
      (pyvenv-activate venv1)
      (pyvenv-deactivate)
      (should (f-same? pre-deactivate-venv venv1))
      (should (f-same? post-deactivate-venv venv1))
      (should (equal (canonicalize-environment process-environment)
                     (canonicalize-environment orig-process-environment)))
      (should (equal exec-path orig-exec-path))
      (should (equal pyvenv-virtual-env nil))
      (should (equal pyvenv-virtual-env-name nil))
      (with-temp-virtualenv venv2
        ;; Should retain the originals, too.
        (pyvenv-activate venv1)
        (pyvenv-activate venv2)
        (pyvenv-deactivate)
        ;; Called for both, but the last one was for the second
        (should (f-same? pre-deactivate-venv venv2))
        (should (f-same? post-deactivate-venv venv2))
        (should (equal (canonicalize-environment process-environment)
                       (canonicalize-environment orig-process-environment)))
        (should (equal exec-path orig-exec-path))
        (should (equal pyvenv-virtual-env nil))
        (should (equal pyvenv-virtual-env-name nil))
        (should (equal python-shell-virtualenv-root nil))
        (should (equal python-shell-virtualenv-path nil))))))
