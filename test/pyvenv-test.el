(ert-deftest pyvenv-activate ()
  "`pyvenv-activate' should set the correct variables."
  (with-temp-virtualenv tmpdir
    (let* ((process-environment process-environment)
           (exec-path exec-path)
           (pyvenv-virtual-env nil)
           (pyvenv-virtual-env-name nil)
           (pyvenv-old-process-environment nil)
           (pyvenv-old-exec-path nil)
           (pre-activate-venv nil)
           (post-activate-venv nil)
           (pyvenv-pre-activate-hooks
            (list (lambda ()
                    (setq pre-activate-venv pyvenv-virtual-env))))
           (pyvenv-post-activate-hooks
            (list (lambda ()
                    (setq post-activate-venv pyvenv-virtual-env)))))
      (pyvenv-activate tmpdir)
      (should (equal pre-activate-venv tmpdir))
      (should (equal post-activate-venv tmpdir))
      (should (equal pyvenv-virtual-env tmpdir))
      (should (equal pyvenv-virtual-env-name (file-name-base tmpdir)))
      (should (equal (getenv "VIRTUAL_ENV")
                     tmpdir))
      (should (string-match (format "^%s/bin" (regexp-quote tmpdir))
                            (getenv "PATH")))
      (should (equal (getenv "PYTHONHOME")
                     nil))
      (should (member (format "%s/bin" tmpdir)
                      exec-path)))))

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
      (should (equal pre-deactivate-venv venv1))
      (should (equal post-deactivate-venv venv1))
      (should (equal process-environment orig-process-environment))
      (should (equal exec-path orig-exec-path))
      (should (equal pyvenv-virtual-env nil))
      (should (equal pyvenv-virtual-env-name nil))
      (with-temp-virtualenv venv2
        ;; Should retain the originals, too.
        (pyvenv-activate venv1)
        (pyvenv-activate venv2)
        (pyvenv-deactivate)
        ;; Called for both, but the last one was for the second
        (should (equal pre-deactivate-venv venv2))
        (should (equal post-deactivate-venv venv2))
        (should (equal process-environment orig-process-environment))
        (should (equal exec-path orig-exec-path))
        (should (equal pyvenv-virtual-env nil))
        (should (equal pyvenv-virtual-env-name nil))))))

;; Fixing a bug in dflet where it would not work in 24.3.1 for some
;; reason.
(when (version= emacs-version "24.3.1")
  (require 'cl))

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

(ert-deftest pyvenv-virtualenv-list ()
  "Ensure all correct virtualenvs are returned"
  (with-temp-dir workon-home
    (let ((process-environment process-environment))
      (setq process-environment
            (cons (format "WORKON_HOME=%s/does-not-exist" workon-home)
                  process-environment))
      (should-error (pyvenv-virtualenv-list))
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
