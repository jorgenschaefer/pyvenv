(require 'f)
(add-to-list 'load-path (f-parent (f-dirname (f-this-file))))
(require 'pyvenv)
(require 'mocker)

;; Fixing a bug in dflet where it would not work in 24.3.1 for some
;; reason.
(when (version= emacs-version "24.3.1")
  (require 'cl))

(defmacro with-temp-dir (name &rest body)
  (let ((var (make-symbol "temp-dir")))
    `(let* ((,var (make-temp-file "pyvenv-test-" t))
            (,name ,var))
       (unwind-protect
           (progn
             ,@body)
         (delete-directory ,var t)))))
(put 'with-temp-dir 'lisp-indent-function 'defun)

(defmacro with-temp-virtualenv (name &rest body)
  `(with-temp-dir ,name
     (make-directory (concat ,name "/bin"))
     (write-region "" nil (concat ,name "/bin/activate"))
     ,@body))
(put 'with-temp-virtualenv 'lisp-indent-function 'defun)


(defun canonicalize-environment (process-environment-list)
  "Prepare PROCESS-ENVIRONMENT-LIST variable for comparison."
  (let (result)
    ;; Dedupe by var name.
    (mapc (lambda (x) (cl-pushnew (split-string x "=") result :key 'car :test 'equal))
          process-environment)
    ;; Delete vars that were unset, and sort the result.
    (sort (cl-delete-if-not 'cdr result)
          (lambda (x y) (string-lessp (car x) (car y))))))
