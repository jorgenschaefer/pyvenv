;;; pyvenv.el --- Python virtual environment interface -*- lexical-binding: t -*-

;; Copyright (C) 2013-2017  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: http://github.com/jorgenschaefer/pyvenv
;; Version: 1.21
;; Keywords: Python, Virtualenv, Tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple global minor mode which will replicate the changes
;; done by virtualenv activation inside Emacs.

;; The main entry points are `pyvenv-activate', which queries the user
;; for a virtual environment directory to activate, and
;; `pyvenv-workon', which queries for a virtual environment in
;; $WORKON_HOME (from virtualenvwrapper.sh).

;; If you want your inferior Python processes to be restarted
;; automatically when you switch your virtual environment, add
;; `pyvenv-restart-python' to `pyvenv-post-activate-hooks'.

;;; Code:

(require 'eshell)
(require 'json)
(require 'subr-x)

;; User customization

(defgroup pyvenv nil
  "Python Virtual Environment Interface."
  :prefix "pyvenv-"
  :group 'languages)

(defcustom pyvenv-workon nil
  "The intended virtualenv in the virtualenvwrapper directory.

This is rarely useful to set globally. Rather, set this in file-
or directory-local variables using \\[add-file-local-variable] or
\\[add-dir-local-variable].

When `pyvenv-mode' is enabled, pyvenv will switch to this
virtualenv. If a virtualenv is already enabled, it will ask first."
  :type 'pyvenv-workon
  :safe #'stringp
  :group 'pyvenv)

(defcustom pyvenv-activate nil
  "The intended virtualenv directory.

This is rarely useful to set globally. Rather, set this in file-
or directory-local variables using \\[add-file-local-variable] or
\\[add-dir-local-variable].

When `pyvenv-mode' is enabled, pyvenv will switch to this
virtualenv. If a virtualenv is already enabled, it will ask first."
  :type 'directory
  :safe #'stringp
  :group 'pyvenv)

(defcustom pyvenv-tracking-ask-before-change nil
  "Non-nil means pyvenv will ask before automatically changing a virtualenv.

This can happen when a new file is opened with a buffer-local
value (from file-local or directory-local variables) for
`pyvenv-workon' or `pyvenv-workon', or if `pyvenv-tracking-mode'
is active, after every command."
  :type 'boolean
  :group 'pyvenv)

(defcustom pyvenv-virtualenvwrapper-python
  (or (getenv "VIRTUALENVWRAPPER_PYTHON")
      (executable-find "python3")
      (executable-find "python")
      (executable-find "py")
      (executable-find "pythonw")
      "python")
  "The python process which has access to the virtualenvwrapper module.

This should be $VIRTUALENVWRAPPER_PYTHON outside of Emacs, but
virtualenvwrapper.sh does not export that variable. We make an
educated guess, but that can be off."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'pyvenv)

(defcustom pyvenv-exec-shell
  (or (executable-find "bash")
      (executable-find "sh")
      shell-file-name)
  "The path to a POSIX compliant shell to use for running
  virtualenv hooks. Useful if you use a non-POSIX shell (e.g.
  fish)."
  :type '(file :must-match t)
  :group 'pyvenv)

(defcustom pyvenv-default-virtual-env-name nil
  "Default directory to use when prompting for a virtualenv directory
in `pyvenv-activate'."
  :type 'string
  :group 'pyvenv)

;; API for other libraries

(defvar pyvenv-virtual-env nil
  "The current virtual environment.

Do not set this variable directly; use `pyvenv-activate' or
`pyvenv-workon'.")

(defvar pyvenv-virtual-env-path-directories nil
  "Directories added to PATH by the current virtual environment.

Do not set this variable directly; use `pyvenv-activate' or
`pyvenv-workon'.")

(defvar pyvenv-virtual-env-name nil
  "The name of the current virtual environment.

This is usually the base name of `pyvenv-virtual-env'.")


(defvar pyvenv-pre-create-hooks nil
  "Hooks run before a virtual environment is created.")


(defvar pyvenv-post-create-hooks nil
  "Hooks run after a virtual environment is created.")


(defvar pyvenv-pre-activate-hooks nil
  "Hooks run before a virtual environment is activated.

`pyvenv-virtual-env' is already set.")

(defvar pyvenv-post-activate-hooks nil
  "Hooks run after a virtual environment is activated.

`pyvenv-virtual-env' is set.")

(defvar pyvenv-pre-deactivate-hooks nil
  "Hooks run before a virtual environment is deactivated.

`pyvenv-virtual-env' is set.")

(defvar pyvenv-post-deactivate-hooks nil
  "Hooks run after a virtual environment is deactivated.

`pyvenv-virtual-env' is still set.")

(defvar pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                     ("[" pyvenv-virtual-env-name "] "))
  "How `pyvenv-mode' will indicate the current environment in the mode line.")

;; Internal code.

(defvar pyvenv-old-process-environment nil
  "The old process environment that needs to be restored after deactivating the current environment.")


(defun pyvenv-create (venv-name python-executable)
  "Create virtualenv.  VENV-NAME  PYTHON-EXECUTABLE."
  (interactive (list
                (read-from-minibuffer "Name of virtual environment: ")
                (let ((dir (if pyvenv-virtualenvwrapper-python
                               (file-name-directory pyvenv-virtualenvwrapper-python)
                             nil))
                      (initial (if pyvenv-virtualenvwrapper-python
                                   (file-name-base pyvenv-virtualenvwrapper-python)
                                 nil)))
                  (read-file-name "Python interpreter to use: " dir nil nil initial))))
  (let ((venv-dir (concat (file-name-as-directory (pyvenv-workon-home))
                          venv-name)))
    (unless (file-exists-p venv-dir)
      (run-hooks 'pyvenv-pre-create-hooks)
      (cond
       ((executable-find "virtualenv")
        (with-current-buffer (generate-new-buffer "*virtualenv*")
          (call-process "virtualenv" nil t t
                        "-p" python-executable venv-dir)
          (display-buffer (current-buffer))))
       ((= 0 (call-process python-executable nil nil nil
                           "-m" "venv" "-h"))
        (with-current-buffer (generate-new-buffer "*venv*")
          (call-process python-executable nil t t
                        "-m" "venv" venv-dir)
          (display-buffer (current-buffer))))
       (t
        (error "Pyvenv necessitates the 'virtualenv' python package")))
      (run-hooks 'pyvenv-post-create-hooks))
    (pyvenv-activate venv-dir)))


;;;###autoload
(defun pyvenv-activate (directory)
  "Activate the virtual environment in DIRECTORY."
  (interactive (list (read-directory-name "Activate venv: " nil nil nil
					  pyvenv-default-virtual-env-name)))
  (setq directory (expand-file-name directory))
  (pyvenv-deactivate)
  (setq pyvenv-virtual-env (file-name-as-directory directory)
        pyvenv-virtual-env-name (file-name-nondirectory
                                 (directory-file-name directory))
        python-shell-virtualenv-path directory
        python-shell-virtualenv-root directory)
  ;; Set venv name as parent directory for generic directories or for
  ;; the user's default venv name
  (when (or (member pyvenv-virtual-env-name '("venv" ".venv" "env" ".env"))
	    (and pyvenv-default-virtual-env-name
		 (string= pyvenv-default-virtual-env-name
			  pyvenv-virtual-env-name)))
    (setq pyvenv-virtual-env-name
          (file-name-nondirectory
           (directory-file-name
            (file-name-directory
             (directory-file-name directory))))))
  (pyvenv-run-virtualenvwrapper-hook "pre_activate" nil pyvenv-virtual-env)
  (run-hooks 'pyvenv-pre-activate-hooks)
  (setq pyvenv-virtual-env-path-directories (pyvenv--virtual-env-bin-dirs directory)
        ;; Variables that must be reset during deactivation.
        pyvenv-old-process-environment (list (cons "PYTHONHOME" (getenv "PYTHONHOME"))
                                       (cons "VIRTUAL_ENV" nil)))
  (setenv "VIRTUAL_ENV" directory)
  (setenv "PYTHONHOME" nil)
  (pyvenv--add-dirs-to-PATH pyvenv-virtual-env-path-directories)
  (pyvenv-run-virtualenvwrapper-hook "post_activate" 'propagate-env)
  (run-hooks 'pyvenv-post-activate-hooks))

;;;###autoload
(defun pyvenv-deactivate ()
  "Deactivate any current virtual environment."
  (interactive)
  (when pyvenv-virtual-env
    (pyvenv-run-virtualenvwrapper-hook "pre_deactivate" 'propagate-env)
    (run-hooks 'pyvenv-pre-deactivate-hooks)
    (pyvenv--remove-dirs-from-PATH (pyvenv--virtual-env-bin-dirs pyvenv-virtual-env))
    (dolist (envvar pyvenv-old-process-environment)
      (setenv (car envvar) (cdr envvar)))
    ;; Make sure PROPAGATE-ENV is nil here, so that it does not change
    ;; `exec-path', as $PATH is different
    (pyvenv-run-virtualenvwrapper-hook "post_deactivate"
                                 nil
                                 pyvenv-virtual-env)
    (run-hooks 'pyvenv-post-deactivate-hooks))
  (setq pyvenv-virtual-env nil
        pyvenv-virtual-env-path-directories nil
        pyvenv-virtual-env-name nil
        python-shell-virtualenv-root nil
        python-shell-virtualenv-path nil))

(defvar pyvenv-workon-history nil
  "Prompt history for `pyvenv-workon'.")

;;;###autoload
(defun pyvenv-workon (name)
  "Activate a virtual environment from $WORKON_HOME.

If the virtual environment NAME is already active, this function
does not try to reactivate the environment."
  (interactive
   (list
    (completing-read "Work on: " (pyvenv-virtualenv-list)
                     nil t nil 'pyvenv-workon-history nil nil)))
  (unless (member name (list "" nil pyvenv-virtual-env-name))
    (pyvenv-activate (format "%s/%s"
                             (pyvenv-workon-home)
                             name))))

(defun pyvenv-virtualenv-list (&optional noerror)
  "Prompt the user for a name in $WORKON_HOME.

If NOERROR is set, do not raise an error if WORKON_HOME is not
configured."
  (let ((workon-home (pyvenv-workon-home))
        (result nil))
    (if (not (file-directory-p workon-home))
        (when (not noerror)
          (error "Can't find a workon home directory, set $WORKON_HOME"))
      (dolist (name (directory-files workon-home))
        (when (or (file-exists-p (format "%s/%s/bin/activate"
                                         workon-home name))
                  (file-exists-p (format "%s/%s/bin/python"
                                         workon-home name))
                  (file-exists-p (format "%s/%s/Scripts/activate.bat"
                                         workon-home name))
                  (file-exists-p (format "%s/%s/python.exe"
                                         workon-home name)))
          (setq result (cons name result))))
      (sort result (lambda (a b)
                     (string-lessp (downcase a)
                                   (downcase b)))))))

(define-widget 'pyvenv-workon 'choice
  "Select an available virtualenv from virtualenvwrapper."
  :convert-widget
  (lambda (widget)
    (setq widget (widget-copy widget))
    (widget-put widget
                :args (cons '(const :tag "None" nil)
                            (mapcar (lambda (env)
                                      (list 'const env))
                                    (pyvenv-virtualenv-list t))))
    (widget-types-convert-widget widget))

  :prompt-value (lambda (_widget prompt _value _unbound)
                  (let ((name (completing-read
                               prompt
                               (cons "None"
                                     (pyvenv-virtualenv-list t))
                               nil t)))
                    (if (equal name "None")
                        nil
                      name))))

(defvar pyvenv-mode-map (make-sparse-keymap)
  "The mode keymap for `pyvenv-mode'.")

(easy-menu-define pyvenv-menu pyvenv-mode-map
  "Pyvenv Menu"
  '("Virtual Envs"
    :visible pyvenv-mode
    ("Workon"
     :help "Activate a virtualenvwrapper environment"
     :filter (lambda (&optional ignored)
               (mapcar (lambda (venv)
                         (vector venv `(pyvenv-workon ,venv)
                                 :style 'radio
                                 :selected `(equal pyvenv-virtual-env-name
                                                   ,venv)))
                       (pyvenv-virtualenv-list t))))
    ["Activate" pyvenv-activate
     :help "Activate a virtual environment by directory"]
    ["Deactivate" pyvenv-deactivate
     :help "Deactivate the current virtual environment"
     :active pyvenv-virtual-env
     :suffix pyvenv-virtual-env-name]
    ["Restart Python Processes" pyvenv-restart-python
     :help "Restart all Python processes to use the current environment"]))

;;;###autoload
(define-minor-mode pyvenv-mode
  "Global minor mode for pyvenv.

Will show the current virtualenv in the mode line, and respect a
`pyvenv-workon' setting in files."
  :global t
  (cond
   (pyvenv-mode
    (add-to-list 'mode-line-misc-info '(pyvenv-mode pyvenv-mode-line-indicator))
    (add-hook 'hack-local-variables-hook #'pyvenv-track-virtualenv))
   ((not pyvenv-mode)
    (setq mode-line-misc-info (delete '(pyvenv-mode pyvenv-mode-line-indicator)
                                      mode-line-misc-info))
    (remove-hook 'hack-local-variables-hook #'pyvenv-track-virtualenv))))

;;;###autoload
(define-minor-mode pyvenv-tracking-mode
  "Global minor mode to track the current virtualenv.

When this mode is active, pyvenv will activate a buffer-specific
virtualenv whenever the user switches to a buffer with a
buffer-local `pyvenv-workon' or `pyvenv-activate' variable."
  :global t
  (if pyvenv-tracking-mode
      (add-hook 'post-command-hook 'pyvenv-track-virtualenv)
    (remove-hook 'post-command-hook 'pyvenv-track-virtualenv)))

(defun pyvenv-track-virtualenv ()
  "Set a virtualenv as specified for the current buffer.

If either `pyvenv-activate' or `pyvenv-workon' are specified, and
they specify a virtualenv different from the current one, switch
to that virtualenv."
  (cond
   (pyvenv-activate
    (when (and (not (equal (file-name-as-directory pyvenv-activate)
                           pyvenv-virtual-env))
               (or (not pyvenv-tracking-ask-before-change)
                   (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                     pyvenv-activate pyvenv-virtual-env))))
      (pyvenv-activate pyvenv-activate)))
   (pyvenv-workon
    (when (and (not (equal pyvenv-workon pyvenv-virtual-env-name))
               (or (not pyvenv-tracking-ask-before-change)
                   (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                     pyvenv-workon pyvenv-virtual-env-name))))
      (pyvenv-workon pyvenv-workon)))))

(defun pyvenv-run-virtualenvwrapper-hook (hook &optional propagate-env &rest args)
  "Run a virtualenvwrapper hook, and update the environment.

This will run a virtualenvwrapper hook and update the local
environment accordingly.

CAREFUL! If PROPAGATE-ENV is non-nil, this will modify your
`process-environment' and `exec-path'."
  (when (pyvenv-virtualenvwrapper-supported)
    (with-temp-buffer
      (let ((tmpfile (make-temp-file "pyvenv-virtualenvwrapper-"))
            (shell-file-name pyvenv-exec-shell))
        (unwind-protect
            (let ((default-directory (pyvenv-workon-home)))
              (apply #'call-process
                     pyvenv-virtualenvwrapper-python
                     nil t nil
                     "-m" "virtualenvwrapper.hook_loader"
                     "--script" tmpfile
                     (if (getenv "HOOK_VERBOSE_OPTION")
                         (cons (getenv "HOOK_VERBOSE_OPTION")
                               (cons hook args))
                       (cons hook args)))
              (call-process-shell-command
               (mapconcat 'identity
                          (list
                           (format "%s -c 'import os, json; print(json.dumps(dict(os.environ)))'"
                                   pyvenv-virtualenvwrapper-python)
                           (format ". '%s'" tmpfile)
                           (format
                            "%s -c 'import os, json; print(\"\\n=-=-=\"); print(json.dumps(dict(os.environ)))'"
                            pyvenv-virtualenvwrapper-python))
                          "; ")
               nil t nil))
          (delete-file tmpfile)))
      (goto-char (point-min))
      (when (not (re-search-forward "No module named '?virtualenvwrapper'?" nil t))
        (let* ((json-key-type 'string)
               (env-before (json-read))
               (hook-output-start-pos (point))
               (hook-output-end-pos (when (re-search-forward "\n=-=-=\n" nil t)
                                      (match-beginning 0)))
               (env-after (when hook-output-end-pos (json-read))))
          (when hook-output-end-pos
            (let ((output (string-trim (buffer-substring hook-output-start-pos
                                            hook-output-end-pos))))
              (when (> (length output) 0)
                (with-help-window "*Virtualenvwrapper Hook Output*"
                  (with-current-buffer "*Virtualenvwrapper Hook Output*"
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert
                       (format
                        "Output from the virtualenvwrapper hook %s:\n\n"
                        hook)
                       output))))))
            (when propagate-env
              (dolist (binding (pyvenv--env-diff (sort env-before (lambda (x y) (string-lessp (car x) (car y))))
                                           (sort env-after (lambda (x y) (string-lessp (car x) (car y))))))
                (setenv (car binding) (cdr binding))
                (when (eq (car binding) 'PATH)
                  (let ((new-path-elts (split-string (cdr binding)
                                                     path-separator)))
                    (setq exec-path new-path-elts)
                    (setq-default eshell-path-env new-path-elts)))))))))))


(defun pyvenv--env-diff (env-before env-after)
  "Calculate diff between ENV-BEFORE alist and ENV-AFTER alist.

Both ENV-BEFORE and ENV-AFTER must be sorted alists of (STR . STR)."
  (let (env-diff)
    (while (or env-before env-after)
      (cond
       ;; K-V are the same, both proceed to the next one.
       ((equal (car-safe env-before) (car-safe env-after))
        (setq env-before (cdr env-before)
              env-after (cdr env-after)))

       ;; env-after is missing one element: add (K-before . nil) to diff
       ((and env-before (or (null env-after) (string-lessp (caar env-before)
                                                           (caar env-after))))
        (setq env-diff (cons (cons (caar env-before) nil) env-diff)
              env-before (cdr env-before)))
       ;; Otherwise: add env-after element to the diff, progress env-after,
       ;; progress env-before only if keys matched.
       (t
        (setq env-diff (cons (car env-after) env-diff))
        (when (equal (caar env-after) (caar env-before))
          (setq env-before (cdr env-before)))
        (setq env-after (cdr env-after)))))
    (nreverse env-diff)))


;;;###autoload
(defun pyvenv-restart-python ()
  "Restart Python inferior processes."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (eq major-mode 'inferior-python-mode)
                 (get-buffer-process buf))
        (let ((cmd (combine-and-quote-strings (process-command
                                               (get-buffer-process buf))))
              (dedicated (if (string-match "\\[.*\\]$" (buffer-name buf))
                             t
                           nil))
              (show nil))
          (delete-process (get-buffer-process buf))
          (goto-char (point-max))
          (insert "\n\n"
                  "###\n"
                  (format "### Restarting in virtualenv %s (%s)\n"
                          pyvenv-virtual-env-name pyvenv-virtual-env)
                  "###\n"
                  "\n\n")
          (run-python cmd dedicated show)
          (goto-char (point-max)))))))

(defun pyvenv-hook-dir ()
  "Return the current hook directory.

This is usually the value of $VIRTUALENVWRAPPER_HOOK_DIR, but
virtualenvwrapper has stopped exporting that variable, so we go
back to the default of $WORKON_HOME or even just ~/.virtualenvs/."
  (or (getenv "VIRTUALENVWRAPPER_HOOK_DIR")
      (pyvenv-workon-home)))

(defun pyvenv-workon-home ()
  "Return the current workon home.

This is the value of $WORKON_HOME or ~/.virtualenvs."
  (or (getenv "WORKON_HOME")
      (expand-file-name "~/.virtualenvs")))

(defun pyvenv-virtualenvwrapper-supported ()
  "Return true iff virtualenvwrapper is supported.

Right now, this just checks if WORKON_HOME is set."
  (getenv "WORKON_HOME"))

(defun pyvenv--virtual-env-bin-dirs (virtual-env)
  (let ((virtual-env
	 (if (string= "/" (directory-file-name virtual-env))
	     ""
	   (directory-file-name virtual-env))))
   (append
    ;; Unix
    (when (file-exists-p (format "%s/bin" virtual-env))
      (list (format "%s/bin" virtual-env)))
    ;; Windows
    (when (file-exists-p (format "%s/Scripts" virtual-env))
      (list (format "%s/Scripts" virtual-env)
            ;; Apparently, some virtualenv
            ;; versions on windows put the
            ;; python.exe in the virtualenv root
            ;; for some reason?
            virtual-env)))))

(defun pyvenv--replace-once-destructive (list oldvalue newvalue)
  "Replace one element equal to OLDVALUE with NEWVALUE values in LIST."
  (let ((cur-elt list))
    (while (and cur-elt (not (equal oldvalue (car cur-elt))))
      (setq cur-elt (cdr cur-elt)))
    (when cur-elt (setcar cur-elt newvalue))))

(defun pyvenv--remove-many-once (values-to-remove list)
  "Return a copy of LIST with each element from VALUES-TO-REMOVE removed once."
  ;; Non-interned symbol is not eq to anything but itself.
  (let ((values-to-remove (copy-sequence values-to-remove))
        (sentinel (make-symbol "sentinel")))
    (delq sentinel
          (mapcar (lambda (x)
                    (if (pyvenv--replace-once-destructive values-to-remove x sentinel)
                        sentinel
                      x))
                  list))))

(defun pyvenv--prepend-to-pathsep-string (values-to-prepend str)
  "Prepend values from VALUES-TO-PREPEND list to path-delimited STR."
  (mapconcat 'identity
             (append values-to-prepend (split-string str path-separator))
             path-separator))

(defun pyvenv--remove-from-pathsep-string (values-to-remove str)
  "Remove all values from VALUES-TO-REMOVE list from path-delimited STR."
  (mapconcat 'identity
             (pyvenv--remove-many-once values-to-remove (split-string str path-separator))
             path-separator))

(defun pyvenv--add-dirs-to-PATH (dirs-to-add)
  "Add DIRS-TO-ADD to different variables related to execution paths."
  (let* ((new-eshell-path-env (pyvenv--prepend-to-pathsep-string dirs-to-add (default-value 'eshell-path-env)))
         (new-path-envvar (pyvenv--prepend-to-pathsep-string dirs-to-add (getenv "PATH"))))
    (setq exec-path (append dirs-to-add exec-path))
    (setq-default eshell-path-env new-eshell-path-env)
    (setenv "PATH" new-path-envvar)))

(defun pyvenv--remove-dirs-from-PATH (dirs-to-remove)
  "Remove DIRS-TO-REMOVE from different variables related to execution paths."
  (let* ((new-eshell-path-env (pyvenv--remove-from-pathsep-string dirs-to-remove (default-value 'eshell-path-env)))
         (new-path-envvar (pyvenv--remove-from-pathsep-string dirs-to-remove (getenv "PATH"))))
    (setq exec-path (pyvenv--remove-many-once dirs-to-remove exec-path))
    (setq-default eshell-path-env new-eshell-path-env)
    (setenv "PATH" new-path-envvar)))

;;; Compatibility

(when (not (fboundp 'file-name-base))
  ;; Emacs 24.3
  (defun file-name-base (&optional filename)
    "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
    (file-name-sans-extension
     (file-name-nondirectory (or filename (buffer-file-name)))))
  )

(when (not (boundp 'mode-line-misc-info))
  (defvar mode-line-misc-info nil
    "Compatibility variable for 24.3+")
  (let ((line mode-line-format))
    (while line
      (when (eq 'which-func-mode
                (car-safe (car-safe (cdr line))))
        (setcdr line (cons 'mode-line-misc-format
                           (cdr line)))
        (setq line (cdr line)))
      (setq line (cdr line)))))

(provide 'pyvenv)
;;; pyvenv.el ends here
