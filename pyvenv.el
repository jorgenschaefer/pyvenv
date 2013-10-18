;;; pyvenv.el --- Python virtual environment interface

;; Copyright (C) 2013  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: http://github.com/jorgenschaefer/pyvenv
;; Version: 1.0

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

;;; Code:

;; API for other libraries or user customization.

(defvar pyvenv-virtual-env nil
  "The current virtual environment.

Do not set this variable directly; use `pyvenv-activate' or
`pyvenv-workon'.")

(defvar pyvenv-virtual-env-name nil
  "The name of the current virtual environment.

This is usually the base name of `pyvenv-virtual-env'.")

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

(defvar pyvenv-workon nil
  "A variable requesting a specific virtualenv.

This is meant to be set in file- or directory-local variables.

When `pyvenv-mode' is enabled, pyvenv will switch to this
virtualenv. If a virtualenv is already enabled, it will ask first.")

(defcustom pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                        ("[" pyvenv-virtual-env-name "] "))
  "How `pyvenv-mode' will indicate the current environment in the mode line.")

;; Internal code.

(defvar pyvenv-old-process-environment nil
  "The old process environment before the last activate.")

(defvar pyvenv-old-exec-path nil
  "The old exec path before the last activate.")

(defun pyvenv-activate (directory)
  "Activate the virtual environment in DIRECTORY."
  (interactive "DActivate venv: ")
  (setq directory (expand-file-name directory))
  (pyvenv-deactivate)
  (setq pyvenv-virtual-env directory
        pyvenv-virtual-env-name (file-name-base directory))
  (run-hooks 'pyvenv-pre-activate-hooks)
  (setq pyvenv-old-process-environment process-environment
        pyvenv-old-exec-path exec-path
        process-environment (append
                             (list
                              (format "VIRTUAL_ENV=%s" directory)
                              (format "PATH=%s/bin:%s"
                                      directory
                                      (getenv "PATH"))
                              ;; No "=" means to unset
                              "PYTHONHOME")
                             process-environment)
        exec-path (cons (format "%s/bin" directory)
                        exec-path))
  (run-hooks 'pyvenv-post-activate-hooks))

(defun pyvenv-deactivate ()
  "Deactivate any current virtual environment."
  (interactive)
  (run-hooks 'pyvenv-pre-deactivate-hooks)
  (when pyvenv-old-process-environment
    (setq process-environment pyvenv-old-process-environment
          pyvenv-old-process-environment nil))
  (when pyvenv-old-exec-path
    (setq exec-path pyvenv-old-exec-path
          pyvenv-old-exec-path nil))
  (run-hooks 'pyvenv-post-deactivate-hooks)
  (setq pyvenv-virtual-env nil
        pyvenv-virtual-env-name nil))

(defvar pyvenv-workon-history nil
  "Prompt history for `pyvenv-workon'.")

(defun pyvenv-workon (name)
  "Activate a virtual environment from $WORKON_HOME."
  (interactive
   (list
    (completing-read "Work on: " (pyvenv-virtualenv-list)
                     nil t nil 'pyvenv-workon-history nil nil)))
  (when (not (equal name ""))
    (pyvenv-activate (format "%s/%s"
                             (or (getenv "WORKON_HOME")
                                 "~/.virtualenvs")
                             name))))

(defun pyvenv-virtualenv-list ()
  "Prompt the user for a name in $WORKON_HOME."
  (let ((workon-home (or (getenv "WORKON_HOME")
                         "~/.virtualenvs"))
        (result nil))
    (when (not (file-directory-p workon-home))
      (error "Can't find a workon home directory, set $WORKON_HOME"))
    (dolist (name (directory-files workon-home))
      (when (file-exists-p (format "%s/%s/bin/activate"
                                   workon-home name))
        (setq result (cons name result))))
    (sort result (lambda (a b)
                   (string-lessp (downcase a)
                                 (downcase b))))))

(define-minor-mode pyvenv-mode
  "Global minor mode for pyvenv.

Will show the current virtual env in the mode line, and respect a
`pyvenv-workon' setting in files."
  :global t
  (cond
   (pyvenv-mode
    (add-to-list 'mode-line-misc-info pyvenv-mode-line-indicator)
    (add-hook 'python-mode-hook 'pyvenv-set-file-virtualenv))
   ((not pyvenv-mode)
    (setq mode-line-misc-info (delete pyvenv-mode-line-indicator
                                      mode-line-misc-info))
    (remove-hook 'python-mode-hook 'pyvenv-set-file-virtualenv))))

(defun pyvenv-set-file-virtualenv ()
  "If `pyvenv-workon' is set, switch to that virtual env."
  (cond
   ((and pyvenv-workon (not pyvenv-virtual-env))
    (virtualenv-workon pyvenv-workon))
   ((and pyvenv-workon (not (equal pyvenv-workon pyvenv-virtual-env)))
    (when (y-or-n-p (format "Switch to virtual env %s (currently %s)? "
                            pyvenv-workon pyvenv-virtual-env))
      (virtualenv-workon pyvenv-workon)))))

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'pyvenv)
;;; pyvenv.el ends here
