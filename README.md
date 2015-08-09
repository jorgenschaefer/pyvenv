# pyvenv.el, Python virtual environment support for Emacs

![Travis-CI Build Status](https://secure.travis-ci.org/jorgenschaefer/pyvenv.png)
[![MELPA Stable](http://stable.melpa.org/packages/pyvenv-badge.svg)](http://stable.melpa.org/#/pyvenv)

This is a simple global minor mode which will replicate the changes
done by virtualenv activation inside Emacs.

The main entry points are `pyvenv-activate`, which queries the user
for a virtual environment directory to activate, and `pyvenv-workon`,
which queries for a virtual environment in `$WORKON_HOME` (from
virtualenvwrapper.sh).

## Similar Projects

[virtualenv.el](https://github.com/aculich/virtualenv.el) is the
original virtualenv implementation for Emacs. I used it for a long
time, but didn’t like some of the design decisions.

For example, it does not modify `process-environment` so does not set
a virtual environment for `M-x compile` and other external processes.
Also, `M-x virtualenv-workon` requires a prefix argument to actually
change the current virtual environment. And it does not support
virtualenvwrapper’s hooks, which I use to set up a working
environment.

All in all, too much magic for too little gain. So I figured I’d write
my own. Still, it’s an excellent package and I’m very grateful to have
used it for a long time.
