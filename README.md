# theme-rotation.el

TODO:
- remove most of functionality, only timers are required
- disable previous theme before applying new one
  - this can be easily done by 'disable-theme current-theme'
- maybe define it as a minor mode?
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Minor-Mode-Conventions.html
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Modes.html
  - https://systemcrafters.net/learning-emacs-lisp/creating-minor-modes/
- add install and usage:
  - clone from github
  - with use package or simply by loading file
- similar / related packages: 
  - https://parasurv.neocities.org/emacs/change-emacs-theme-depending-on-time
  - https://github.com/toroidal-code/cycle-themes.el
  - https://github.com/hadronzoo/theme-changer
  - https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
- prepare for melpa publication:
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Packages.html
  - https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org
  - https://github.com/bbatsov/emacs-lisp-style-guide
  - use checkdoc in each buffer to point out style errors
  - flymake-mode to catch byte-compilation errors
  - install https://github.com/purcell/package-lint and M-x package-lint-current-buffer to get file ready for MELPA
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html