# theme-rotation.el

TODO:
- instead of always loading themes, check if theme is already loaded, and just enable it?
  - custom-enabled-themes
- when mode is turned off, remove timers, and when mode is turned on again, reapply timers
  - https://github.com/DarwinAwardWinner/emacs-named-timer
- check if it is well defined as a minor mode
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
  - improve comments, capitalize first letter of phrases, add more extensive explanations
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Packages.html
  - https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org
  - https://github.com/bbatsov/emacs-lisp-style-guide
  - use checkdoc in each buffer to point out style errors
  - flymake-mode to catch byte-compilation errors
  - install https://github.com/purcell/package-lint and M-x package-lint-current-buffer to get file ready for MELPA
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html