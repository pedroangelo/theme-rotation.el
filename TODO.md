# theme-rotation.el TODO notes

TODO:
- write README as a .org file?
- change :type of theme-rotation-config to alist?
- enable to define default theme, which just disables the previous theme
- instead of always loading themes, check if theme is already loaded, and just enable it?
  - custom-enabled-themes
- check if it is well defined as a minor mode
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Minor-Mode-Conventions.html
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Modes.html
  - https://systemcrafters.net/learning-emacs-lisp/creating-minor-modes/
- prepare for melpa publication:
  - improve comments, capitalize first letter of phrases, add more extensive explanations
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Packages.html
  - https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org
  - https://github.com/bbatsov/emacs-lisp-style-guide
  - use checkdoc in each buffer to point out style errors
  - flymake-mode to catch byte-compilation errors
  - install https://github.com/purcell/package-lint and M-x package-lint-current-buffer to get file ready for MELPA
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html
