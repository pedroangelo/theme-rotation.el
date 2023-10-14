# `theme-rotation.el`
An Emacs minor mode to automatically change themes according to a specified times of day. 

## Instalation and Usage
Install by cloning this repository somewhere.

Then include with `use-package`:

    (use-package theme-rotation
        :load-path "/path/to/folder"
        :config 
        (theme-rotation-mode))

Otherwise, include this in your init file:

    (load "/path/to/folder")
    (theme-rotation-mode)

## Customization


## Similar Packages and Contributions
- https://parasurv.neocities.org/emacs/change-emacs-theme-depending-on-time
- https://github.com/toroidal-code/cycle-themes.el
- https://github.com/hadronzoo/theme-changer
- https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time