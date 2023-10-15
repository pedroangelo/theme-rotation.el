# `theme-rotation.el`
An Emacs minor mode to automatically change themes according to specified time periods.

## Instalation and Usage
Install `theme-rotation.el` by cloning this repository, and then use it with `use-package`:

```lisp
(use-package theme-rotation
    :load-path "path/to/theme-rotation.el"
    :config
    (theme-rotation-mode))
```

Otherwise, you can load the file manually by including this in your init file:
```lisp
(load "path/to/theme-rotation.el/theme-rotation.el")
(theme-rotation-mode)
```

## Customization
The user can customize the **theme rotation**, i.e. which themes are enabled and at which time periods. The recommended way to customize the theme rotation is to set the `theme-rotation-config` variable.

If your using `use-package`, set the variable using `:custom`, after `:load-path` and before `:config`:

```lisp
(use-package theme-rotation
    :load-path "path/to/theme-rotation.el"
    :custom
    (theme-rotation-config
     '(("08:00" . tsdh-light)
       ("20:00" . tsdh-dark))
     "apply dark-mode after 20:00.")
    :config
    (theme-rotation-mode))
```

If you loaded `theme-rotation.el` manually, you can set the variable after `load` and before calling the mode.

```lisp
(load "path/to/theme-rotation/theme-rotation.el")
(setq theme-rotation-config
   '(("08:00" . tsdh-light)
     ("20:00" . tsdh-dark)))
(theme-rotation-mode)
```

`theme-rotation-config` holds an alist between starting times and theme names. Starting times *time1*, *time2*, ..., *timeN* are strings of the format "hh:mm". Theme names *theme1*, *theme2*, ..., *themeN* are theme names (not strings). The format of `theme-rotation-config` is:

```lisp
'((time1 . theme1)
  (time2 . theme2)
  ...
  (timeN . themeN))
```
Note that you can set more than 2 time periods, so it allows more than just the common *dark mode* configuration.

## Similar Packages and Contributions
Several packages and contributions handle themes, usually by allowing to apply dark mode functionality. Here we discuss these, and compare with `theme-rotation.el`package:
- https://parasurv.neocities.org/emacs/change-emacs-theme-depending-on-time
- https://github.com/toroidal-code/cycle-themes.el
- https://github.com/hadronzoo/theme-changer
- https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
