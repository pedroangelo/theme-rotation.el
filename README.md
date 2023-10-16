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

If you're using `use-package`, set the variable using `:custom`, after `:load-path` and before `:config`:

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

`theme-rotation-config` holds an list of pairs composed of starting times and theme names, in a similar way to an alist. Starting times *time1*, *time2*, ..., *timeN* are strings of the format "hh:mm". Theme names *theme1*, *theme2*, ..., *themeN* are theme names (not strings). The format of `theme-rotation-config` is:

```lisp
'((time1 . theme1)
  (time2 . theme2)
  ...
  (timeN . themeN))
```
Note that you can set more than 2 time periods, so it allows more than just the common *dark mode* configuration.

## Similar Packages and Contributions
Several packages and contributions handle themes, usually by enabling some sort of dark mode functionality. Here we discuss some of these, and compare with `theme-rotation.el` package.

[`cycle-themes.el`](https://github.com/toroidal-code/cycle-themes.el) is an emacs minor mode that allows the user to cycle between a predefined list of themes. The difference between this package and the `cycle-themes.el`package, is that changing themes is automatic according to the time of day. 

[`theme-changer`](https://github.com/hadronzoo/theme-changer) allows a user to set a location, and according to sunrise and sunset times, changes between predefined light and dark themes. The difference between this package and the `theme-changer` package is that the user may configure custom time intervals, without being bound to sunrise / sunset times.

I've found some other contributions that offer similar functionality as this package:
- In [here](https://parasurv.neocities.org/emacs/change-emacs-theme-depending-on-time), a simple script for changing themes by calling run-at-time timers is described. However, to avoid undesirable behaviour, the previous theme should be disabled before the next one is applied. This is not discussed in the above contribution. 
- In [here](https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time), a simple script that changes themes according to two time periods is described. However, it is not discussed how to allow more than two time periods. Furthermore, it doesn't allow to define periods that do not start at an exact hour. 
