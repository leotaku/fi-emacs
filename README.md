# fi-emacs

`fi-emacs` is a collection of several packages that aim to provide the user with a holistic way of managing their GNU Emacs configuration.

## Components

+ `fi` :: helper functions missing from Emacs
+ `sd` :: systemd-inspired startup for Emacs
+ `bk` :: use-package replacement using sd

## Documentation

### `sd.el` (unit system)

This package provides an unit system partly inspired by systemd.

The unit system provides the following unique features:

+ Reliable error backtraces
+ Simulated asynchronous loading

Please consult the individual elisp docstrings for documentation.

### `sd-display.el` (startup display)

This package provides a visual display of all defined sd-units.
It can be accessed by executing the `sd-display-tabulated` command.

### `bk.el` (configuration management)

This package provides an use-package replacement based on `sd.el`.

Please consult the individual elisp docstrings for documentation.

### Meta

Currently `fi-emacs` is in constant flux and pretty much only used by me, making good documentation both hard and a relatively low priority.
Please consult the elisp docstrings for documentation or come back later.

## License

This Emacs package is distributed under the terms of the [GPL-3.0-or-later](LICENSE) license, meaning the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Copyright notices are included with all individual files of source code.
