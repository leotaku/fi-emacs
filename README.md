# fi-emacs
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fleotaku%2Ffi-emacs.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fleotaku%2Ffi-emacs?ref=badge_shield)


`fi-emacs` is a collection of several packages that aim to provide the user with a holistic way of managing their GNU Emacs configuration.

## Components

+ `fi` :: small helper functions, grouped by purpose
+ `sd` :: systemd unit inspired startup for Emacs
+ `bk` :: use-package replacement using sd

## Documentation

### `sd.el` (unit system)

This package provides a low-level unit system, which is inspired partly by systemd.

The unit system provides the following unique features:

+ Reliable error backtraces
+ Simulated asynchronous loading

#### sd-register-unit `(name &optional form requires wanted-by overridep)`

#### sd-reach-target `(name)`

#### sd-poll-target `(name delay &optional notify callback)`

### `sd-display.el` (startup display)

This package provides a visual display of all defined `sd-unit` instances.
One could think of it as the equivalent to systemd's `systemctl status`.

#### sd-display-tabulated `()` `(interactive)`

### `bk.el` (configuration management)

This package provides an use-package replacement, based on `sd.el`.

#### bk-block{0, !, \*, !\*} `(name &rest args)`

#### bk-reach-target `(name)`

#### bk-poll-target `(name &optional after)`

#### bk-expansion-alist

#### bk-generate-expansions `()`

### Meta

Currently `fi-emacs` is in constant flux, making good documentation pretty much impossible.
As such, this README may already be outdated.
Please read the docstrings or come back later.

## License

This Emacs package is distributed under the terms of the [GPL-3.0-or-later](LICENSE) license, meaning the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Copyright notices are included with all individual files of source code.


[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fleotaku%2Ffi-emacs.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fleotaku%2Ffi-emacs?ref=badge_large)