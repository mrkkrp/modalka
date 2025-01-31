# Modalka

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/modalka-badge.svg)](https://melpa.org/#/modalka)
[![CI](https://github.com/mrkkrp/modalka/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/modalka/actions/workflows/ci.yaml)

* [What is this?](#what-is-this)
  * [What Modalka is not](#what-modalka-is-not)
  * [Why should I use it then?](#why-should-i-use-it-then)
* [Installation](#installation)
* [Example of use](#example-of-use)
* [Usage](#usage)
  * [How to define translations](#how-to-define-translations)
  * [How to activate the minor mode](#how-to-activate-the-minor-mode)
  * [Change the cursor shape for visual feedback](#change-cursor-shape-for-visual-feedback)
* [Customization](#customization)
* [Other solutions](#other-solutions)
  * [Evil](#evil)
  * [Control Mode](#control-mode)
  * [God Mode](#god-mode)
  * [Boon](#boon)
  * [Fingers](#fingers)
  * [Xah Fly Keys](#xah-fly-keys)
  * [Ergoemacs Mode](#ergoemacs-mode)
* [License](#license)

## What is this?

This is a building kit to help switch to modal editing in Emacs.

In this article I use Vi-inspired terms when I refer to the modes of
operation:

* In the *normal mode* you manipulate existing text and keystrokes typically
  result in editing operations instead of insertion of characters.

* In the *insert mode* keystrokes insert the corresponding characters, i.e.
  it's how Emacs works by default.

### What Modalka is not

* Modalka does not introduce a new keyboard layout for the normal mode, you
  set it up yourself.

* Modalka does not provide new commands for editing.

### Why should I use it then?

Modal editing is more efficient, but most importantly, it is better for
health. This package allows its users to switch from tiring `Ctrl`-based key
combinations to editing through normal typing. The transition can be gradual
and the user can define their own layout.

## Installation

The package is available via MELPA, so you can just type `M-x
package-install RET modalka RET`.

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`. Then you can require it in your init file like
this:

```emacs-lisp
(require 'modalka)
```

## Example of use

Here is a simple collection of translations that an Emacs user could easily
adopt:

```emacs-lisp
(modalka-define-kbd "W" "M-w")
(modalka-define-kbd "Y" "M-y")
(modalka-define-kbd "a" "C-a")
(modalka-define-kbd "b" "C-b")
(modalka-define-kbd "e" "C-e")
(modalka-define-kbd "f" "C-f")
(modalka-define-kbd "g" "C-g")
(modalka-define-kbd "n" "C-n")
(modalka-define-kbd "p" "C-p")
(modalka-define-kbd "w" "C-w")
(modalka-define-kbd "y" "C-y")
(modalka-define-kbd "SPC" "C-SPC")
```

One can type `M-x modalka-mode` to try it. When in the normal mode
(`modalka-mode`) with such a setup, two lines of text can be killed like
this: `SPC n n w`. Note that Modalka can translate sequential key bindings
such as `x ;`, too.

Numeric prefixes can also be added:

```emacs-lisp
(modalka-define-kbd "2" "C-2")
```

Now twenty-two lines can be killed with `SPC 2 2 n w`.

For an example of a complete setup see [this][my-setup].

## Usage

Modalka is implemented as a minor mode called `modalka-mode`. This section
describes how to set up efficient modal editing and provides some tips.

### How to define translations

There is a set of functions to define key translations and to remove them:

* `modalka-define-key`
* `modalka-remove-key`

Here are the versions that wrap their arguments with `kbd`:

* `modalka-define-kbd`
* `modalka-remove-kbd`

Using these functions it's easy to setup a translation map.

Note that the target key binding cannot be a prefix key:

```emacs-lisp
(modalka-define-kbd "x" "C-x") ;; will not work
```

Translations involving prefix keys will be ignored.

If you want to bind a command in `modalka-mode` without performing a
keybinding translation, remember that `modalka-mode` is just a normal minor
mode which has an associated key map called `modalka-mode-map`. So you can
do the following:

```emacs-lisp
(define-key modalka-mode-map (kbd "Q") #'my-command)
```

Using this approach it is possible to remap a prefix key like this:

```emacs-lisp
(define-key modalka-mode-map "x" ctl-x-map)
(define-key ctl-x-map (kbd "e") #'eval-last-sexp)
(define-key ctl-x-map (kbd "s") #'save-buffer)
```

### How to activate the minor mode

One should bind a key to toggle `modalka-mode`. This should be an easy key—
one keystroke, easy to reach. I would even advise binding `;` or `Enter`:

```emacs-lisp
(global-set-key (kbd "<return>") #'modalka-mode)
```

The next thing to decide is whether `modalka-mode` should be enabled by
default. `modalka-mode` can be enabled everywhere (except for the
minibuffer) by activating `modalka-global-mode`:

```emacs-lisp
(modalka-global-mode 1)
```

It is also possible to give Modalka a list of major modes where it should
not be enabled:

```emacs-lisp
(add-to-list 'modalka-excluded-modes 'magit-status-mode)
```

However, one may choose to enable `modalka-mode` only in conjunction with
certain major modes:

```emacs-lisp
(add-hook 'text-mode-hook #'modalka-mode)
(add-hook 'prog-mode-hook #'modalka-mode)
```

This is a whitelisting approach. `modalka-global-mode` and
`modalka-excluded-modes` implement a blacklisting approach.

### Change the cursor shape for visual feedback

`modalka-mode` comes with a lighter—`↑`. I don't recommend disabling it
because it is nice to have an indication of whether `modalka-mode` is active
or not. Furthermore, one can make the current editing mode even more
conspicuous by changing the cursor shape. I suggest using the vertical bar
cursor in the insert mode and the box cursor in the normal mode. Modalka
uses the cursor specified in the `modalka-cursor-type` variable, so the
whole setup might look like this:

```emacs-lisp
(setq-default cursor-type '(bar . 1))
(setq modalka-cursor-type 'box)
```

## Customization

`modalka-mode` is a normal minor mode. This means that you can use the
`modalka-mode-hook` to define mode-specific hooks. You can use the
customization interface to customize Modalka-related variables like this:
`M-x customize-group modalka RET`.

## Other solutions

In this section I describe other solutions and compare them with this
package. Some of the solutions are quite popular, others are almost not
used. I attempt to guess why it is so and why Modalka may be worth trying
out.

### Evil

Emulation of the Vi-style modal editing for Emacs is provided by several
different packages, but the most advanced is [Evil][evil]. What's wrong with
it? Emacs is very flexible and can be Vi, with some effort, but Emacs is not
Vi. Emacs has different keybindings for movement, etc. that permeate its
whole ecosystem. Once `evil-mode` is used to edit text, one needs to either
accept that text editing is done with a set of key bindings that differs
from everything else, or to try to convert Emacs further. To convert Emacs
further one needs more bridge packages: `evil-org`, `evil-smartparens`, etc.
The sort of conversion can never be fully complete. Evil by itself is fairly
complex and hooks deep into Emacs internals. It can cause incompatibilities
with other packages. It also makes it harder to hack Emacs.

### Control Mode

[Control Mode][control-mode] is essentially a hack. From my experience it
has the following flaws:

* If one works with overlays that have local key maps this mode cannot
  handle it. One needs to disable it to interact with the overlays (packages
  that implement text folding are an example of that).

* If a minor mode is activated when `control-mode` is already enabled, it
  cannot catch this change and adapt. One needs to turn `control-mode` off
  and then reactivate it or to run a special command that re-generates the
  key bindings.

* Generalizing the previous points, in Emacs, a given combination of keys
  may have different meanings depending on the situation. The automatic
  generation of key bindings that `control-mode` uses fixes keybindings
  every time and thus causes all sorts of problems.

* Control mode generates more key bindings than necessary replacing key
  bindings that should not be used in the normal mode.

### God Mode

[God Mode][god-mode] can be considered an improvement on `control-mode`.
However, compared to Modalka, God Mode has certain downsides:

* Design decisions are made for you. You can change something (because it's
  Emacs), but forming the entire key map is not meant to be done by the
  user.

* The implementation is far more hairy without additional benefits.

* Unlike Modalka, God Mode doesn't work with input methods.

* You don't need to write hooks to change the shape of cursor according to
  current mode with Modalka, it handles this for you.

### Boon

[Boon][boon] is a package for modal editing with emphasis on ergonomics.
This package gives you complete implementation of a modal editing system
similar to Vi. It may take some time to learn it and I'm not entirely sure
it will make much difference. Modal editing is easier, but ergonomic layout
in the normal mode is somewhat optional for most people. I value
compatibility with the Emacs ecosystem more.

### Fingers

[Fingers][fingers] is another attempt at ergonomic modal editing. The same
thoughts that have been said regarding Boon can be repeated here. The
differences between these packages are not very significant except for the
fact that Fingers is optimized for the *Workman* keyboard layout.

### Xah Fly Keys

[Xah Fly Keys][xah-fly-keys] is a package for ergonomic modal editing
optimized for *Dvorak*. It's rather big compared to Boon and Fingers. If you
look at source code you'll see that it has a peculiar collection of editing
primitives. For example one can capitalize text skipping words like “and”,
“to”, “or”, etc.—functionality that is rarely found in this sort of a
package. Good dose of Unicode support is guaranteed, too.

### Ergoemacs Mode

According to the authors, [Ergoemacs Mode][ergoemacs-mode] supports modal
editing and can even emulate `god-mode`. And that's not all:

> You can either define your own modal keymap, or tell `ergoemacs-mode` that
> the keyboard layout is the same as the current layout, but with Alt (or
> control pressed, or swapped, or any sort of other key combination).

## License

Copyright © 2015–present Mark Karpov

Distributed under GNU GPL, version 3.

[my-setup]: https://github.com/mrkkrp/nixos-config/blob/b8b20c8868c8902fb342d7a5ba638dd610b6019c/imports/emacs/mk-packages.el#L627-L750
[evil]: https://gitorious.org/evil
[control-mode]: https://github.com/stephendavidmarsh/control-mode
[god-mode]: https://github.com/chrisdone/god-mode
[boon]: https://github.com/jyp/boon
[fingers]: https://github.com/fgeller/fingers.el
[xah-fly-keys]: https://github.com/xahlee/xah-fly-keys
[ergoemacs-mode]: https://github.com/ergoemacs/ergoemacs-mode
