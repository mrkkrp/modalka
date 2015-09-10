# Modalka

*Work in progress.*

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/modalka.svg?branch=master)](https://travis-ci.org/mrkkrp/modalka)

* [What is this?](#what-is-this)
  * [What Modalka is not](#what-modalka-is-not)
  * [Why should I use it then?](#why-should-i-use-it-then)
  * [Other solutions](#other-solutions)
    * [Evil](#evil)
    * [Control Mode](#control-mode)
    * [Fingers and Xah Fly Keys](#fingers-and-xah-fly-keys)
  * [Example](#example)
* [Installation](#installation)
* [Usage](#usage)
* [Customization](#customization)
* [License](#license)

## What is this?

This is a building kit to help switch to modal editing in Emacs. Main goal
of the package is making modal editing in Emacs as natural and native as
possible. There is no hack, no corner cases, no emulation — just start edit
modally the way you want.

### What Modalka is not

* Modalka does not introduce new “efficient” keyboard layout for normal mode
  of editing, you set it up yourself.

* Modalka does not reinvent the wheel providing “improved” commands for
  editing, I believe you can find all possible commands to edit text in
  Emacs itself or in numerous third-party packages.

### Why should I use it then?

Modal editing is more efficient and is better for your health. You should at
least try it. This package allows easily switch from <kbd>⎈ Ctrl</kbd> (and
other modifiers as well) based keys to modal editing and you can do it
gradually and use your own design (although it's recommended to keep it
similar to existing basic Emacs commands).

I propose you set up the most frequently used key bindings “Emacs-way”. Most
packages also follow conventions of Emacs world, don't fight it, setup
everything conventionally.

This applies to actual editing itself, `grep`, `man`, `gnus` — all this is
recommended to be called using key sequences and calling of these commands
should not be modal, you only should make modal “intense” editing commands
that you use very often.

When you have your key bindings set up traditionally, you're OK. The only
thing you need to change now is how you press the key bindings, make it
easier.

Here Modalka comes into play. It adds a thin wrapper that translates some
normally self-inserting characters in normal mode into traditional key
bindings that you specify. In insert mode everything works as usual. This
way you can work with all existing packages “natively”, without the need for
any sort of “bridge”.

### Other solutions

In this section I describe other solutions and compare them with this
package. Some of the solutions are quite popular, others are almost not
used. I'll attempt to guess why it is so and why Modalka may be worth
trying.

#### Evil

[`evil`](https://gitorious.org/evil) is popular because Vi (and Vim) are
popular. Vi (and Vim) are more popular than Emacs itself. Emulation of
Vi-style modal editing for Emacs is provided by several various modes, but
most advanced one is `evil`.

What's wrong with it? Well, you see, Emacs is very flexible and can be Vim,
of course, with sufficient effort, but Emacs is not Vim. Emacs has different
traditional keybindings and other parts of Emacs ecosystem follow these
conventions (mostly). Then if you are using `evil` to edit text you will
need to either accept that you edit text with different set of key bindings
than key bindings used everywhere else or try to “convert” Emacs further.

To convert Emacs further you will need sort of bridge package per every
more-or-less complex thing: `evil-org`, `evil-smartparens`, et cetera.

Modalka helps you use Emacs how it is supposed to be used, but adds modal
interface when you need to edit text, that looks like a more natural
solution.

#### Control Mode

[Control Mode](https://github.com/stephendavidmarsh/control-mode) is
essentially a hack. From my experience it has the following flaws:

* Automatic generation of key-bindings on the fly is always a bit dirty.

* If you work with overlays that has local key maps this mode cannot handle
  this, you will need to disable it to interact with the overlays (for
  example if you are using packages to fold text).

* If you enable a minor mode when `control-mode` is already enabled, it
  cannot catch this change and adapt, you need to turn it off then
  reactivate or run a special command that re-generates key bindings.

* Generalizing the previous points, in Emacs, given combination of keys may
  have different meaning depending on situation. Control Mode automatic
  generation of key bindings puts them into stone.

* It generates more key bindings than necessary replacing key bindings that
  should not be used in “normal” mode. As I said, *you* should control your
  key bindings, don't let an algorithm generate them automatically.

Due to these flaws this cannot be considered as mature solution, indeed it
has hardly 200 downloads on MELPA being at least 2 years old.

#### Fingers and Xah Fly Keys

These are not better then `evil` and they both are very personal. The
authors have created packages for modal editing as they see it. Both
packages don't have even 200 downloads and it's easy to explain. These modes
are different from both Emacs and Vim, they are something completely
unknown. You can learn them, of course, but is it really worth it?

### Example of use

Let's try to bring modal editing to Emacs that feels natural and is easy to
learn. If you want to follow this example making the changes in your Emacs
configuration along the way you should first install `modalka`. Installation
instructions are given [in the next section](#installation).

*Coming soon…*

## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'modalka)
```

## Usage

*Coming soon…*

## Customization

*Coming soon…*

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
