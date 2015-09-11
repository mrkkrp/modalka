# Modalka

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/modalka-badge.svg)](http://melpa.org/#/modalka)
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
  * [How to define translations](#how-to-define-translations)
  * [How to activate the minor mode](#how-to-activate-the-minor-mode)
  * [Change cursor shape for visual feedback](#change-cursor-shape-for-visual-feedback)
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
everything more or less conventionally.

This applies to actual editing, `grep`, `man`, `gnus` — all these goodies
are recommended to be called using key sequences and calling of these
commands should not necessarily be modal, you only should make modal
“intense” editing commands that you use very often.

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
popular. In fact, Vi (and Vim) are more popular than Emacs itself. Emulation
of Vi-style modal editing for Emacs is provided by several various modes,
but most advanced one is `evil`.

What's wrong with it? Well, you see, Emacs is very flexible and can be Vim,
of course, with sufficient effort, but Emacs is not Vim. Emacs has different
traditional keybindings and other parts of Emacs ecosystem follow these
conventions (mostly). Then if you are using `evil` to edit text you will
need to either accept that you edit text with different set of key bindings
than key bindings used everywhere else or try to “convert” Emacs further.

To convert Emacs further you will need sort of bridge package for every
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

Here is simple collection of translations that Emacs user could easily adopt:

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

For now you can use <kbd>M-x modalka-mode</kbd> to try it. When in normal
mode (`modalka-mode`) with such a setup, you can kill two lines of text like
this: <kbd>SPC n n w</kbd>. If you're missing numeric prefixes it's easy to
add them:

```emacs-lisp
(modalka-define-kbd "2" "C-2")
```

Now you can kill twenty-two lines <kbd>SPC 2 2 n w</kbd>. You get the idea,
everything depends on your imagination now!

*Hint: some useful tips are described in [Usage](#usage) section.*

*For example of complete, “real word” setup see
[this](https://github.com/mrkkrp/dot-emacs#modal-editing).*

## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'modalka)
```

It's available via MELPA, so you can just <kbd>M-x package-install RET
modalka RET</kbd>.

## Usage

Modalka implemented as a minor mode called `modalka-mode`. This section
describes how to set up efficient modal editing and provides some tips.

### How to define translations

There is a set of functions to define key translations and to remove them:

* `modalka-define-key`
* `modalka-define-keys`
* `modalka-remove-key`
* `modalka-remove-keys`

Here are versions that wrap arguments with `kbd`:

* `modalka-define-kbd`
* `modalka-define-kbds`
* `modalka-remove-kbd`
* `modalka-remove-kbds`

Using these functions it's easy to setup your translation map. Note that
target key binding cannot be prefix key (prefix keys will be ignored).

### How to activate the minor mode

You should bind some key to toggle `modalka-mode`. This should be an easy
key: one key pressing, easy to reach. I would even advise binding
<kbd>;</kbd> or <kbd>↵ Enter</kbd>, but it's up to you. Bind it globally,
like this:

```emacs-lisp
(global-set-key (kbd "<return>") #'modalka-mode)
```

The next thing to consider is whether `modalka-mode` should be enabled by
default and where. There is no default setup, the whole thing is up to
you. If you want to enable it everywhere, just add the following to your
configuration file:

```emacs-lisp
(modalka-global-mode 1)
```

However it may be better to enable `modalka-mode` only in modes where you
need to edit text:

```emacs-lisp
(add-hook 'text-mode-hook #'modalka-mode)
(add-hook 'prog-mode-hook #'modalka-mode)
```

You can omit all of these if you prefer always start in insert mode.

### Change cursor shape for visual feedback

`modalka-mode` comes with a lighter, currently it's in from of up arrow
“↑”. I don't recommend disabling it because you need some visual feedback to
know if you are in `modalka-mode` or not.

However you can improve visual feedback by using different shapes of cursor
according to editing mode you are in (normal mode — `modalka-mode` and
insert mode — your normal Emacs editing).

You can specify what your cursor looks like by setting `cursor-type`
value. I suggest using vertical bar cursor in insert mode and box cursor in
normal mode. Modalka uses cursor specified in `modalka-cursor-type`
variable, so the whole setup might look like this:

```emacs-lisp
(setq-default cursor-type '(bar . 1))
(setq modalka-cursor-type 'box)
```

And that's it! Now it's obvious what mode you're in.

## Customization

`modalka-mode` is normal minor mode. This means you can use
`modal-mode-hook` to define your hooks. You can use customization interface
to customize Modalka-related variables like this: <kbd>M-x
customize-interface modalka RET</kbd>.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
