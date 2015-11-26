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
    * [God Mode](#god-mode)
    * [Boon](#boon)
    * [Fingers](#fingers)
    * [Xah Fly Keys](#xah-fly-keys)
    * [Ergoemacs Mode](#ergoemacs-mode)
  * [Example of use](#example-of-use)
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

In this article I use “vimish” terms when I refer to modes of operation:

* *Normal mode* — this is where you manipulate existing text, normal
  characters typically run primitive editing operations instead of
  self-insertion.

* *Insert mode* — in this mode characters are self-inserting, i.e. it's
  how Emacs works by default.

### What Modalka is not

* Modalka does not introduce new “efficient” keyboard layout for normal mode
  of editing, you set it up yourself.

* Modalka does not reinvent the wheel providing “improved” commands for
  editing, I believe you can find all possible commands to edit text in
  Emacs itself or in numerous third-party packages.

### Why should I use it then?

Modal editing is more efficient, but most importantly it is better for your
health. You should at least try it. This package allows easily switch from
<kbd>⎈ Ctrl</kbd> (and other modifiers) -based key combinations to
performing editing manipulations via normal typing experience and you can do
it gradually using your own design (although it's recommended to keep it
similar to commands you already have in *insert mode*).

`grep`, `man`, `gnus` — all these goodies are recommended to be called using
key sequences and calling of these commands should not necessarily be modal,
you only should make modal “intense” editing commands that you use very
often.

The only thing you need to set up is how you press the key bindings, make it
easier. Here Modalka comes into play. It adds a thin wrapper that translates
some normally self-inserting characters in normal mode into traditional key
bindings that you specify. In insert mode everything works as usual. This
way you can work with all existing packages “natively”, without the need for
any sort of “bridge”.

### Other solutions

In this section I describe other solutions and compare them with this
package. Some of the solutions are quite popular, others are almost not
used. I'll attempt to guess why it is so and why Modalka may be worth
trying.

#### Evil

[Evil](https://gitorious.org/evil) is popular because Vi (and Vim) are
popular. In fact, Vi (and Vim) are more popular than Emacs itself. Emulation
of Vi-style modal editing for Emacs is provided by several various modes,
but most advanced one is Evil.

What's wrong with it? Well, you see, Emacs is very flexible and can be Vim,
of course, with sufficient effort, but Emacs is not Vim. Emacs has different
traditional keybindings and other parts of Emacs ecosystem follow these
conventions (mostly). Then if you are using `evil-mode` to edit text you
will need to either accept that you edit text with different set of key
bindings than key bindings used everywhere else or try to “convert” Emacs
further.

To convert Emacs further you will need sort of bridge package for every
more-or-less complex thing: `evil-org`, `evil-smartparens`, et cetera.

Evil by itself is fairly complex and hooks deep into Emacs internals and can
cause incompatibilities with other packages. It also makes it harder (or at
least *intricate*) to hack Emacs.

Modalka feels vanilla, it lets you use Emacs how it is supposed to be used,
but adds modal interface when you need to edit text, that looks like a more
natural solution (at least for me).

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

#### God Mode

[God Mode](https://github.com/chrisdone/god-mode) is quite popular and can
be considered an improvement over `control-mode`.

However, compared to Modalka, God Mode has certain downsides:

* Design decisions are made for you. You can change something (because it's
  Emacs), but forming entire key map is not meant to be done by user.

* Its implementation is far more hairy without any additional benefit. You
  can do the same things with Modalka if you really want to, and it will
  always be more robust and bug-free simply because the system is clearer
  and more minimalistic.

* God Mode cannot handle situations with input methods are enabled. Modalka
  works with enabled input methods with ease. This may be important for
  users working with non-Latin languages.

* You don't need to write hooks to change shape to cursor according to
  current mode with Modalka, it handles this for you.

#### Boon

[Boon](https://github.com/jyp/boon) is a package for modal editing with
respect to ergonomics. This package gives you complete implementation of
modal editing system similar to Vim.

It may take some time to learn it and I'm not entirely sure it will bring
much difference. Modal editing is easier, but ergonomic layout of normal
mode is somewhat optional for most people. I would value compatibility with
the rest of Emacs ecosystem more.

Of course it is far more complex than Modalka and you are not supposed to
change layout (it's carefully designed for you already). I also should note
that this is for *Colemak* typists.

#### Fingers

[Figners](https://github.com/fgeller/fingers.el) is another attempt on
ergonomic modal editing. The same things said for Boon can be repeated
here. Differences between these packages are not very significant except for
the fact that Fingers is optimized for *Workman* keyboard layout.

#### Xah Fly Keys

[Xah Fly Keys](https://github.com/xahlee/xah-fly-keys) is one more package
for ergonomic modal editing optimized for *Dvorak* (QWERTY layout is said to
be supported too). It's rather big compared to Boon and Fingers. If you look
at source code you'll see that it has peculiar collection of editing
primitives, for example you can capitalize things skipping words like “and”,
“to”, “or”, etc. — functionality you rarely find in this sort of Emacs
package. Good dose of Unicode support is guaranteed too!

#### Ergoemacs Mode

According to authors
[Ergoemacs Mode](https://github.com/ergoemacs/ergoemacs-mode) supports modal
editing and can even emulate `god-mode`. And that's not all:

> You can either define your own modal keymap, or tell `ergoemacs-mode` that
> the keyboard layout is the same as the current layout, but with Alt (or
> control pressed, or swapped, or any sort of other key combination).

It's more complex of course, but goals of Ergoemacs Mode are entirely
different from goals of this package and do not include “lightweightness”.

### Example of use

Let's try to bring modal editing to Emacs using Modalka package. If you want
to follow this example making the changes in your Emacs configuration along
the way you should first install `modalka`. Installation instructions are
given [in the next section](#installation).

Here is simple collection of translations that an Emacs user could easily
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

For now you can use <kbd>M-x modalka-mode</kbd> to try it. When in normal
mode (`modalka-mode`) with such a setup, you can kill two lines of text like
this: <kbd>SPC n n w</kbd>. Note that Modalka can translate sequential key
bindings like <kbd>x ;</kbd> too.

If you're missing numeric prefixes it's easy to add them:

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
* `modalka-remove-key`

Here are versions that wrap arguments with `kbd`:

* `modalka-define-kbd`
* `modalka-remove-kbd`

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

This will enable `modalka-mode` in every buffer except for minibuffer. You
can also avoid enabling `modalka-mode` when buffer is in certain major
mode. To do that add names of major modes to `modalka-excluded-modes` list,
like this:

```emacs-lisp
(add-to-list 'modalka-excluded-modes 'magit-status-mode)
```

However you may want to enable `modalka-mode` only in modes where you need
to edit text:

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
according to editing mode you are in (*normal mode* — `modalka-mode` and
*insert mode* — your normal Emacs editing).

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

`modalka-mode` is a normal minor mode. This means that you can use
`modal-mode-hook` to define mode-specific hooks. You can use customization
interface to customize Modalka-related variables like this: <kbd>M-x
customize-group modalka RET</kbd>.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
