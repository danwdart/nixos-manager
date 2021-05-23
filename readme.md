nixos-manager --- manage your NixOS graphically
===============================================

[![](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[<https://travis-ci.com/pmiddend/nixos-manager.svg?branch=master>](https://travis-ci.com/pmiddend/nixos-manager.svg?branch=master)

Rationale
---------

The idea is to build something to **configure NixOS graphically**. To
me, this means: managing the installed software packages, and managing
the enabled services, as well as updating and garbage-collecting. For
the moment, I'm focusing on a solution for a declarative, global
configuration; so *for the moment* it's not calling `nix-env` and it's
not supporting `home-manager`.

The goal is **not** to make all of NixOS accessible via a GUI, and not
give an alternative to writing Nix expressions for your configuration.
Instead, this is a project to explore how far you can get graphically
after installing NixOS (which --- in the future --- might be possible
via a GUI installer as well).

Approach
--------

Since the NixOS configuration is very "free-form", I have to make some
assumptions about the structure of your `configuration.nix` file, as
such:

-   You have two additional `.nix` files, referenced from the `imports`
    section in your `configuration.nix`: `packages.nix` and
    `services.nix`
-   `packages.nix` is a flat list of packages
-   `services.nix` is a list of service options, like
    `services.xserver.enable = true`.

In nixos-manager, you can mark packages for installation. They will be
added to your *local* copy of `packages.nix`. You can then trigger a
nixos-rebuild from inside nixos-manager, which copies your local
installation to the *global installation* inside
`/etc/nixos/nixos-manager/packages.nix`. Same for configuring services
and `services.nix`.

To gather service definitions, nixos-manager downloads and parses
<https://nixos.org/nixos/options.json>. This way, the service
definitions might not accurately reflect your system configuration, but
I just found no other way to gather the definitions, see [this Discourse
thread](https://discourse.nixos.org/t/list-available-services-and-their-options/).

Screenshots
-----------

### Welcome screen, featuring a frontend for `nixos-rebuild`

![](./screenshots/admin.png)

### Welcome screen for `home-manager` users

![](./screenshots/home-manager.png)

### Search and install (or try) packages

![](./screenshots/packages.png)

### Configure services

![](./screenshots/services.png)

Screencast
----------

[Watch on
YouTube](https://www.youtube.com/watch?v=Fa4_9ueOdpY&feature=youtu.be)

Please recommend to me another platform where I can upload this,
preferably lossless as the source mkv that I have.

FAQ
---

### Have you seen [hnix](https://github.com/haskell-nix/hnix)? Why don't you parse your you Nix files with

I'd love to! But sadly and ironically, it's currently
[broken](https://github.com/NixOS/nixpkgs/issues/82233) in nixpkgs. I'm
waiting for someone to repair it, as I don't have the knowledge to do
that.

I'm using Haskell as the language of choice, simply because `hnix` is
available and allows me to easily parse and serialize the relevant Nix
configuration files. The GUI is made via `gi-gtk-declarative`, which
serves me well and is pretty stable.

### Can I configure NixOS using this tool and manually at the same time?

Yes, that's possible. The files that nixos-manager uses shouldn't be
edited by hand, but you can configure services and packages the regular
way as well, by editing `configuration.nix`. Thus, nixos-manager is just
an enhancement.

### The list of packages only lists packages installed by nixos-manager, not globally installed ones. How come?

The simple reason is: I haven't gotten to that part yet, and my Nix
knowledge is so limited I'd have to ask how do that first.

Building
--------

nixos-manager isn't in nixpkgs yet, so you'll have to build it
using...Nix!

``` {.bash}
nix-build -I nixpkgs=channel:nixos-unstable
```

The channel argument is necessary in case you're not on
`nixos-unstable`.

Setup and running
-----------------

### With home-manager

After building it (see above), you have to tell home-manager to read
nixos-manager's configuration files. To do that, open your
`~/.config/nixpkgs/home.nix` file and insert the following somewhere
inside the main attribute set:

``` {.example}
imports = [ ./hm-extra-services.nix ./hm-extra-packages.nix ];

manual.json.enable = true;
```

nixos-manager, when run in home-manager mode, will write these extra
files. The second line enables JSON export of the available options in
home-manager, which nixos-manager reads to generate the configuration
UI.

To run nixos-manager in home-manager mode, simply run it with the
`--home-manager` argument. For example, if you built it with Nix, run:

``` {.example}
result/bin/nixos-manager --home-manager
```

If you've built it with `cabal` (for example, to try it out after
changing the code), run:

``` {.example}
cabal v2-run nixos-manager -- --home-manager
```

### With NixOS

After building it (see above), you have to tell NixOS to read
nixos-manager's configuration files. To do that, open your
`/etc/nixos/configuration.nix` file and look for a line containing
`imports = …`. Usually it looks something like this:

``` {.nix}
{
  # …
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  # …
}
```

So `imports` contains an array of Nix files. You can extend this array
by just listing more files, separated by spaces. To use NixOS manager,
extend the list as such:

``` {.nix}
{
  # …
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./nixos-manager/services.nix
      ./nixos-manager/packages.nix
    ];
  # …
}
```

nixos-manager will create these two files when you apply changes.

Current status
--------------

-   The frontend for `nixos-rebuild` works. It supports `--upgrade` and
    `--rollback`.
-   `nix-collect-garbage` is also supported, supporting the `-d` flag.
-   Rebuilding copies `packages.nix` and `services.nix` to
    `/etc/nixos/nixos-manager`. Without these files being included in
    your `configuration.nix`, they're worthless, though.
-   Configuring services works, with some minor quirks.
-   Trying packages works, and installing/uninstalling too. The
    `packages.nix` will be updated accordingly.

Contributing guide
------------------

### Prerequisites

-   nixos-manager is written in [the Haskell
    Language](https://www.haskell.org/). You should be proficient with
    this language. I can't give a full recommendation list for learning
    it (can anyone reading this?). It's *really* worth it though. Some
    books are
    -   [Real World Haskell](http://book.realworldhaskell.org/) (free)
    -   [Learn You a Haskell for Great
        Good!](http://learnyouahaskell.com/) (free)
    -   [Programming in Haskell: Hutton, Graham
        (Amazon-Link)](https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229/ref=sr_1_1?dchild=1&keywords=haskell&qid=1585907775&sr=8-1)
    -   [Haskell Programming from first
        principles](https://haskellbook.com/)
-   I'm using [the GTK toolkit](https://www.gtk.org/) and specifically
    [gi-gtk-declarative](https://owickstrom.github.io/gi-gtk-declarative/)
    as a declarative approach to writing GTK applications. The
    documentation is
    [here](https://owickstrom.github.io/gi-gtk-declarative/widgets/the-widget-type/).
    A good blog post by the author is [Declarative GTK+ Programming with
    Haskell](https://wickstrom.tech/programming/2018/09/04/declarative-gtk-programming-with-haskell.html).

### Code structure

Each module has haddock documentation. Finding information about the
code just from that might be difficult though, so let me give you a
short overview:

-   Each of the tabs you see (notebook pages, in GTK-Speak) has a
    submodule. There's `NixManager.Admin`, `NixManager.Services` and
    `NixManager.Packages`, respectively.
-   Inside each such submodule you have modules `Event`, `Update` and
    `View`.
    -   `Event` contains the even data type for the submodule (which is
        then embedded in `NixManager.ManagerEvent`)
    -   `Update` contains the update handler (a function receiving the
        current state and an event and returns a new state, as well as
        an event and IO side-effects)
    -   `View` contains the `gi-gtk-declarative` classes for the
        notebook page.
    -   `State` contains the state corresponding to the subsystem
-   When starting a process, such as the rebuild process, we cannot
    easily run it in the background and emit an event when it's finished
    (at least I don't know how to do that comfortably with
    `gi-gtk-declarative-app-simple`). Instead, we are spawning the
    process (in the background, mind you), then wait for a small amount
    of time (which doesn't block the GUI, fortunately) and then emit a
    "watch" event, which does the same thing, until the process signals
    completion. It sounds hacky, and it is, but it's not that bad.

Alternatives
------------

-   [nix42b](https://gitlab.com/juliendehos/nix42b) --- is another
    user-friendly Nix package manager, also written in Haskell. It
    doesn't seem to be actively maintained anymore.

Donations
---------

[PayPal.Me](https://paypal.me/PhilippMiddendorf)
