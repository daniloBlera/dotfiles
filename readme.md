# A bunch of .files

A place to backup my system's configuration. This is the place I keep my personal
configuration files, currently an arch Linux machine. Feel free to adapt anything you
find useful.

## The tl;dr version

* [GNU Stow][stow];
* individual tools have their separated directory tree.

## About the repository structure

For isolation purposes and ease of maintenance, the files in this repository are
organized in terms of *packages*, where each individual tool will have their separated
sub-directory with everything they need. The general structure of this repository's root
is:

```text
repository-root
├── package-1
│   └── dotfiles-tree  # everything package-1 needs
├── package-2
│   └── dotfiles-tree  # everything package-2 needs
├── ...
└── package-N
    └── dotfiles-tree  # everything package-N needs
```

with the idea that each package's sub-directory structure matches the root of the system's
configuration directory tree. For this repository, we're using `$HOME` as the root, which
we'll refer to as *target*. As an example, here are the structures of the `neovim` and
`tmux` packages:

```text
neovim
└── .config
    └── nvim
        ├── init.lua
        └── lua
            ├── config
            │   └── lazy.lua
            └── plugins
                └── plugins.lua
```

and

```text
tmux
└── .config
    └── tmux
        └── tmux.conf
```

this means you could copy and paste the contents of a package's root directly into your
user's `$HOME` (but please, backup your files before doing anything).

## Manual installation

1. clone the repository;
2. copy the contents of a package into your user's home;
3. be happy.

## Installation using GNU Stow

If using `stow` to install configurations, note that [it first checks for conflicts
between the target and destination directories before making any change to the file
system][deferred-op], which will reduce the chances that the target directory will be left
in an inconsistent state. With that in mind, you can go ahead and clone this repository if
you didn't already.

Then, install a specific package by running something with the following structure:

```zsh
stow [--no-folding] -d SOURCE -t TARGET PACKAGE [PACKAGE ...]
```

where `SOURCE` is the path to the root of the cloned repository, `TARGET` is the root to
where the files should be installed (the user's home in the case for this repository) and
`--no-folding` is an option if you would want to disable folding (more on the section
[About directory folding](#target-directory-folding)).

As an example, to install configuration files for `neovim`, `tmux`, and `zsh`, do:

```zsh
stow -d ConfigFiles -t ~/ neovim tmux zsh
```

Assuming that no conflicts happened - as stow won't overwrite existing files - the
symlinks should be on their right location on the user's home.

### About directory folding

By default stow tries to place a single symlink at the installation target. By using
`--no-folding`, stow will re-create the package's directory structure on the installation
target and populate it with symlinks to the files. This is useful to avoid accidentally
putting personal or sensitive data from a tool's configuration root into the repository
(e.g: Emacs hosting downloaded packages and backups inside its dotfile home)

For illustration, here's the result from installing the neovim config package without
`--no-folding`:

```zsh
.config
└── nvim -> ../Programs/ConfigFiles/neovim/.config/nvim
```

with a single symlink at `~/.config`, compared to a tree of symlinks with folding
disabled:

```zsh
.config/nvim
├── init.lua -> ../../Programs/ConfigFiles/neovim/.config/nvim/init.lua
└── lua
    ├── config
    │   └── lazy.lua -> ../../../../Programs/ConfigFiles/neovim/.config/nvim/lua/config/lazy.lua
    └── plugins
        └── plugins.lua -> ../../../../Programs/ConfigFiles/neovim/.config/nvim/lua/plugins/plugins.lua
```

[stow]: https://www.gnu.org/software/stow/
[deferred-op]: https://www.gnu.org/software/stow/manual/stow.html#Deferred-Operation-1
