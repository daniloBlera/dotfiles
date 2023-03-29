# A bunch of configuration files

A place to backup my system's configuration.

## About the repository structure

This repo uses a directory tree structure of *packages*, wich are organized as:

```text
root-dir
├── package-1
│   └── dotfiles-tree
├── package-2
│   └── dotfiles-tree
├── ...
└── package-N
    └── dotfiles-tree
```

and each package includes their respective configuration files tree structure.
Note that the package contents match the directory tree structure from your
user's home. As an example, the `tmux` and `neovim` packages contain the
following structures:

```text
tmux
└── .tmux.conf
```

and

```text
neovim
└── .config
    └── nvim
        ├── colourschemes
        │  ├── mountains-on-mars.vim
        │  ├── red-star.vim
        │  └── ruiner.vim
        ├── init.vim
        ├── plug.vim
        └── venv.vim
```

wich means, you could simply copy the contents of a package's root directory
(the `.tmux.conf` file or the `.config` folder) and paste them on your user's
home.

## Manual installation

The easiest way to install the configs is to clone this repository and then,
copy the contents of the required packages directly onto your user's home.

## Installation using GNU Stow

First, note that since `stow` can't overwrite files. If a conflict is found
during the installation process then the whole operation is aborted, hopefully
this will help avoiding unstable configuration situations. With that in mind,
you can go ahead and clone this repository somewhere on your machine

```zsh
git clone https://github.com/daniloBlera/ConfigFiles.git
```

then, install a specific package by running something like

```zsh
stow [--no-folding] -d STOW -t TARGET PACKAGE ...
```

where `STOW` is the path to the root of the cloned repository, `TARGET` is the
root to where the files should be installed (the user's home) and `--no-folding`
is an option if you would want to disable folding (more on the section
[About directory folding](#about-directory-folding)). For example, to install
the `zsh`, `nvim`,  `tmux`, and `xinit` configuration packages you would run

```zsh
stow -d ConfigFiles -t ~/ zsh nvim tmux xinit
```

Assumming no conflicts happened - as stow won't overwrite existing files - the
symlinks should be on their right location on the user's home.

### About directory folding

Stow by default will fold directories, i.e., it will create a single symlink
pointing to the root of the package's files instead of re-creating the same
subdirectory tree structure and populating it with symlinks to the source
files. If you'd want to add other files to the stowed directory without
cluttering the package's source, use the `--no-folding` flag. Using the same
*stow* (`-d`) and *target* (`-t`) paths as the example before:

```zsh
stow --no-folding -d ConfigFiles -t ~/ scripts
```

with the `--no-folding` option set, stow will create a directory tree structure
matching the same structure from the source files and populating it with
symlinks to the individual source files instead of placing a single symlink
pointing to the root of the package's tree.

[stow]: https://www.gnu.org/software/stow/
