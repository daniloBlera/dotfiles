#
# ~/.zshrc
#

# NEW LINES
# Lines configured by zsh-newuser-install
# HISTFILE=~/.histfile
HISTFILE=/tmp/$(whoami)-zsh_histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd extendedglob nomatch
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle ':completion:*' menu select
zstyle :compinstall filename '/home/dcb/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# NEW LINES

# OLD LINES
# zstyle ':completion:*' menu select
# zstyle :compinstall filename "$HOME/.zshrc"
# 
# autoload -Uz compinit
# compinit

# Reset history on reboots or shutdowns. To prevent saving any history file,
# unset HISTFILE and set SAVEHIST to 0, commenting the history lines below.
# HISTFILE=/tmp/zsh_history-$(whoami)
# HISTSIZE=1000
# SAVEHIST=1000

bindkey -e
# OLD LINES

PROMPT='[%n@%m %1~]%# '

# Utility functions used to handle incompatibilities between shells (bash, zsh and fish)
set_env() {
    export $1=$2
}

source_if_exists() {
    [ -f "$1" ] && source "$1"
}

# If the directory exists and is not included in PATH, return TRUE
not_in_path() {
    [[ -d "$1" ]] && [[ ":$PATH:" != *":$1:"* ]] && return 0 || return 1
}

prepend_to_path() {
    not_in_path "$1" && export PATH="$1:$PATH"
}

append_to_path() {
    not_in_path "$1" && export PATH="$PATH:$1"
}

# Enable file managers cd on quit
source_if_exists "$HOME/.config/nnn/misc/quitcd.sh"
source_if_exists "$HOME/.config/lf/lfcd.sh"

# Enabling fuzzyfind
source_if_exists "$HOME/.fzf.zsh"

# Configuring path, environment variables, aliases and extra functions
source_if_exists "$HOME/.config/shells/environment"
source_if_exists "$HOME/.config/shells/aliases"
source_if_exists "$HOME/.config/shells/zsh_aliases"
source_if_exists "$HOME/.config/shells/zsh_keybindings"
source_if_exists '/usr/share/fzf/completion.zsh'
source_if_exists '/usr/share/fzf/key-bindings.zsh'
source_if_exists "$HOME/.local/scripts/fuzzyfuncs.sh"

# pyenv stuff
eval "$(pyenv init - --no-rehash)"
source /usr/share/zsh/site-functions/_pyenv

# Extra stuff
stty -ixon
