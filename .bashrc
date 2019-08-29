#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# Enable extended completion
if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    source /usr/share/bash-completion/bash_completion
fi

# Utility functions -- handle incompatibilities between shells
function set_env() {
    export $1=$2
}

function source_if_exists() {
    if [[ -f "$1" ]]; then
        source "$1"
    fi
}

function prepend_to_path() {
    if [[ -d "$1" ]] && [[ ":$PATH:" != *":$1:"* ]]; then
        export PATH="$1:$PATH"
    fi
}

function append_to_path() {
    if [[ -d "$1" ]] && [[ *":$PATH:"* != ":$1:" ]]; then
        export PATH="$PATH:$1"
    fi
}

# Jump to directory on exit
function n() {
    export NNN_TMPFILE=${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd
    nnn -d "$@"

    if [ -f $NNN_TMPFILE ]; then
        . $NNN_TMPFILE
        rm -f $NNN_TMPFILE > /dev/null
    fi
}

# Configuring path, environment variables and aliases
source_if_exists "$HOME/.environment"
source_if_exists "$HOME/.bash_aliases"

