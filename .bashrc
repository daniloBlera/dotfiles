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
# function n() {
    # export NNN_TMPFILE=${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd
    # nnn -otd "$@"
# 
    # if [ -f $NNN_TMPFILE ]; then
        # . $NNN_TMPFILE
        # rm -f $NNN_TMPFILE > /dev/null
    # fi
# }

function n () {
    # Block nesting of nnn in subshells
    if [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, export NNN_TMPFILE after the call to nnn
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef

    nnn -otd "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

# Configuring path, environment variables and aliases
source_if_exists "$HOME/.environment"
source_if_exists "$HOME/.bash_aliases"
source_if_exists "$HOME/.shared_aliases"
