zstyle ':completion:*' menu select
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit

HISTFILE=/tmp/zsh_history-$(whoami)
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

PROMPT='[%n@%m %1~]%# '

# Utility functions used to handle incompatibilities between shells (bash, zsh and fish)
function set_env() {
    export $1=$2
}

function source_if_exists() {
    [[ -f "$1" ]] && source "$1"
}

# If the directory exists and is not included in PATH, return TRUE
function not_in_path() {
    [[ -d "$1" ]] && [[ ":$PATH:" != *":$1:"* ]] && return 0 || return 1
}

function prepend_to_path() {
    not_in_path "$1" && export PATH="$1:$PATH"
}

function append_to_path() {
    not_in_path "$1" && export PATH="$PATH:$1"
}

# Enable 'cd' on quit
function n () {
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

	nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

# Configuring path, environment variables and aliases
source_if_exists "$HOME/.config/shells/environment"
source_if_exists "$HOME/.config/shells/zsh_aliases"
source_if_exists "$HOME/.config/shells/aliases"

# Extra stuff
stty -ixon

## Using 'broot' for the awesome filesystem navigation and fuzzy search
source $HOME/.config/broot/launcher/bash/br
