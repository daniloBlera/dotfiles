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

# Not saving command history
HISTFILE="/tmp/$(whoami)_zsh_history"

# USER CONFIGURATIONS
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
source_if_exists "$HOME/.fzf.bash"

# Configuring path, environment variables, aliases and extra functions
source_if_exists "$HOME/.config/shells/environment"
source_if_exists "$HOME/.config/shells/aliases"
source_if_exists "$HOME/.config/shells/bash_aliases"
source_if_exists '/usr/share/fzf/completion.bash'
source_if_exists '/usr/share/fzf/key-bindings.bash'
source_if_exists "$HOME/.local/bin/fuzzyfuncs.sh"

# pyenv stuff
eval "$(pyenv init - --no-rehash)"

# Extra stuff
stty -ixon

# Disable redirect if the destination file exists
set -o noclobber

# Enable broot fuzzy search finder/navigator
source_if_exists "$HOME/.config/broot/launcher/bash/br"
