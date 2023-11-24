# Configuring how the prompt should look
#   '[<user>@<host> <lastdir>]<prompt> '
PROMPT='[%n@%m %1~]%# '

# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' menu select=0
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/dcb/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE="/tmp/$(whoami)_zsh_history"
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -e
# End of lines configured by zsh-newuser-install

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

# Disable redirect if the destination file exists
set -o noclobber

# Enable broot fuzzy search finder/navigator
source_if_exists "$HOME/.config/broot/launcher/bash/br"

# Reload zsh's config
alias srccfg="echo 'sourcing ~/.zshrc...'; source ~/.zshrc; echo 'done!'"

# Enable editing a command with $EDITOR -- shortcut: <c-x>, <c-e>
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line
