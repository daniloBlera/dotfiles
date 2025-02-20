# Configuring how the prompt should look
#   '[<user>@<host> <cwd-last-segment>]<prompt> '
PROMPT='%n@%m:%1~%# '

# show the level of shell nesting from nnn
[ -n "$NNNLVL" ] && PROMPT="N$NNNLVL:$PROMPT"

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

# use emacs mode keybindings
bindkey -e

# Check if file exists before trying to source it
source_if_exists() {
    [ -f "$1" ] && source "$1"
}

# If the directory exists and is not included in PATH, return TRUE
not_in_path() {
    [[ -d "$1" ]] && [[ ":$PATH:" != *":$1:"* ]]
}

prepend_to_path() {
    not_in_path "$1" && export PATH="$1:$PATH"
}

append_to_path() {
    not_in_path "$1" && export PATH="$PATH:$1"
}

# Configuring path, environment variables, aliases and extra functions
source_if_exists "$HOME/.config/shells/environment"
source_if_exists "$HOME/.config/shells/aliases"
source_if_exists "$HOME/.config/shells/zsh_aliases"
source_if_exists "$HOME/.config/shells/zsh_keybindings"
source_if_exists '/usr/share/fzf/completion.zsh'
source_if_exists '/usr/share/fzf/key-bindings.zsh'
source_if_exists "$HOME/.local/scripts/fuzzyfuncs.sh"
source_if_exists "$HOME/.config/nnn/misc/quitcd.bash_sh_zsh"

# pyenv stuff
eval "$(pyenv init - --no-rehash)"
source /usr/share/zsh/site-functions/_pyenv

# direnv hook
eval "$(direnv hook zsh)"

# setup zoxide
eval "$(zoxide init zsh)"

# Extra stuff
HISTFILE="${XDG_STATE_HOME:-~/.local}/zsh_history"
HISTSIZE=1000
SAVEHIST=1000
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt autocd

stty -ixon

# Disable overwriting files with redirection
set -o noclobber
