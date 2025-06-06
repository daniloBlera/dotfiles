# zsh configuration file

# configuring how the prompt should look

# 1 - the common approach
# '[user@host:last_dir] % '
PROMPT='[%n@%m:%1~]%# '

# 2 - user prompt on the line below
# [user@host:~/sub/dir]
# %
#PROMPT=$'[%n@%m:%~]\n%# '

# 3 - going simple with just the prompt, rc-style
# '% '
#PROMPT='%# '

# show the level of shell nesting from nnn
[ -n "$NNNLVL" ] && PROMPT="N$NNNLVL:$PROMPT"

# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' menu select=0
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall

# use emacs mode keybindings
bindkey -e

# Check if file exists before trying to source it
source_if_exists() {
    [ -f "$1" ] && source "$1"
}

# Check if argument is in PATH
not_in_path() {
    [[ ":$PATH:" != *":$1:"* ]]
}

# Insert path to the beginning of PATH
prepend_to_path() {
    not_in_path "$1" && export PATH="$1:$PATH"
}

# Insert path to the end of PATH
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

# setup commands if they're installed somewhere in PATH
if [ -x "$(command -v $PYENV_ROOT/bin/pyenv)" ]; then
    eval "$($PYENV_ROOT/bin/pyenv init - --no-rehash)"
fi

if [ -x "$(command -v direnv)" ]; then
    eval "$(direnv hook zsh)"
fi

if [ -x "$(command -v zoxide)" ]; then
    eval "$(zoxide init zsh)"
fi

# Extra stuff
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh_history"
HISTSIZE=500
SAVEHIST=500
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt autocd

stty -ixon

# Disable overwriting files with redirection
set -o noclobber
