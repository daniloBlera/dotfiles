#!/usr/bin/env fish
# Put here your fish exclusive aliases

# Attempts to activate Python virtual environments at the current
# directory (assuming there is a directory called `.pyenv`)
alias py2='source ./.pyenv/py2/bin/activate.fish'
alias py3='source ./.pyenv/py3/bin/activate.fish'
alias mlpy='source ./.pyenv/mlpy/bin/activate.fish'
alias py2h='source ~/.local/share/python/environments/py2/bin/activate.fish'
alias py3h='source ~/.local/share/python/environments/py3/bin/activate.fish'
alias mlh='source ~/.local/share/python/environments/mlh/bin/activate.fish'

# Jump to the current directory upon exit
alias ranger="ranger --choosedir=/tmp/rangerdir; cd (cat /tmp/rangerdir); rm /tmp/rangerdir"

# Source common configurations to bash, zsh and fish
source_if_exists "$HOME/.shared_aliases"

