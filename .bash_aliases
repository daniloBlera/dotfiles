# Put here your bash exclusive aliases

# Jump to the current directory upon exit
alias ranger='ranger --choosedir=/tmp/rangerdir; cd $(cat /tmp/rangerdir); rm /tmp/rangerdir'

# Source common configurations to bash, zsh and fish
source_if_exists "$HOME/.shared_aliases"

