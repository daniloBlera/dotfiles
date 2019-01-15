#!/usr/bin/fish

# Utility functions
function set_env
    # Wrapping export into function for bash/zsh/fish interfacing
    set -xg $argv[1] $argv[2]
end

function source_if_exists
    # Source file if it exists
    if [ -e "$argv[1]" ]
        source "$argv[1]"
    end
end

# Configuring initial settings
set -xg EDITOR nvim

# Configuring path, environment variables and aliases
source_if_exists "$HOME/.environment"
source_if_exists "$HOME/.config/fish/aliases.fish"

# Suppress greetings
set fish_greeting

