#!/usr/bin/fish

# Utility functions -- handle incompatibilities between shells
function set_env
    set -xg $argv[1] $argv[2]
end

function source_if_exists
    if [ -e "$argv[1]" ]
        source "$argv[1]"
    end
end

function append_to_path
    if [ -d $argv[1] ] && not contains $argv[1] $PATH
        set -x PATH $PATH $argv[1]
    end
end

function prepend_to_path
    if [ -d $argv[1] ] && not contains $argv[1] $PATH
        set -x PATH $argv[1] $PATH
    end
end

# Configuring path, environment variables and aliases
source_if_exists "$HOME/.environment"
source_if_exists "$HOME/.config/fish/aliases.fish"
source_if_exists "$HOME/.shared_aliases"

# Suppress greetings
set fish_greeting
