#!/usr/bin/env fish

# Add to path if not included already
function prepend_to_path
    if [ -d $argv[1] ]; and not contains $argv[1] $PATH
        set -x PATH $argv[1] $PATH
    end
end
