#!/usr/bin/env fish

# Add to path if not included already
function append_to_path
    if [ -d $argv[1] ]; and not contains $argv[1] $PATH
        set -x PATH $PATH $argv[1]
    end
end
