#!/bin/sh
# Implementing some fzf-based utilities
#
# To use these functions, export this script from your shell's config file.

# Fuzzy-Search directory (or file) and `cd` to it
#
# Usage:
#   fcd [SEARCH_PATH]
#
# Arguments:
#   SEARCH_PATH     The path where the fuzzy search should be applied. If given
#                   the path to a file, `cd` to the directory containing this
#                   file.
#
,fcd() {
    searchdir=''
    if [ "$#" -eq 0 ]
    then
        searchdir="$HOME"
    elif [ -d "$1" ]
    then
        searchdir="$1"
    else
        searchdir="$(dirname "$1")"
    fi

    result="$(find "$searchdir" 2>/dev/null | fzf)"
    if [ -n "$result" ]
    then
        [ -f "$result" ] && result="$(dirname "$result")"
        cd "$result" || return
    fi
}

# Edit one or more files
#
# Fuzzy-Search files and open them with `VISUAL`
#
# Usage:
#   fed [SEARCH_PATH]
#
# Arguments:
#   SEARCH_PATH     The root path where the fuzzy search should be applied. if
#                   the argument is a path to a file, its `dirname` will be
#                   used instead.
#
,fed() {
    searchdir=''
    if [ "$#" -eq 0 ]
    then
        searchdir="$HOME"
    elif [ -d "$1" ]
    then
        searchdir="$1"
    else
        searchdir="$(dirname "$1")"
    fi

    result=$(find "$searchdir" -type f 2>/dev/null | fzf -m | tr '\r\n' ' ')
    if [ -n "$result" ]
    then
        echo "$result" | xargs nvim -p
    fi
}

# Kill one or more processes
#
# Select multiple processes to kill with `Tab` or `Shift+Tab`
#
# Usage:
#   fkill
#
,fkill() {
    search_result="$(ps -e -o pid= -o cmd= | fzf -m)"
    if [ -n "$search_result" ]
    then
        pid="$(echo "$search_result" | sed -e 's/^  *//g' -e 's/   */ /g' | cut -d ' ' -f 1)"
        echo "Killing PID's: $pid"
        echo "$pid" | xargs kill -9
    else
        echo 'No process to kill'
    fi
}

# Remove one or more items from trash
#
# Select multiple items to remove from trash with `Tab` or `Shift+Tab`
#
# Usage:
#   fclean
#
,ftrash() {
    selection="$(trash-list | fzf -m)"

    if [ -n "$selection" ]
    then
        paths="$(echo "$selection" | cut -d ' ' -f 3)"
        echo "$paths" | xargs -L 1 trash-rm
    fi
}

# Select files to view with `git diff`
#
# If inside a git repository, list all files differing from the working tree
# and display the selected ones with `git diff`
#
# Usage:
#   fgd
#
,fgd() {
    if git rev-parse --is-inside-work-tree > /dev/null 2>&1
    then
        selection="$(git status --porcelain | fzf -m | cut -c 4- | tr '\n' ' ')"

        if [ -n "$selection" ]
        then
            git diff "$(echo "$selection" | xargs)"
        fi
    fi
}

# Search and open pdf files
#
# List all pdf files in the search directory (defaults to ~/Documents) and open
# the selected one using xdg-open.
#
# Usage:
#   fpdf [DIRECTORY]
#
# Arguments:
#   DIRECTORY       The path to the root of the search directory
#
,fpdf() {
    searchdir=''
    if [ "$#" -eq 0 ]
    then
        searchdir="$HOME/Documents"
    elif [ -d "$1" ]
    then
        searchdir="$1"
    else
        searchdir="$(dirname "$1")"
    fi

    selection="$(find "$searchdir" -type f -name '*.pdf' 2> /dev/null | fzf)"
    if [ -n "$selection" ]
    then
        zathura "$selection"
    fi
}

# Search all available font names
#
# Usage:
#   ffc
#
,ffc() {
    fc-list : family | cut -d ',' -f 1 | sort | uniq | sed 's/^  *//' | fzf --multi
}
