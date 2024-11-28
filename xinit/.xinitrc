#!/bin/sh
# Initial X setup
#
# to start the default window manager (stumpwm in this case), run
#
#   startx
#
# alternatively, to specify a different option
#
#   startx ~/.xinitrc <window-manager>
#
# Example, to initialize `rio`:
#
#   startx ~/.xinitrc rio

# configure the Xorg-related environment
~/.local/scripts/,autostart-config

window_manager="${1:-stumpwm}"
terminal=st

case $window_manager in
    bspwm)
        sxhkd &
        exec bspwm;;
    dwm)
        dwmblocks &
        exec dwm 2> "$XDG_CACHE_HOME/dwm.log";;
    twm)
        exec twm;;
    9wm)
        exec 9wm -term "${terminal}" -active white -inactive grey 2> "$XDG_CACHE_HOME/9wm.log";;
    rio)
        exec rio -term "${terminal}" -virtuals 9 2> "$XDG_CACHE_HOME/rio.log";;
    openbox)
        exec openbox;;
    *)
        exec "${window_manager}";;
esac
