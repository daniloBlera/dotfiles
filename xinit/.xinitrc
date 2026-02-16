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

# configure the Xorg environment
if [ -f "${session_setup:=$HOME/.xprofile}" ]; then
    . $session_setup
fi

window_manager="${1:-stumpwm}"
terminal=st

case $window_manager in
    bspwm)
        sxhkd &
        exec bspwm;;
    dwm)
        dwmblocks &
        exec dwm 2> "$XDG_CACHE_HOME/dwm.log";;
    rio)
        exec rio -term "${terminal}" -virtuals 9 2> "$XDG_CACHE_HOME/rio.log";;
    icewm)
        exec icewm-session;;
    qtile)
        exec qtile start;;
    stumpwm)
        exec stumpwm;;
    *)
        exec "${window_manager}";;
esac
