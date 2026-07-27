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

##################################################
# copied from the default `/etc/X11/xinit/xinitrc`
#   https://wiki.archlinux.org/title/Xinit#xinitrc
xinitdir="/etc/X11/xinit"

# start some nice programs

if [ -d "$xinitdir"/xinitrc.d ] ; then
    for f in "$xinitdir/xinitrc.d"/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi
##################################################

# configure the Xorg environment
if [ -f "${session_setup:=$HOME/.xprofile}" ]; then
    . $session_setup
fi

window_manager="${1:-stumpwm}"
terminal=st

case $window_manager in
    bspwm)
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
