# Initial X setup
# Terminal configuration and colourscheme
xrdb ~/.Xresources
xrdb -merge ~/.config/xresources-themes/gjm

cd $HOME
unclutter &     # Automatically hide mouse cursor when unused
urxvtd -q -o -f # Initialize the urxvt daemon to save some memory
exec bspwm

