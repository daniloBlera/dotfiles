# Initial X setup
# Terminal configuration
xrdb ~/.Xresources

# Colourscheme
xrdb -merge ~/.config/xresources-themes/get-us-out-of-this

cd $HOME    # 'cd' to HOME before starting stuff
sxhkd &     # Initialize keyboard event handler
exec bspwm  # Initialize window manager

