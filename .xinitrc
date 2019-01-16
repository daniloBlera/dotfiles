# Initial X setup
# Terminal configuration
xrdb ~/.Xresources

# Colourscheme
xrdb -merge ~/.config/xresources-themes/we-were-never-friends

cd $HOME    # 'cd' to HOME before starting stuff
sxhkd &     # Initialize keyboard event handler
exec bspwm  # Initialize window manager

