# Initial X setup
# Terminal configuration
xrdb ~/.Xresources

# Colourscheme
xrdb -merge ~/.config/xresources-themes/try-harder-puppy

cd $HOME    # 'cd' to HOME before starting stuff
unclutter & # Automatically hide mouse cursor when unused
exec bspwm  # Initialize window manager

