# Initial X setup
# Terminal configuration
xrdb ~/.Xresources

# Colourscheme
xrdb -merge ~/.config/xresources-themes/we-were-never-friends

cd $HOME    # 'cd' to HOME before starting stuff
unclutter & # Automatically hide mouse cursor when unused
sxhkd &     # Initialize keyboard event handler
exec bspwm  # Initialize window manager

