#!/bin/sh
# Enable keyboard auto repeat for specific keys
# note: you can use `xev -event keyboard` to find individual keycodes

# disable autorepeat for keycodes 8-255
seq 8 255 | xargs -n 1 xset -r

# re-enable it for the following
# xset r 22   # backspace
xset r 111  # up arrow
xset r 113  # left arrow
xset r 114  # right arrow
xset r 116  # down arrow
# xset r 119  # delete
