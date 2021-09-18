# Notes about the Xresources package
The `Xresources` configurations on this package are separated by three files:

*   settings &mdash; fonts and things not related to colours;
*   xresources-term &mdash; terminal and dmenu colours;
*   xresources-dwm &mdash; DWM colours.

Setting these configs is done in the first few lines of the
`~/.local/bin/autostart-config/autostart-config` file, included in the the
`scripts` package.

Also, the `setxtheme` and `setxdwmtheme` files from the `scripts` package can be used to change the terminal and dwm colourschemes.
