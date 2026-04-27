-- Example config for Swayimg.
-- This file contains the default configuration used by the application.

-- The viewer searches for the config file in the following locations:
-- 1. $XDG_CONFIG_HOME/swayimg/init.lua
-- 2. $HOME/.config/swayimg/init.lua
-- 3. $XDG_CONFIG_DIRS/swayimg/init.lua
-- 4. /etc/xdg/swayimg/init.lua

-- colourscheme
color1 = 0xffdadada
color2 = 0xff282828
color3 = 0xbb000000
color4 = 0xffffd700

-- General config
swayimg.set_mode("viewer")
swayimg.enable_antialiasing(true)
swayimg.enable_decoration(false)
swayimg.enable_overlay(false)
swayimg.set_dnd_button("MouseRight")

-- Image list configuration
swayimg.imagelist.set_order("alpha")
swayimg.imagelist.enable_reverse(false)
swayimg.imagelist.enable_recursive(false)
swayimg.imagelist.enable_adjacent(false)

-- Text overlay configuration
swayimg.text.set_font("monospace")
swayimg.text.set_size(20)
swayimg.text.set_padding(10)
swayimg.text.set_foreground(color1)
swayimg.text.set_background(color3)
swayimg.text.set_shadow(0x0d000000)
swayimg.text.set_timeout(3)
swayimg.text.set_status_timeout(3)
swayimg.text.hide()

-- Image viewer mode
swayimg.viewer.set_default_scale("optimal")
swayimg.viewer.set_default_position("center")
swayimg.viewer.set_drag_button("MouseLeft")
swayimg.viewer.set_window_background(color2)
swayimg.viewer.set_image_chessboard(20, 0xff333333, 0xff4c4c4c)
swayimg.viewer.enable_centering(true)
swayimg.viewer.enable_loop(false)
swayimg.viewer.limit_preload(1)
swayimg.viewer.set_mark_color(color4)
swayimg.viewer.set_text("topleft", {
  "File: {name}",
  "Format: {format}",
  "File size: {sizehr}",
  "File time: {time}",
  "EXIF date: {meta.Exif.Photo.DateTimeOriginal}",
  "EXIF camera: {meta.Exif.Image.Model}"
})
swayimg.viewer.set_text("topright", {
  "Image: {list.index} of {list.total}",
  "Frame: {frame.index} of {frame.total}",
  "Size: {frame.width}x{frame.height}"
})
swayimg.viewer.set_text("bottomleft", {
  "Scale: {scale}"
})

-- helper function to move the window vieport in viewer mode
function move_viewport (direction)
  local wnd = swayimg.get_window_size()
  local pos = swayimg.viewer.get_position()

  x = pos.x
  y = pos.y

  if direction == "left" then
    x = math.floor(pos.x + wnd.width / 10)
  elseif direction == "right" then
    x = math.floor(pos.x - wnd.width / 10)
  elseif direction == "up" then
    y = math.floor(pos.y + wnd.width / 10)
  elseif direction == "down" then
    y = math.floor(pos.y - wnd.width / 10)
  end

  swayimg.viewer.set_abs_position(x, y)
end

-- move the viewport of the image
swayimg.viewer.on_key("Left", function()
  move_viewport("left")
end)

swayimg.viewer.on_key("Down", function()
  move_viewport("down")
end)

swayimg.viewer.on_key("Up", function()
  move_viewport("up")
end)

swayimg.viewer.on_key("Right", function()
  move_viewport("right")
end)

-- bind mouse vertical scroll button with pressed Ctrl to zoom in the image at mouse pointer
-- coordinates
swayimg.viewer.on_mouse("Ctrl-ScrollUp", function()
  local pos = swayimg.get_mouse_pos()
  local scale = swayimg.viewer.get_scale()
  scale = scale + scale / 10
  swayimg.viewer.set_abs_scale(scale, pos.x, pos.y);
end)

-- exit to gallery mode
swayimg.viewer.on_key("Escape", function()
  swayimg.set_mode("gallery")
end)

swayimg.viewer.on_key("q", function()
  swayimg.set_mode("gallery")
end)

-- select previous, next, first, and last images
swayimg.viewer.on_key("p", function()
  swayimg.viewer.switch_image("prev")
end)

swayimg.viewer.on_key("n", function()
  swayimg.viewer.switch_image("next")
end)

swayimg.viewer.on_key("Home", function()
  swayimg.viewer.switch_image("first")
end)

swayimg.viewer.on_key("End", function()
  swayimg.viewer.switch_image("last")
end)

swayimg.viewer.on_key("Comma", function()
  swayimg.viewer.switch_image("first")
end)

swayimg.viewer.on_key("Period", function()
  swayimg.viewer.switch_image("last")
end)

-- set image scale (fit by height, width, and reset)
swayimg.viewer.on_key("h", function()
  swayimg.viewer.set_fix_scale("height")
end)

swayimg.viewer.on_key("w", function()
  swayimg.viewer.set_fix_scale("width")
end)

swayimg.viewer.on_key("r", function()
  swayimg.viewer.reset()
end)

-- rotation and flipping
swayimg.viewer.on_key("Shift-r", function()
  swayimg.viewer.rotate(90)
end)

swayimg.viewer.on_key("Shift-h", function()
  swayimg.viewer.flip_horizontal()
end)

swayimg.viewer.on_key("Shift-v", function()
  swayimg.viewer.flip_vertical()
end)

-- mark currently displayed image
swayimg.viewer.on_key("m", function()
  swayimg.viewer.mark_image()
end)

-- Slide show mode, same config as for viewer mode with the following defaults:
swayimg.slideshow.set_timeout(5)
swayimg.slideshow.set_default_scale("fit")
swayimg.slideshow.set_window_background("auto")
swayimg.slideshow.set_text("topleft", { "{name}" })

-- exit to viewer mode
swayimg.slideshow.on_key("Escape", function()
  swayimg.set_mode("viewer")
end)

swayimg.slideshow.on_key("q", function()
  swayimg.set_mode("viewer")
end)

-- Gallery mode
swayimg.gallery.set_aspect("fill")
swayimg.gallery.set_thumb_size(200)
swayimg.gallery.set_padding_size(5)
swayimg.gallery.set_border_size(5)
swayimg.gallery.set_border_color(color1)
swayimg.gallery.set_selected_scale(1.15)
swayimg.gallery.set_selected_color(0xff00aa99)
swayimg.gallery.set_unselected_color(color2)
swayimg.gallery.set_window_color(color2)
swayimg.gallery.set_mark_color(color4)
swayimg.gallery.limit_cache(100)
swayimg.gallery.enable_preload(false)
swayimg.gallery.enable_pstore(false)
swayimg.gallery.set_text("topleft", {
  "File: {name}"
})
swayimg.gallery.set_text("topright", {
  "{list.index} of {list.total}"
})

-- bind Enter key to open image in viewer
swayimg.gallery.on_key("Return", function()
  swayimg.set_mode("viewer")
end)

-- quit swayimg
swayimg.gallery.on_key("Escape", function()
  swayimg.exit()
end)

swayimg.gallery.on_key("q", function()
  swayimg.exit()
end)

-- image selection
swayimg.gallery.on_key("j", function()
  swayimg.gallery.switch_image("left")
end)

swayimg.gallery.on_key("k", function()
  swayimg.gallery.switch_image("down")
end)

swayimg.gallery.on_key("l", function()
  swayimg.gallery.switch_image("up")
end)

swayimg.gallery.on_key(";", function()
  swayimg.gallery.switch_image("right")
end)

swayimg.gallery.on_key("p", function()
  swayimg.gallery.switch_image("left")
end)

swayimg.gallery.on_key("n", function()
  swayimg.gallery.switch_image("right")
end)

swayimg.gallery.on_key("comma", function()
  swayimg.gallery.switch_image("first")
end)

swayimg.gallery.on_key("period", function()
  swayimg.gallery.switch_image("last")
end)

-- mark currently selected image
swayimg.gallery.on_key("m", function()
  swayimg.gallery.mark_image()
end)

--
-- Other configuration examples
--

-- force set scale mode on window resize (useful for tiling compositors)
-- swayimg.on_window_resize(function()
--   swayimg.viewer.set_fix_scale("optimal")
-- end)

-- bind the Delete key in slide show mode to delete the current file and display a status message
-- swayimg.slideshow.on_key("Delete", function()
--   local image = swayimg.slideshow.get_image()
--   os.remove(image.path)
--   swayimg.text.set_status("File "..image.path.." removed")
-- end)

-- set a custom window title in gallery mode
-- swayimg.gallery.on_image_change(function()
--   local image = swayimg.gallery.get_image()
--   swayimg.set_title("Gallery: " .. image.path)
-- end)

-- print paths to all marked files by pressing Ctrl-p in gallery mode
swayimg.gallery.on_key("Ctrl-p", function()
  local entries = swayimg.imagelist.get()
  for _, entry in ipairs(entries) do
    if entry.mark then
        print(entry.path)
    end
  end
end)
