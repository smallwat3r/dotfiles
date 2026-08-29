# Foxglove Studio (snap)
#
# The snap sandbox only maps wayland-0, but the compositor uses wayland-1,
# so native Wayland segfaults. Force XWayland instead.

has foxglove-studio || return

alias foxglove-studio='foxglove-studio --ozone-platform=x11'
