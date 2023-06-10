import platform

# Do not load configs set via the UI
config.load_autoconfig(False)

if platform.system() == "Linux":
    c.fonts.default_family = "UW Ttyp0"
    c.editor.command = ["emacs", "{}"]
else:
    c.fonts.default_family = "Triplicate A Code"
    c.editor.command = ["/usr/bin/emacsclient", "-c", "{}"]

c.fonts.default_size = "14pt"

# Download location
c.downloads.location.directory = "~/Downloads"

# Stylesheets
c.content.user_stylesheets = ["~/.config/qutebrowser/css/stylesheet.css"]

# Allow javascript to access clipboard
c.content.javascript.can_access_clipboard = True

# Default encoding
c.content.default_encoding = "utf-8"

# Custom headers
c.content.headers.custom = {}

# Bindings
config.bind("$", "fake-key <End>")
config.bind("0", "fake-key <Home>")

config.bind("I", "fake-key <Home>;; mode-enter insert")
config.bind("A", "fake-key <End>;; mode-enter insert")

config.bind("cw", "fake-key <Alt-End><Backspace>;; mode-enter insert")
config.bind("cc", "fake-key <Home><Shift-End><Delete>;; mode-enter insert")
config.bind("<Shift-c>", "fake-key <Shift-End><Delete>;; mode-enter insert")

config.bind("<Ctrl-j>", "completion-item-focus next", mode="command")
config.bind("<Ctrl-k>", "completion-item-focus prev", mode="command")
