# Do not load configs set via the UI
config.load_autoconfig(False)

# Fonts
c.fonts.default_family = "Triplicate A Code"
c.fonts.default_size = "14pt"

# Editor
c.editor.command = ["/usr/bin/emacsclient", "-c", "{}"]

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

# Bindings for vim like text editing
config.bind("$", "fake-key <End>")
config.bind("0", "fake-key <Home>")
config.bind("I", "fake-key <Home>;; mode-enter insert")
config.bind("A", "fake-key <End>;; mode-enter insert")
config.bind("cw", "fake-key <Alt-End><Backspace>;; mode-enter insert")
config.bind("cc", "fake-key <Home><Shift-End><Delete>;; mode-enter insert")
config.bind("<Shift-c>", "fake-key <Shift-End><Delete>;; mode-enter insert")
