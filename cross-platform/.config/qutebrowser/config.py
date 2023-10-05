import platform

# Do not load configs set via the UI
config.load_autoconfig(False)

if platform.system() == "Linux":
    c.fonts.default_family = "UW Ttyp0"
    c.editor.command = ["emacs", "{}"]
    c.zoom.default = "90%"
else:
    c.fonts.default_family = "Triplicate A Code"
    c.editor.command = ["/usr/bin/emacsclient", "-c", "{}"]
    c.content.user_stylesheets = ["~/.config/qutebrowser/css/mac.css"]

c.fonts.default_size = "14pt"
c.downloads.location.directory = "~/Downloads"
c.tabs.position = "bottom"

c.content.default_encoding = "utf-8"
c.content.headers.custom = {}
c.content.autoplay = False

c.completion.height = 300  # 30%
c.completion.delay = 0

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
