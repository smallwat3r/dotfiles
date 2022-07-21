config.load_autoconfig(False)

# Fonts
c.fonts.default_family = "Monaco"
c.fonts.default_size = "13pt"

# Editor
c.editor.command = ["/usr/local/bin/emacsclient", "-c", "{}"]

# Download location
c.downloads.location.directory = "~/Downloads"

# Stylesheets
c.content.user_stylesheets = ["~/.qutebrowser/css/stylesheet.css"]

# Allow javascript to access clipboard
c.content.javascript.can_access_clipboard = True
