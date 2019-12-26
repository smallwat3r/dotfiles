#!/usr/bin/env perl
# File  : do-symlink.pl
# Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
# Date  : 23.12.2019
#
# Symlink
#

use Cwd qw(cwd);
my $current_dir = cwd;
my $user = getlogin;
my $root = "/Users/${user}";

# Create directories in advance if they do not exists yet.
@directories = ("${root}/.pip", "${root}/.zsh", "${root}/.tmux/plugins",
    "${root}/.vim/autoload", "${root}/config/alacritty");
foreach $dir (@directories) {
    mkdir -p $dir unless -d $dir;
}

# Source <> Links
my %links = (
    "aliases" => "${root}/.aliases",
    "functions" => "${root}/.functions",
    "vimrc" => "${root}/.vimrc",
    "zshrc" => "${root}/.zshrc",
    "tmux.conf" => "${root}/.tmux.conf",
    "bashrc" => "${root}/.bashrc",
    "chunkwmrc" => "${root}/.chunkwmrc",
    "git/gitconfig" => "${root}/.gitconfig",
    "alacritty.yml" => "${root}/.config/alacritty/alacritty.yml",
    "pip/pip.conf" => "${root}/.pip/pip.conf",
    "zsh/antigen.zsh" => "${root}/.zsh/antigen.zsh",
    "tmux/plugins/tmux-battery" => "${root}/.tmux/plugins/tmux-battery",
    "tmux/plugins/tmux-cpu" => "${root}/.tmux/plugins/tmux-cpu",
    "tmux/plugins/tmux-sensible" => "${root}/.tmux/plugins/tmux-sensible",
    "tmux/plugins/tpm" => "${root}/.tmux/plugins/tpm",
    "vim/autoload/plug.vim" => "${root}/.vim/autoload/plug.vim",
    "bin/sketch" => "/usr/local/bin/sketch",
    "bin/tubestatus" => "/usr/local/bin/tubestatus"
);

# Symlink all.
while (($key, $value) = each (%links)) {
    $value = $links{$key};
    symlink("${current_dir}/${key}", $value);
    if ($key=~ m(^bin\/)) {
        # Make exe if bin script.
        chmod 0755, $value;
    }
    print "[+] ${key} linked to ${value}\n";
}
