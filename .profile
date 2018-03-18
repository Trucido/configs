#!/bin/sh
# ~/.profile
# Time-stamp: <2018-03-17 22:17:35 PDT xoddf2>

# Set PATH
PATH="/usr/local/games:/usr/games:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin"

if [ -d "$HOME/local/bin" ]; then
	PATH="$HOME/local/bin:$PATH"
elif [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi

export PATH

# Set locale
export LANG="en_US.UTF-8"

# Set MANPATH
[ -d "$HOME/local/share/man" ] && export MANPATH="$HOME/local/share/man:$MANPATH"

# Set width of manual pages
export MANWIDTH=80

# Sort dotfiles first
export LC_COLLATE="C"

# Set time zone at salt.hax0r.se
[ "$HOSTNAME" = "salt" ] && export TZ="UTC"

# Reduce dotfile clutter
if [ "$HOSTNAME" = "nomad" ]; then
	export XDG_CONFIG_HOME="$HOME/local/etc"
	export XDG_DATA_HOME="$HOME/local/share"
	export XDG_CACHE_HOME="$HOME/local/var/cache"
	export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
	export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
	export LESSHISTFILE="$XDG_CACHE_HOME/lesshst"
	export GIMP2_DIRECTORY="$XDG_CONFIG_HOME/gimp"
	export XCOMPOSEFILE="$XDG_CONFIG_HOME/xorg/XCompose"
	export TERMINFO_DIRS="$XDG_DATA_HOME/terminfo"
	export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
fi

# Display a fortune
if command -v fortune >/dev/null 2>&1 && [[ $(tty) != /dev/tty1 ]] && [[ $- == *i* ]]; then
	fortune
	echo
fi

# Start X from tty1
if [ "$HOSTNAME" = "nomad" ] && [[ $(tty) == /dev/tty1 ]]; then
	exec startx
fi

# Under bash(1), source ~/.bashrc if it exists
if [[ $SHELL =~ bash ]] && [ -f "$HOME/.bashrc" ]; then
	. $HOME/.bashrc
fi
