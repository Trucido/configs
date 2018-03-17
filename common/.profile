#!/bin/sh
# ~/.profile
# Time-stamp: <2018-03-17 02:58:35 PDT xoddf2>

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
if [ -d "$HOME/local/share/man" ]; then
	export MANPATH="$HOME/local/share/man:$MANPATH"
fi

# Set width of manual pages
export MANWIDTH=80

# Sort dotfiles first
export LC_COLLATE="C"

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
