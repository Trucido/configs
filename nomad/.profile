# ~/.profile
# Time-stamp: <2017-09-06 01:00:52 PDT xoddf2>

# Set PATH
PATH="/usr/local/games:/usr/games:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin"

if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi

export PATH

# Set locale
export LANG="en_US.UTF-8"

# Set width of manual pages
export MANWIDTH=80

# Start X from tty1
if [ "$TTY" = "/dev/tty1" ]; then
	exec startx
fi
