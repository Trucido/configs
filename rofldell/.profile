# woddfellow2's ksh ~/.profile
# by woddfellow2 | http://wlair.us.to/

PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:.
export PATH HOME TERM
export LC_CTYPE="en_US.UTF-8"

export PKG_PATH="ftp://ftp5.usa.openbsd.org/pub/OpenBSD/5.4/packages/amd64/"

export ENV="$HOME/.kshrc"

case $- in *i*)
	fortune
	echo
	w
	;;
esac
