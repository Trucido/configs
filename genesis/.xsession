# woddfellow2's ~/.xsession (genesis)
# by woddfellow2 | http://wlair.us.to/

export LC_CTYPE="en_US.UTF-8"

xscreensaver -no-splash &
xmodmap ~/.Xmodmap &

urxvtd -q -o -f &

xsetroot -solid black &

exec xmonad
