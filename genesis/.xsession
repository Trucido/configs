# woddfellow2's ~/.xsession (genesis)
# by woddfellow2 | http://wlair.us.to/

export LC_CTYPE="en_US.UTF-8"

xscreensaver -no-splash &
xmodmap ~/.Xmodmap &

urxvtd -q -o -f &

xsetroot -solid Grey20 &

xclock -digital -update 1 -geometry -0+0 -face Fixed &
xload -geometry -0+40 &
xbiff -geometry -0+190 &

exec cwm
