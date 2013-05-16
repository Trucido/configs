# woddfellow2's ~/.xsession (genesis)
# by woddfellow2 | http://wlair.us.to/

xscreensaver -no-splash &
xmodmap ~/.Xmodmap &

xsetroot -solid rgb:11/11/11 &

xclock -digital -update 1 -geometry -0+0 -face Fixed -bg black -fg green &
xload -geometry -0+40 -bg black -fg green -hl blue &

exec cwm
