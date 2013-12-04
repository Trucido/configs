# woddfellow2's ~/.xsession (rofldell)
# by woddfellow2 | http://wlair.us.to/

export LC_CTYPE="en_US.UTF-8"

# To speed up window moving and to facilitate eye candy
xcompmgr &

# Start XScreenSaver and change some keys
xscreensaver -no-splash &
setxkbmap -option "caps:escape" -option "compose:ralt" &

# Pointless eye candy (wallpaper)
feh --bg-tile ~/img/wallpaper/grid.xpm &

# Start some nice programs
xclock -geometry -0+0 &
xload -geometry 184x48-0+31 &
xbiff -geometry -184+31 &

# ...and attach tmux:
urxvt -geometry 80x50+0+0 -e tmux attach-session -d &

# Finally, start the WM:
exec cwm
