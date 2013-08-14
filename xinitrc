#!/bin/bash

echo "--- starting xinitrc ---"

# Load resources

echo "merging .Xresources"
#xrdb -merge .Xresources
echo "merging .Xdefaults"
xrdb -merge .Xdefaults

# set up dual monitor
#xrandr --output DVI-I-1 --auto --primary --left-of DVI-I-2 --auto
#xrandr --output DVI-0 --auto --primary --right-of DVI-1 --auto

# Load systray
echo "loading systray"
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5  --transparent true --tint 0x000000 --alpha 0 --heighttype pixel --height 17 --monitor primary &

# Set the background color 
echo "setting background"
xsetroot -solid black

# Set wallpaper
echo "setting wallpaper"
eval `cat ~/.fehbg`

# turn off the anoying beep
echo "turning bell off"
xset -b 

# start composit manager
echo "starting xcompmgr"
xcompmgr -c -o 0 &

# start conky
conky -a top_left -x 1460 -y 425 &

# Fire up apps
echo "starting apps"

#echo "starting synergys"
#synergys 2> /var/log/synergy/synergys.err 1> /var/log/synergy/synergys.log

echo "...iceweasel"
iceweasel &

echo "...emacs"
ec 2> /var/log/ec/ec.err 1> /var/log/ec/ec.log &

echo "...psi"
psi 2> /var/log/psi/psi.err 1> /var/log/psi/psi.log &

echo "...torrent"
start_torrent &

echo "...dropbox"
~/.dropbox-dist/dropboxd &


# runs as cron now
#echo "starting offlineimap"
#offlineimap -l -u Quiet  > /var/log/offlineimap/offlineimap.log 2> /var/log/offlineimap/offlineimap.err  &

echo "--- finished xinitrc ---"

# Start xmonad
echo "starting xmonad"
exec xmonad

