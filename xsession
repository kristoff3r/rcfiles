# The following packages need to be installed (probably others as well):
# xmonad
# dzen2
# acpi
# alsa-utils
# pavucontroller
# pidgin
# mplayer
# wpa_supplicant
# x11-xserver-utils (for xmodmap)
# xbindkeys (for shortcuts)

# Add ~/bin and /sbin to PATH
export PATH="$HOME/bin:/sbin:$PATH"

xsetroot -cursor_name left_ptr
xsetroot -solid black

# Turns off audio
soundcontroller init

# Fixes screen
tvconnect

# Enables the ctrl+alt+backspace shortcut to kill X
setxkbmap -option terminate:ctrl_alt_bksp

# Remaps some of the keys
xmodmap ~/.xmodmap

# Starts the wireless script
sudo wireless.sh &

# Starts the dropbox script
dropbox start &

# Fuck off bell noise
xset b off

# Saves DBUS
set | grep DBUS > ~/.xmonad/tempenv

# Starts xmonad
xmonad

# Make sure that dzen doesn't keep running after logout
killall dzenheader dzen2
