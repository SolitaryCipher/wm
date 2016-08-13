# Window Manager Config

## File Locations
    admiral.d/    -> $POME/.config/admiral.d/
    xmoand        -> $HOME/.xmonad/
    Xresources    -> $HOME/.Xresources
    Xresources.d/ -> $HOME/.Xresources.d/
    xsessionrc    -> $HOME/.xsessionrc
    sources/bar/  -> $HOME/.config/admiral.d/bar/

## Dependencies:
### Required Programs:
    dmenu2
    xbacklight
    urxvt
    scrot
    i3lock
    arandr
    feh
    xmonad
    MissingH (from cabal)
    xdotool
    stalonetray
    nm-applet
    setxkbmap
    Hack-font

### Required Scripts in $PATH (found in SolitaryCipher/cfg):
    lockscreen.sh      # in /usr/local/bin atm - not linked.
    screens.sh         # hardcoded path
    gcal2.sh           # hardcoded path

### Required Files and Directories:
    $HOME/.xmonad/wallpapers/    # referenced in xmonad/wallpaper.sh
    $HOME/.local/bin/gcal2.sh    # for the clickable bar item
    $HOME/.local/bin/screensh.sh # probably could solve with editing $PATH
    $HOME/.urxvt/ext             # for copy/paste perls
    $HOME/.config/base16-env     # https://github.com/SolitaryCipher/base16-env


