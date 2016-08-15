# Window Manager Config

## File Locations
    admiral.d/    -> $POME/.config/admiral.d/
    xmonad        -> $HOME/.xmonad/
    Xresources    -> $HOME/.Xresources
    Xresources.d/ -> $HOME/.Xresources.d/
    xsessionrc    -> $HOME/.xsessionrc
    sources/bar/  -> $HOME/.config/admiral.d/bar/

## Dependencies:
### Required Programs:
    xmonad
    urxvt
    setxkbmap
    dmenu2
    feh
    xbacklight
    arandr
    xdotool
    stalonetray
    nm-applet
    scrot
    Hack-font
    MissingH      # cabal install MissingH
    i3lock        # my version is in sources/
    lemonbar      # my version is in sources/
    admiral       # my version is in sources/

### Required Scripts in $PATH (found in https://www.github.com/SolitaryCipher/cfg/bin/):
    lockscreen.sh      # run the lockscreen command 
    screens.sh         # handle multi monitor setups
    gcal2.sh           # console calender app. Quits on input
    wallpaper.sh       # set the wallpaper (mine uses feh)

### Required Files and Directories:
    $HOME/.xmonad/wallpapers/  # referenced in xmonad/wallpaper.sh
    $HOME/.urxvt/ext.          # for copy/paste perls
    $HOME/.config/base16-env/  # https://github.com/SolitaryCipher/base16-env


