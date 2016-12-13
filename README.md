# Window Manager Config

## File Locations
    xmonad        -> $HOME/.xmonad/
    Xresources    -> $HOME/.Xresources
    Xresources.d/ -> $HOME/.Xresources.d/
    xsessionrc    -> $HOME/.xsessionrc
    taffybar/     -> $HOME/.config/taffybar/

## Dependencies:
### Required Programs:
    xmonad
    urxvt
    setxkbmap
    rofi
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
    taffybar      # maybe works in stack?

### Required Scripts in $PATH (found in https://www.github.com/SolitaryCipher/cfg/bin/):
    lockscreen.sh      # run the lockscreen command 
    screens.sh         # handle multi monitor setups
    wallpaper.sh       # set the wallpaper (mine uses feh)

### Required Files and Directories:
    $HOME/.xmonad/wallpapers/  # referenced in xmonad/wallpaper.sh
    $HOME/.urxvt/ext.          # for copy/paste perls
    $HOME/.config/base16-env/  # https://github.com/SolitaryCipher/base16-env


