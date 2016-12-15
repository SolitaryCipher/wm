# Window Manager Config

## File Locations
    Xresources    -> $HOME/.Xresources
    Xresources.d/ -> $HOME/.Xresources.d/
    xsessionrc    -> $HOME/.xsessionrc

## Dependencies:
### Required Programs:
    stack         # for xmonad/
    urxvt256c
    setxkbmap
    rofi
    feh
    xbacklight
    arandr
    xdotool
    nm-applet
    scrot
    Fira Mono font
    i3lock        # my version is in sources/

### Required Scripts in $PATH (found in https://www.github.com/SolitaryCipher/cfg/bin/):
    lockscreen.sh      # run the lockscreen command 
    screens.sh         # handle multi monitor setups
    start_rofi.sh      # rofi start script
    autostart          # autostart script (calls wallpaper.sh)
    wallpaper.sh       # set the wallpaper (mine uses feh)

### Required Files and Directories:
    $HOME/.wallpapers/         # referenced in ~/.local/bin/wallpaper.sh
    $HOME/.urxvt/ext.          # for copy/paste perls
    $HOME/.config/base16-env/  # https://github.com/SolitaryCipher/base16-env

## Building XMonad
1. Go to xmonad/
2. run `make`
3. Install dependancies or libraries if needed

