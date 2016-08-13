--
-- SolitaryCipher's XMonad  config
-- 

import System.Exit
import System.IO
import System.Environment (getEnv)

import Data.Monoid
import qualified Data.Map        as M
import qualified Data.List       as L
import Data.String.Utils (strip)

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Replace
import XMonad.Util.EZConfig

import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation as Nav
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize

import XMonad.Actions.CycleWS
import XMonad.Actions.Volume

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place

import Bar
import Color

myModMask       = mod4Mask
altMask         = mod1Mask
myTerminal      = "urxvt"
myWorkspaces    = ["001","010","011","100","101","110","111","080","090"]

myClickJustFocuses = False
myFocusFollowsMouse = True

myStartupHook = spawn     $ "~/.xmonad/autostart"
myBarScript   = spawnPipe $ "~/.config/admiral.d/start_admiral.sh"
myLauncher    =             "dmenu_run -i -hist ~/.local/share/dmenu_hist -uh 2 -h 22"
--myLauncher    =             "j4-dmenu-desktop --dmenu='dmenu -i -hist ~/.local/share/dmenu_hist -uh 3 -h 24' --term='bash'"

myBorderWidth        = 2
myNormalBorderColor  = DarkGrey --"#373b41" -- base 02 (tomorrow) grey
myFocusedBorderColor = Magenta  --"#b294bb" -- base 0E (tomorrow) magenta
myMarkedColor        = Red      --"#b5bd68" -- base 08 (tomorrow) red

myLayout markedColor = spacing 0 $ borderResize $ 
         configurableNavigation config 
            (   avoidStruts $ bsp  
            ||| tiled     
            ||| Mirror tiled
            ||| noBorders Full
            )
  where
    bsp     = configurableBSP "#ff00ff" 0.5
    config  = noNavigateBorders
    tiled   = ResizableTall nmaster delta ratio slaves
    nmaster = 1      -- default num of windows in master pane
    ratio   = 1/2    -- default proportion occupied by master
    delta   = 3/100  -- percent of screen to increment
    slaves  = [1/16] -- fraction to mult window height 


myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "barapp"         --> placeHook app_location <+> doFloat
    , resource  =? "kdesktop"       --> doIgnore 
    ] where app_location = fixed (1- 5/1080,45/1920)

newManageHook = myManageHook <+> manageDocks <+> placeHook (inBounds (underMouse (0, 0))) <+> manageHook defaultConfig

myLogHook h colors = dynamicLogWithPP defaultPP -- ( -- def
      { ppCurrent           = barOL Magenta . barBG LightGrey . makeClickable . pad
      , ppVisible           = barOL Yellow . makeClickable . pad
      , ppHidden            = makeClickable . pad
      , ppUrgent            = barOL Red . makeClickable . pad
      , ppWsSep             = "  "
      , ppSep               = "  "
      , ppLayout            = barBG DarkGrey . clickable ("xdotool key shift+super+space") . pad
      , ppTitle             = \x -> ""
      , ppOutput            = \s -> hPutStrLn h ("  "++s)
      } -- ) 
    where 
        barBG = Bar.bg . getColor colors
        barFG = Bar.fg . getColor colors
        barOL = Bar.ol . getColor colors
        makeClickable ws = clickable ("xdotool key super+"++(getWSNumber ws)) ws
        getWSNumber w = show $ (1+) $ makeN $ L.elemIndex (strip w) myWorkspaces
            where
                makeN (Just n) = n
                makeN Nothing  = 0

getEnvironment var = do
    str <- getEnv var
    return str

main = do
    bar <- myBarScript
    app <- spawnPipe "zsh" -- spawn here to keep it global.

    blue      <- getEnvironment "base0D"
    darkGrey  <- getEnvironment "base02"
    lightGrey <- getEnvironment "base03"
    green     <- getEnvironment "base0B"
    red       <- getEnvironment "base08"
    black     <- getEnvironment "base01" -- bar bg ...
    magenta   <- getEnvironment "base0E"
    yellow    <- getEnvironment "base0A"
    let colors = ColorSet { blue      = "#" ++ blue
                          , darkGrey  = "#" ++ darkGrey
                          , lightGrey = "#" ++ "4c4c4c" --lightGrey
                          , green     = "#" ++ green
                          , black     = "#" ++ black
                          , red       = "#" ++ red
                          , magenta   = "#" ++ magenta
                          , yellow    = "#" ++ yellow
                          }
    
    replace
    xmonad  $ withUrgencyHook NoUrgencyHook
            -- $ def 
            $ defaultConfig
            { terminal           = myTerminal
            , workspaces         = myWorkspaces

            , focusFollowsMouse  = myFocusFollowsMouse
            , clickJustFocuses   = myClickJustFocuses

            , normalBorderColor  = getColor colors myNormalBorderColor 
            , focusedBorderColor = getColor colors myFocusedBorderColor 
            , borderWidth        = myBorderWidth

            , modMask            = myModMask
            , keys               = myKeys app
            , mouseBindings      = myMouseBindings

            , layoutHook         = myLayout $ getColor colors myMarkedColor 
            , handleEventHook    = mempty <+> docksEventHook
            , logHook            = myLogHook bar colors -- fadeInactiveLogHok 0xdddddddd
            , startupHook        = myStartupHook
            , manageHook         = newManageHook
        }
        `additionalKeysP` [
              ("<XF86MonBrightnessUp>", spawn $ "xbacklight -inc 10")
            , ("<XF86MonBrightnessDown>", spawn $ "xbacklight -dec 10")
            , ("<XF86AudioLowerVolume>", lowerVolume 3 >> return ())
            , ("<XF86AudioRaiseVolume>", raiseVolume 3 >> return ())
            , ("<XF86AudioMute>", toggleMute >> return ())
        ]


myKeys appBinding conf@(XConfig {XMonad.modMask = modm}) = 
  M.fromList $
  [ ((modm .|.   shiftMask, xK_Return     ), spawn $ XMonad.terminal conf)
  , ((modm                , xK_Return     ), spawn $ XMonad.terminal conf)
  , ((modm                , xK_space      ), spawn myLauncher)
  , ((modm                , xK_p          ), spawn myLauncher)
  , ((modm .|.   shiftMask, xK_c          ), kill)

  , ((modm .|.   shiftMask, xK_q          ), io (exitWith ExitSuccess))
  , ((modm                , xK_q          ), spawn "xmonad --recompile; xmonad --restart")

  -- Resize viewed windows to the cor     rect size
  , ((modm                , xK_x          ), refresh)
  
  , ((modm .|. shiftMask  , xK_space      ), sendMessage NextLayout)

  , ((modm                , xK_Tab        ), windows W.focusDown)

  , ((modm                , xK_l          ), sendMessage $ Nav.Go R)
  , ((modm                , xK_h          ), sendMessage $ Nav.Go L)
  , ((modm                , xK_k          ), sendMessage $ Nav.Go U)
  , ((modm                , xK_j          ), sendMessage $ Nav.Go D)

  , ((modm                , xK_Right      ), sendMessage $ Nav.Go R)
  , ((modm                , xK_Left       ), sendMessage $ Nav.Go L)
  , ((modm                , xK_Up         ), sendMessage $ Nav.Go U)
  , ((modm                , xK_Down       ), sendMessage $ Nav.Go D)


  , ((modm .|. controlMask, xK_h          ), sendMessage $ MoveSplit L)
  , ((modm .|. controlMask, xK_j          ), sendMessage $ MoveSplit D)
  , ((modm .|. controlMask, xK_k          ), sendMessage $ MoveSplit U)
  , ((modm .|. controlMask, xK_l          ), sendMessage $ MoveSplit R)

  , ((modm .|. shiftMask  , xK_l          ), sendMessage $ Nav.Swap R)
  , ((modm .|. shiftMask  , xK_h          ), sendMessage $ Nav.Swap L)
  , ((modm .|. shiftMask  , xK_k          ), sendMessage $ Nav.Swap U)
  , ((modm .|. shiftMask  , xK_j          ), sendMessage $ Nav.Swap D)
  
  , ((modm                , xK_s          ), sendMessage BSP.Swap)
  , ((modm                , xK_r          ), sendMessage Rotate)

  , ((modm                , xK_n          ), sendMessage SelectNode)
  , ((modm                , xK_m          ), sendMessage MoveNode)

  , ((modm                , xK_grave      ), toggleWS)

  , ((modm                , xK_bracketleft  ), prevWS)
  , ((modm                , xK_bracketright ), nextWS)
  , ((modm .|. shiftMask  , xK_bracketleft  ), shiftToPrev >> prevWS )
  , ((modm .|. shiftMask  , xK_bracketright ), shiftToNext >> nextWS )

  , ((modm                , xK_comma  ), prevScreen)
  , ((modm                , xK_period ), nextScreen)
  , ((modm .|. shiftMask  , xK_comma  ), swapPrevScreen)
  , ((modm .|. shiftMask  , xK_period ), swapNextScreen)

  , ((modm                , xK_z      ), windows W.swapMaster) 

  , ((modm                , xK_f      ), withFocused $ windows . W.sink)
  , ((modm                , xK_t      ), withFocused $ windows . W.sink)

  , ((modm                , xK_b      ), sendMessage $ ToggleStruts)
  , ((modm                , xK_equal  ), sendMessage $ IncSpacing $ 2 )
  , ((modm                , xK_minus  ), sendMessage $ IncSpacing $ -2)
  , ((modm .|. controlMask, xK_1      ), io $ barApp appBinding "/home/nick/.local/bin/gcal2.sh")

  , (( controlMask .|. altMask   , xK_l), spawn $ "lockscreen.sh" )
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]
  
