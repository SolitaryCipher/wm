{-# OPTIONS -fno-warn-missing-signatures #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

----------------------------------------------------------------------------------
module Main where

import XMonad hiding (config)

import System.Exit
import System.IO

import qualified Data.Map        as M
import qualified Data.List       as L
import Data.String.Utils (strip)

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig

import Local.XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.ResizableTile
import Local.XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation as Nav
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize
import XMonad.Layout.Renamed

import XMonad.Actions.CycleWS

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place

import qualified Local.Color as Local
import qualified Local.Bar   as Bar


myModMask       = mod4Mask
altMask         = mod1Mask
ctrlMask        = controlMask
myTerminal      = "urxvt256c"
--myTerminal      = "gnome-terminal"
myWorkspaces    = ["001","010","011","100","101","110","111","080","090"]

myClickJustFocuses = False
myFocusFollowsMouse = True

myStartupHook = spawn "autostart"
myLauncher _ = "start_rofi.sh"

myBorderWidth        = 2
myNormalBorderColor  = Local.darkGrey --"#373b41" -- base 02 (tomorrow) grey
myFocusedBorderColor = Local.magenta  --"#b294bb" -- base 0E (tomorrow) magenta
myMarkedColor        = Local.red      --"#b5bd68" -- base 08 (tomorrow) red

myLayout markedColor = renamed [CutWordsLeft 2] $ spacing 0 $ borderResize $ 
         configurableNavigation config 
            (   avoidStruts $ bsp  
            ||| tiled     
            ||| noBorders Full
            )
  where
    bsp     = configurableBSP markedColor 0.5
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

newManageHook = myManageHook <+> manageDocks <+> placeHook (inBounds (underMouse (0, 0))) <+> manageHook def

myLogHook h colors = dynamicLogWithPP def -- ( -- def
      { ppCurrent           = barOL Local.magenta . barBG Local.lightGrey . makeClickable . pad
      , ppVisible           = barOL Local.yellow . makeClickable . pad
      , ppHidden            = makeClickable . pad
      , ppUrgent            = barOL Local.red . makeClickable . pad
      , ppWsSep             = "  "
      , ppSep               = "  "
      , ppLayout            = barBG Local.darkGrey . Bar.clickable "xdotool key shift+super+space" . pad
      , ppTitle             = const ""
      , ppOutput            = \s -> hPutStrLn h ("  "++s)
      } -- ) 
    where 
        barBG c = Bar.bg $ show $ c colors
        --barFG c = Bar.fg $ (show $ c colors)
        barOL c = Bar.ol $ show $ c colors
        makeClickable ws = Bar.clickable ("xdotool key super+"++getWSNumber ws) ws
        getWSNumber w = show $ (1+) $ makeN $ L.elemIndex (strip w) myWorkspaces
            where
                makeN (Just n) = n
                makeN Nothing  = 0

main :: IO ()
main = do
    --bar <- myBarScript
    --app <- spawnPipe "bash" -- spawn here to keep it global.
    colors <- Local.getB16Colors

    xmonad $
      ewmh $
        --pagerHints $
       withUrgencyHook NoUrgencyHook
            -- $ def 
            $ def
            { terminal           = myTerminal
            , workspaces         = myWorkspaces

            , focusFollowsMouse  = myFocusFollowsMouse
            , clickJustFocuses   = myClickJustFocuses

            , normalBorderColor  = show $ myNormalBorderColor colors 
            , focusedBorderColor = show $ myFocusedBorderColor colors 
            , borderWidth        = myBorderWidth

            , modMask            = myModMask
            , keys               = myKeys colors
            , mouseBindings      = myMouseBindings

            , layoutHook         = myLayout $ show $ myMarkedColor colors
            , handleEventHook    = mempty <+> docksEventHook
            --, logHook            = myLogHook bar colors -- fadeInactiveLogHok 0xdddddddd
            , startupHook        = myStartupHook
            , manageHook         = newManageHook
        }
        `additionalKeysP`
            [ ("<XF86MonBrightnessUp>"  , spawn "xbacklight -inc 1")
            , ("<Print>"                , spawn "scrot '%Y-%m-%d_%H%M%S.png' -e 'mv $f ~/pics/screenshots/'")
            , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 1")
            , ("<XF86AudioLowerVolume>" , spawn "amixer set Master 2%-")
            , ("<XF86AudioRaiseVolume>" , spawn "amixer set Master 2%+")
            , ("<XF86AudioMute>"        , spawn "amixer set Master toggle -q -D pulse")
        ]


myKeys colors conf@XConfig {XMonad.modMask = modm} = 
  M.fromList $
  [ ((modm .|.   shiftMask, xK_Return       ), spawn $ XMonad.terminal conf)
  , ((modm                , xK_Return       ), spawn $ XMonad.terminal conf)
  , ((modm                , xK_space        ), spawn $ myLauncher colors )
  , ((modm                , xK_p            ), spawn $ myLauncher colors )

  , ((modm .|.   shiftMask, xK_c            ), kill)

  --, ((modm                , xK_q ), spawn $ XMonad.terminal conf)
  , ((modm .|.   shiftMask, xK_q            ), io exitSuccess)
  --, ((modm                , xK_q          ), spawn "xmonad --recompile; xmonad --restart")

  -- Resize viewed windows to the cor       rect size
  , ((modm                , xK_x            ), refresh)
  
  , ((modm .|. shiftMask  , xK_space        ), sendMessage NextLayout)

  , ((modm                , xK_Tab          ), windows W.focusDown)
  
  , ((modm                , xK_s            ), sendMessage BSP.Swap)
  , ((modm                , xK_r            ), sendMessage Rotate)

  , ((modm                , xK_m            ), sendMessage SelectNode)
  , ((modm .|. shiftMask  , xK_m            ), sendMessage MoveNode)

  , ((modm                , xK_grave        ), toggleWS)
  , ((modm                , xK_Escape       ), toggleWS)

  , ((modm                , xK_bracketleft  ), prevWS)
  , ((modm                , xK_bracketright ), nextWS)
  , ((modm .|. shiftMask  , xK_bracketleft  ), shiftToPrev >> prevWS )
  , ((modm .|. shiftMask  , xK_bracketright ), shiftToNext >> nextWS )

  , ((modm                , xK_comma        ), prevScreen)
  , ((modm                , xK_period       ), nextScreen)
  , ((modm .|. shiftMask  , xK_comma        ), swapPrevScreen)
  , ((modm .|. shiftMask  , xK_period       ), swapNextScreen)

  , ((modm                , xK_z            ), windows W.swapMaster) 

  , ((modm                , xK_f            ), withFocused $ windows . W.sink)
  , ((modm                , xK_t            ), withFocused $ windows . W.sink)

  , ((modm                , xK_b            ), sendMessage   ToggleStruts)
  , ((modm                , xK_equal        ), sendMessage $ IncSpacing   2 )
  , ((modm                , xK_minus        ), sendMessage $ IncSpacing (-2))

  , ((altMask .|. ctrlMask, xK_l            ), spawn "lockscreen.sh" )
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  ++ -- window navigation, swapping, and moving splits. 
     -- mod-[hjkl] and mod-[up,down,left,right] will go to the split
     -- adding the shift modifier will swap windows, 
     -- adding control modifier will move the split 
    [((modm .|. m, k), f i)  
      | (i, k) <- concat $ zipWith (zip . repeat) [L, R, U, D] keys'
      , (f, m) <- [(sendMessage . Nav.Go   , 0),
                   (sendMessage . Nav.Swap , shiftMask),
                   (sendMessage . MoveSplit, ctrlMask)]
    ] where keys' = [ [xK_Left,  xK_h] -- keys for left-y actions
                    , [xK_Right, xK_l] -- keys for right-y actions
                    , [xK_Up,    xK_k] -- upwards actions
                    , [xK_Down,  xK_j] -- downwards actions
                    ]

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList 
    -- Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
    ]
  


