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
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig

import qualified XMonad.Layout.WindowNavigation as Nav
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.NoBorders     (noBorders)
import XMonad.Layout.BorderResize  (borderResize)
import XMonad.Layout.Renamed       (Rename(CutWordsLeft), renamed)

import Local.XMonad.Layout.Spacing (SpacingMsg(..), spacing)
import qualified Local.XMonad.Layout.BinarySpacePartition as BSP

import XMonad.Actions.CycleWS (shiftToNext, shiftToPrev
                              , nextScreen, prevScreen
                              , swapNextScreen, swapPrevScreen
                              , nextWS,  prevWS, toggleWS)

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks  (ToggleStruts(..), manageDocks, docksEventHook, avoidStruts)
import XMonad.Hooks.UrgencyHook  (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Hooks.Place        (placeHook, inBounds, fixed, underMouse)

import System.Taffybar.Hooks.PagerHints (pagerHints)

import qualified Local.Color as Colors


myModMask       = mod4Mask
altMask         = mod1Mask
ctrlMask        = controlMask
myTerminal      = "urxvt256c"
myWorkspaces    = ["001","010","011","100","101","110","111","080","090"]

myClickJustFocuses = False
myFocusFollowsMouse = True

myStartupHook = "autostart"
myLauncher = "start_rofi.sh"

myBorderWidth        = 2
myNormalBorderColor  = Colors.darkGrey --"#373b41" -- base 02 (tomorrow) grey
myFocusedBorderColor = Colors.magenta  --"#b294bb" -- base 0E (tomorrow) magenta
myMarkedColor        = Colors.red      --"#b5bd68" -- base 08 (tomorrow) red

myLayout markedColor = renamed [CutWordsLeft 2] 
                     $ spacing 0 
                     $ borderResize 
                     $ Nav.configurableNavigation config 
                     (   avoidStruts $ bsp  
                     ||| tiled     
                     ||| noBorders Full
                     )
  where
    bsp     = BSP.configurableBSP markedColor 0.5
    config  = Nav.noNavigateBorders
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

main :: IO ()
main = do
    colors <- Colors.getB16Colors

    xmonad $ ewmh $ pagerHints $ withUrgencyHook NoUrgencyHook
      $ def { terminal           = myTerminal
            , workspaces         = myWorkspaces

            , focusFollowsMouse  = myFocusFollowsMouse
            , clickJustFocuses   = myClickJustFocuses

            , normalBorderColor  = show $ myNormalBorderColor colors 
            , focusedBorderColor = show $ myFocusedBorderColor colors 
            , borderWidth        = myBorderWidth

            , modMask            = myModMask
            , keys               = myKeys 
            , mouseBindings      = myMouseBindings

            , layoutHook         = myLayout $ show $ myMarkedColor colors
            , handleEventHook    = mempty <+> docksEventHook
            , startupHook        = spawn myStartupHook
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


myKeys conf@XConfig {XMonad.modMask = modm} = 
  M.fromList $
  [ ((modm .|.   shiftMask, xK_Return       ), spawn $ XMonad.terminal conf)
  , ((modm                , xK_Return       ), spawn $ XMonad.terminal conf)
  , ((modm                , xK_space        ), spawn myLauncher )
  , ((modm                , xK_p            ), spawn myLauncher )

  , ((modm .|.   shiftMask, xK_c            ), kill)

  --, ((modm                , xK_q ), spawn $ XMonad.terminal conf)
  , ((modm .|.   shiftMask, xK_q            ), io exitSuccess)
  --, ((modm                , xK_q          ), spawn "xmonad --recompile; xmonad --restart")

  -- Resize viewed windows to the cor       rect size
  , ((modm                , xK_x            ), refresh)
  
  , ((modm .|. shiftMask  , xK_space        ), sendMessage NextLayout)

  , ((modm                , xK_Tab          ), windows W.focusDown)
  
  , ((modm                , xK_s            ), sendMessage BSP.Swap)
  , ((modm                , xK_r            ), sendMessage BSP.Rotate)

  , ((modm                , xK_m            ), sendMessage BSP.SelectNode)
  , ((modm .|. shiftMask  , xK_m            ), sendMessage BSP.MoveNode)

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
      | (i, k) <- concat $ zipWith (zip . repeat) [Nav.L, Nav.R, Nav.U, Nav.D] keys'
      , (f, m) <- [(sendMessage . Nav.Go   , 0),
                   (sendMessage . Nav.Swap , shiftMask),
                   (sendMessage . BSP.MoveSplit, ctrlMask)]
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
  


