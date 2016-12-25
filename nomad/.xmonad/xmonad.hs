-- xmonad.hs 2.0.5   Time-stamp: <2016-12-25 05:32:57 PST xoddf2>

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run
import Control.Monad
import Data.Monoid
import Data.Ratio ((%))
import System.Exit
import System.IO

-- xmonad-contrib
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook 
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window

-- Workspaces
myWorkspaces = ["1:main","2:www","3:media","4:gimp","5:vm","6:emul","7:games","8:xnest","9:misc"]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ [ className =? a --> viewShift "1:main"     | a <- myClassMainShifts ]
  , [ className =? b --> doF (W.shift "2:www")  | b <- myClassWWWShifts ]
  , [ className =? c --> viewShift "3:media"    | c <- myClassMediaShifts ]
  , [ className =? d --> doF (W.shift "4:gimp") | d <- myClassGIMPShifts ]
  , [ className =? e --> viewShift "5:vm"       | e <- myClassVMShifts ]
  , [ className =? f --> viewShift "6:emul"     | f <- myClassEmulShifts ]
  , [ className =? g --> viewShift "7:games"    | g <- myClassGameShifts ]
  , [ className =? h --> viewShift "8:xnest"    | h <- myClassXnestShifts ]
  , [ className =? i --> doF (W.shift "9:misc") | i <- myClassOtherShifts ]

  -- Floating
  , [ className =? "Torbrowser-launcher" --> doFloat
    , className =? "TorLauncher"         --> doFloat
    , className =? "QEMU"                --> doFloat
    , className =? "VirtualBox"          --> doFloat
    , className =? "Fceux"               --> doFloat
    , className =? "Snes9x-gtk"          --> doFloat
    , className =? "Gvbam"               --> doFloat
    , className =? "Desmume"             --> doFloat
    , className =? "Fusion"              --> doFloat
    , className =? "fs-uae"              --> doFloat
    , className =? "Wine"                --> doFloat
    , className =? "Xephyr"              --> doFloat
    , className =? "Xmessage"            --> doFloat
    , title     =? "Event Tester"        --> doFloat
    , className =? "XClock"              --> doFloat
    , className =? "XLoad"               --> doFloat
    , className =? "XBiff"               --> doFloat
    , className =? "Xmag"                --> doFloat
    , className =? "XCalc"               --> doFloat
    , className =? "XConsole"            --> doFloat
    ]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myClassMainShifts  = ["st-256color","XTerm","Emacs"]
    myClassWWWShifts   = ["Navigator","Firefox","Tor Browser","Torbrowser-launcher","TorLauncher","chromium-browser"]
    myClassMediaShifts = ["feh","mpv","Vlc"]
    myClassGIMPShifts  = ["Gimp"]
    myClassVMShifts    = ["QEMU","VirtualBox"]
    myClassEmulShifts  = ["Fceux","Snes9x-gtk","Gvbam","Desmume","Fusion","fs-uae","dosbox","Application.py"]
    myClassGameShifts  = ["lbreakout2","Frozen-Bubble 2"]
    myClassXnestShifts = ["Xephyr"]
    myClassOtherShifts = ["Wine","Xpdf","Audacity","Easytag","XConsole"]

-- Keybindings
myKeys =
  -- Focus Urgent Window
  [ ("M-<Backspace>",               focusUrgent)

  -- Run or Raise
  , ("M-a",                         raiseMaybe (runInTerm "-n tmux" "tmux attach-session")    (resource =? "tmux"))
  , ("M-s",                         raiseMaybe (spawn "dmenu_ssh 'st -n mosh -e mosh'")       (resource =? "mosh"))
  , ("M-d",                         raiseMaybe (spawn "emacsclient --alternate-editor='' -c") (className =? "Emacs"))
  , ("M-f",                         raiseMaybe (spawn "firefox")                              (className =? "Firefox"))
  , ("M-S-f",                       raiseMaybe (spawn "torbrowser-launcher")                  (className =? "Tor Browser"))

  -- mosh
  , ("M-S-s",                       spawn "dmenu_ssh 'st -n mosh -e mosh'")

  -- Banish Pointer
  , ("M-x",                         banishScreen LowerRight)

  -- Workspaces
  , ("M-<Tab>",                     toggleWS)
  , ("M-[",                         prevWS)
  , ("M-]",                         nextWS)

  -- Lock Screen
  , ("<XF86ScreenSaver>",           spawn "xscreensaver-command -lock")
  , ("M-<Escape>",                  spawn "xscreensaver-command -lock")

  -- Volume
  , ("<XF86AudioLowerVolume>",      spawn "amixer set Master 1-")
  , ("<XF86AudioRaiseVolume>",      spawn "amixer set Master 1+")
  , ("<XF86AudioMute>",             spawn "amixer set Master toggle")

  , ("M-;",                         spawn "amixer set Master 1-")
  , ("M-'",                         spawn "amixer set Master 1+")
  , ("M-/",                         spawn "amixer set Master toggle")

  -- mpd
  , ("<XF86AudioPrev>",             spawn "mpc -q prev")
  , ("<XF86AudioPlay>",             spawn "mpc -q toggle")
  , ("<XF86AudioStop>",             spawn "mpc -q stop")
  , ("<XF86AudioNext>",             spawn "mpc -q next")

  , ("M-z z",                       spawn "mpc -q prev")
  , ("M-z x",                       spawn "mpc -q play")
  , ("M-z c",                       spawn "mpc -q toggle")
  , ("M-z v",                       spawn "mpc -q stop")
  , ("M-z b",                       spawn "mpc -q next")

  -- Monitor Config
  , ("<XF86Display> <XF86Display>", spawn "monitor-switch --auto")
  , ("<XF86Display> d",             spawn "monitor-switch --dual")
  , ("<XF86Display> i",             spawn "monitor-switch --internal")
  , ("<XF86Display> e",             spawn "monitor-switch --external")
  , ("<XF86Display> m",             spawn "monitor-switch --mirror")

  , ("M-\\ M-\\",                   spawn "monitor-switch --auto")
  , ("M-\\ d",                      spawn "monitor-switch --dual")
  , ("M-\\ i",                      spawn "monitor-switch --internal")
  , ("M-\\ e",                      spawn "monitor-switch --external")
  , ("M-\\ m",                      spawn "monitor-switch --mirror")

  -- TrackPoint Toggle
  , ("<XF86TouchpadToggle>",        spawn "toggle-trackpoint")

  -- ThinkVantage
  , ("<XF86Launch1>",               spawn "xdg-open http://www.thinkwiki.org/wiki/Category:X200")
  ]

-- Startup (I'd rather put the spawn commands in ~/.xsession, but that fails)
myStartupHook = do
  spawn "xmodmap ~/.Xmodmap"
  runInTerm "-n tmux" "tmux attach-session"
  spawn "emacsclient -a '' -c"
  banishScreen LowerRight

-- Layouts
myLayoutHook = avoidStruts
               $ onWorkspace "1:main"  (tiled ||| (Mirror tiled) ||| Full ||| Grid ||| (ThreeCol 1 (3/100) (1/2)) ||| (ThreeColMid 1 (3/100) (1/2)))
               $ onWorkspace "2:www"   ((noBorders Full) ||| (Mirror tiled) ||| Grid)
               $ onWorkspace "3:media" ((noBorders Full) ||| Grid ||| tiled ||| (Mirror tiled) ||| (ThreeCol 1 (3/100) (1/2)) ||| (ThreeColMid 1 (3/100) (1/2)))
               $ onWorkspace "4:gimp"  (withIM (1/4) (Role "gimp-toolbox") Grid)
               $ onWorkspace "5:vm"    (noBorders Full)
               $ onWorkspace "6:emul"  (noBorders Full)
               $ onWorkspace "7:games" (noBorders Full)
               $ onWorkspace "8:xnest" (noBorders Full)
               $ onWorkspace "9:misc"  (tiled ||| (Mirror tiled) ||| Full ||| Grid ||| (ThreeCol 1 (3/100) (1/2)) ||| (ThreeColMid 1 (3/100) (1/2)))
               $ tiled
               ||| (Mirror tiled)
               ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    delta   = 1/100
    ratio   = 50/100

-- Main
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { manageHook         = manageDocks <+> myManageHook
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , startupHook        = myStartupHook
    , logHook            = dynamicLogWithPP xmobarPP
                           { ppOutput  = hPutStrLn xmproc
                           , ppSep     = " "
                           , ppTitle   = xmobarColor "#8BDE58" ""
                           , ppCurrent = xmobarColor "#DBDF39" ""
                           , ppLayout  = xmobarColor "#7DACDE" ""
                           , ppUrgent  = xmobarColor "#DD424C" ""
                           }
    , modMask            = mod4Mask
    , terminal           = "st"
    , normalBorderColor  = "#929292"
    , focusedBorderColor = "#8BDE58"
    , borderWidth        = 2
    } `additionalKeysP` myKeys
