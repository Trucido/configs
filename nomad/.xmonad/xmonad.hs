-- xmonad.hs
-- Time-stamp: <2018-03-06 19:31:50 PST xoddf2>

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.ManageHook
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
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook 
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.SimplestFloat
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

-- Workspaces
myWorkspaces = ["1:main","2:www","3:tor","4:media","5:doc","6:gimp","7:vm/emul","8:misc"]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ [ className =? a --> viewShift "1:main"     | a <- myClassMainShifts  ]
  , [ className =? b --> doF (W.shift "2:www")  | b <- myClassWWWShifts   ]
  , [ className =? c --> doF (W.shift "3:tor")  | c <- myClassTBBShifts   ]
  , [ className =? d --> viewShift "4:media"    | d <- myClassMediaShifts ]
  , [ className =? e --> viewShift "5:doc"      | e <- myClassDocShifts   ]
  , [ className =? f --> doF (W.shift "6:gimp") | f <- myClassGIMPShifts  ]
  , [ className =? g --> viewShift "7:vm/emul"  | g <- myClassVMShifts    ]
  , [ className =? h --> viewShift "8:misc"     | h <- myClassOtherShifts ]

  -- Floating
  , [ resource  =? "Browser"              --> doFloat
    , resource  =? "Places"               --> doFloat
    , className =? "Torbrowser-launcher"  --> doFloat
    , className =? "TorLauncher"          --> doFloat
    , className =? "Gtk-gnash"            --> doFloat
    , title     =? "Event Tester"         --> doFloat
    , className =? "Nm-applet"            --> doFloat
    , className =? "Nm-connection-editor" --> doFloat
    , className =? "Uim-pref-gtk"         --> doFloat
    , className =? "XClock"               --> doFloat
    , className =? "XLoad"                --> doFloat
    , className =? "XBiff"                --> doFloat
    , className =? "Xmessage"             --> doFloat
    , className =? "Xmag"                 --> doFloat
    , className =? "XCalc"                --> doFloat
    , className =? "XConsole"             --> doFloat
    ]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myClassMainShifts  = ["st-256color","XTerm","Emacs"]
    myClassWWWShifts   = ["Firefox-esr","Chromium-browser"]
    myClassTBBShifts   = ["Tor Browser","Torbrowser-launcher","TorLauncher"]
    myClassMediaShifts = ["feh","mpv","vlc","Gtk-gnash"]
    myClassDocShifts   = ["Xpdf"]
    myClassGIMPShifts  = ["Gimp"]
    myClassVMShifts    = ["QEMU","VirtualBox","Xephyr","Hiro","Fusion","stella","mGBA","Desmume","fs-uae","dosbox","Application.py","Wine"]
    myClassOtherShifts = ["Audacity","Easytag","Wireshark","lbreakout2","perl"]

-- Layouts
myLayoutHook = avoidStruts
               $ onWorkspace "1:main"    (Full ||| tiled)
               $ onWorkspace "2:www"     ((noBorders Full) ||| (Mirror tiled) ||| Grid)
               $ onWorkspace "3:tor"     simplestFloat
               $ onWorkspace "4:media"   ((noBorders Full) ||| Grid)
               $ onWorkspace "5:doc"     (noBorders Full)
               $ onWorkspace "6:gimp"    gimpLayout
               $ onWorkspace "7:vm/emul" simplestFloat
               $ onWorkspace "8:misc"    (Full ||| (Mirror tiled) ||| tiled)
               $ tiled
               ||| (Mirror tiled)
               ||| Full
  where
    tiled      = Tall nmaster delta ratio
    nmaster    = 1
    delta      = 1/100
    ratio      = 50/100
    gimpLayout = withIM (1/6) (Role "gimp-toolbox") $
      reflectHoriz $
      withIM (1/5) (Role "gimp-dock") $
      (Full ||| Grid)

-- Scratchpad
myScratchPads = [ NS "volume"   "pavucontrol" (className =? "Pavucontrol") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                , NS "password" "keepassx"    (className =? "Keepassx")    (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                ]

-- Keybindings
myKeys =
  -- Focus Urgent Window
  [ ("M-<Backspace>",               focusUrgent)

  -- Run or Raise
  , ("M-a",                         raiseMaybe (runInTerm "-n tmux" "tmux attach-session")    (resource =? "tmux"))
  , ("M-s",                         raiseMaybe (spawn "dmenu_ssh 'st -n mosh -e mosh'")       (resource =? "mosh"))
  , ("M-d",                         raiseMaybe (spawn "emacsclient --alternate-editor='' -c") (className =? "Emacs"))
  , ("M-f",                         raiseMaybe (spawn "firefox-esr")                          (className =? "Firefox-esr"))
  , ("M-S-f",                       raiseMaybe (spawn "torbrowser")                           (className =? "Tor Browser"))

  -- Run
  , ("M-p",                         spawn "dmenu_run -fn 'Terminus-11' -nb '#3F3F3F' -nf '#DCDCCC' -sb '#3F3F3F' -sf '#C3BF9F'")

  -- mosh
  , ("M-S-s",                       spawn "dmenu_ssh 'st -n mosh -e mosh'")

  -- Password Manager
  , ("M-`",                         namedScratchpadAction myScratchPads "password")

  -- Banish Pointer
  , ("M-x",                         banishScreen LowerRight)

  -- Workspaces
  , ("M-<Tab>",                     myToggle)
  , ("M-[",                         moveTo Prev (WSIs notSP))
  , ("M-]",                         moveTo Next (WSIs notSP))

  -- Lock Screen
  , ("<XF86ScreenSaver>",           spawn "xscreensaver-command -lock")
  , ("M-<Escape>",                  spawn "xscreensaver-command -lock")

  -- Volume
  , ("<XF86AudioLowerVolume>",      spawn "pactl set-sink-volume 0 -1%")
  , ("<XF86AudioRaiseVolume>",      spawn "pactl set-sink-volume 0 +1%")
  , ("<XF86AudioMute>",             spawn "pactl set-sink-mute 0 toggle")

  , ("M-;",                         spawn "pactl set-sink-volume 0 -1%")
  , ("M-'",                         spawn "pactl set-sink-volume 0 +1%")
  , ("M-/",                         spawn "pactl set-sink-mute 0 toggle")

  , ("M-v",                         namedScratchpadAction myScratchPads "volume")

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

  -- Clear Clipboard
  , ("M-=",                         spawn "clear-clipboard")

  -- ThinkVantage
  , ("<XF86Launch1>",               spawn "xdg-open http://www.thinkwiki.org/wiki/Category:X200")
  ]
  where
    notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
    shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
            >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
    shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
            >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
    getSortByIndexNoSP =
            fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
    myToggle = windows $ W.view =<< W.tag . head . filter
            ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

myRemoveKeys =
  -- I can just run xmonad --recompile and xmonad --restart instead:
  [ "M-q"
  -- I prefer dmenu instead, and gmrun is not even installed:
  , "M-S-p"
  ]

-- Startup
myStartupHook = do
  banishScreen LowerRight

-- Main
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar-top.hs"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { manageHook         = manageDocks <+> myManageHook <+> namedScratchpadManageHook myScratchPads
    , layoutHook         = myLayoutHook
    , XMonad.workspaces  = myWorkspaces
    , startupHook        = myStartupHook
    , logHook            = dynamicLogWithPP xmobarPP
                           { ppOutput  = hPutStrLn xmproc
                           , ppSep     = " <fc=#709080>|</fc> "
                           , ppTitle   = xmobarColor "#C3BF9F" ""
                           , ppCurrent = xmobarColor "#F0DFAF" ""
                           , ppLayout  = xmobarColor "#94BFF3" ""
                           , ppUrgent  = xmobarColor "#DCA3A3" ""
                           , ppSort    = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort defaultPP
                           } <+> ewmhDesktopsLogHook
    , handleEventHook    = ewmhDesktopsEventHook <+> docksEventHook <+> fullscreenEventHook
    , modMask            = mod4Mask
    , terminal           = "st"
    , normalBorderColor  = "#6F6F6F"
    , focusedBorderColor = "#7F9F7F"
    , borderWidth        = 2
    } `additionalKeysP` myKeys `removeKeysP` myRemoveKeys
