-- xmonad.hs 1.4.2   Time-stamp: <2015-01-28 17:55:22 PST xoddf2>

-- Features:
-- - Grid and Roledex layouts
-- - Zoom keybinding
-- - Hides border if there is only 1 window visible
-- - Keybindings to cycle through and toggle between workspaces
-- - ratpoison-style banish keybinding
-- - Keybinding to run or raise tmux, Emacs, Firefox
-- - SSH menu
-- - Shell menu
-- - Window menu
-- - xmobar
--
-- This config is recommended with the following lines in ~/.xinitrc or
-- ~/.xsession:
--
-- xscreensaver -no-splash &
-- urxvtd -q -o -f &
--
-- TODO:
-- - Switch to previous workspace if current one becomes empty.
-- - additionalKeysP
-- - Remember cursor position per workspace.  (Mainly for Firefox)

-- Imports
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
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
import XMonad.Layout.Circle
import XMonad.Layout.DwmStyle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Roledex
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window

-- Workspaces and what to put in them
myWorkspaces = ["1:main","2:www","3:media","4:gimp","5:vm","6:emulation","7:other"]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? a --> viewShift "1:main"      | a <- myClassMainShifts  ]
    , [ className =? b --> viewShift "2:www"       | b <- myClassWWWShifts   ]
    , [ className =? c --> viewShift "3:media"     | c <- myClassMediaShifts ]
    , [ className =? d --> viewShift "4:gimp"      | d <- myClassGIMPShifts  ]
    , [ className =? e --> viewShift "5:vm"        | e <- myClassVMShifts    ]
    , [ className =? f --> viewShift "6:emulation" | f <- myClassEmulShifts  ]
    , [ className =? g --> doF (W.shift "7:other") | g <- myClassOtherShifts ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassMainShifts  = ["URxvt","XTerm","Emacs"]
        myClassWWWShifts   = ["Firefox"]
        myClassMediaShifts = ["feh","mpv"]
        myClassGIMPShifts  = ["Gimp"]
        myClassVMShifts    = ["QEMU","VirtualBox"]
        myClassEmulShifts  = ["Mednafen","UAE","Xnest","Xephyr"]
        myClassOtherShifts = ["XConsole","XClock","XLoad","XBiff","Xmessage","Xpdf"]

-- Keybindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Launching
    [ ((modMask .|. shiftMask, xK_Return),       spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_slash),        shellPrompt defaultXPConfig)
    , ((modMask .|. shiftMask, xK_c),            kill)

    -- Layout
    , ((modMask,               xK_space),        sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space),        setLayout $ XMonad.layoutHook conf)

    -- Redraw
    , ((modMask,               xK_n),            refresh)

    -- Focus
    , ((modMask,               xK_j),            windows W.focusDown)
    , ((modMask,               xK_k),            windows W.focusUp)
    , ((modMask,               xK_m),            windows W.focusMaster)
    , ((modMask,               xK_Return),       focusUrgent)

    -- Moving and Resizing
    , ((modMask .|. shiftMask, xK_m),            windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j),            windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k),            windows W.swapUp)
    , ((modMask,               xK_h),            sendMessage Shrink)
    , ((modMask,               xK_l),            sendMessage Expand)
    , ((modMask,               xK_t),            withFocused $ windows . W.sink)
    , ((modMask .|. shiftMask, xK_h),            sendMessage (IncMasterN 1))
    , ((modMask .|. shiftMask, xK_l),            sendMessage (IncMasterN (-1)))
    , ((modMask,               xK_z),            sendMessage $ Toggle FULL)

    -- Leave
    , ((modMask .|. shiftMask, xK_backslash),    io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask, xK_q),            spawn "xmonad --recompile; xmonad --restart")
    , ((modMask,               xK_q),            spawn "xmonad --restart")
    , ((modMask,               xK_Escape),       spawn "xscreensaver-command -lock")

    -- Applications
    , ((modMask,               xK_a),            raiseMaybe (runInTerm "-name tmux -sl 0" "tmux attach-session") (resource =? "tmux"))
    -- Placeholder (This line will bind mod+s to SSH eventually.)
    , ((modMask,               xK_d),            raiseMaybe (spawn "emacsclient --alternate-editor='' -c") (className =? "Emacs"))
    , ((modMask,               xK_f),            raiseMaybe (spawn "firefox") (className =? "Firefox"))

    -- Open things
    , ((modMask,               xK_i),            spawn "xclip -o | xargs feh")
    , ((modMask,               xK_o),            spawn "xclip -o | xargs mpv")

    -- Banish Pointer
    , ((modMask,               xK_x),            banishScreen LowerRight)

    -- Menus
    , ((modMask .|. shiftMask, xK_apostrophe),   windowPromptGoto defaultXPConfig)
    , ((modMask .|. shiftMask, xK_period),       sshPrompt defaultXPConfig)

    -- Workspace Keybindings
    , ((modMask,               xK_Tab),          toggleWS)
    , ((modMask,               xK_bracketleft),  prevWS)
    , ((modMask,               xK_bracketright), nextWS)

    -- Volume (FreeBSD-specific)
    , ((modMask,               xK_comma),        spawn "mixer vol -1")
    , ((modMask,               xK_period),       spawn "mixer vol +1")
    ]
    ++

    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- Xinerama Keybindings
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse Bindings
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    ]

-- Banish pointer at startup
myStartupHook = do
    banishScreen LowerRight

-- Layouts (Tall, Mirror Tall, Full, Grid, ThreeColMid, Roledex)
myLayoutHook = avoidStruts
             $ smartBorders
             $ mkToggle (NOBORDERS ?? FULL ?? EOT)
             $ onWorkspace "1:main"      (tiled ||| (Mirror tiled) ||| Full ||| Grid ||| (ThreeColMid 1 (3/100) (1/2)) ||| Roledex)
             $ onWorkspace "2:www"       (Full ||| (Mirror tiled) ||| Grid ||| Roledex)
             $ onWorkspace "3:media"     (Full ||| Grid ||| Roledex)
             $ onWorkspace "4:gimp"      (withIM (1/5) (Role "gimp-toolbox") Grid)
             $ onWorkspace "5:vm"        Full
             $ onWorkspace "6:emulation" Full
             $ onWorkspace "7:other"     (tiled ||| (Mirror tiled) ||| Full ||| Grid ||| ThreeColMid 1 (3/100) (1/2) ||| Roledex)
             $ tiled
           ||| (Mirror tiled)
           ||| Full
    where
        tiled      = Tall nmaster delta ratio
        nmaster    = 1
        delta      = 1/100
        ratio      = 1/2

main = do
    xmproc <- spawnPipe "xmobar" -- For xmobar
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook         = manageDocks <+> myManageHook
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , workspaces         = myWorkspaces
        , logHook            = dynamicLogWithPP xmobarPP
                               { ppOutput = hPutStrLn xmproc
                               , ppTitle  = xmobarColor "green"  ""
                               , ppUrgent = xmobarColor "yellow" "red"
                               }
        , layoutHook         = myLayoutHook
        , startupHook        = myStartupHook
        , modMask            = mod4Mask
        , terminal           = "urxvtcd"
        , clickJustFocuses   = False
        , normalBorderColor  = "gray"
        , focusedBorderColor = "red"
        }
