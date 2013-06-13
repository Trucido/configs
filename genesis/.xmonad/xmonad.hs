-- woddfellow2's xmonad Config
-- by woddfellow2 | http://wlair.us.to/

-- Features:
-- - Spiral and Grid layouts
-- - Zoom keybinding
-- - Hides border if there is only 1 window visible
-- - Keybindings to cycle through and toggle between workspaces
-- - ratpoison-style banish keybinding
-- - Keybinding to run or raise xombrero
-- - FloatKeys
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

-- Imports
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import Control.Monad
import Data.Monoid
import System.Exit
import System.IO

-- xmonad-contrib
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook 
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window

-- Workspaces and what to put in them
myWorkspaces = ["1:term","2:www","3:vi","4:media","5:Xnest","6:other"]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? a --> viewShift "1:term"      | a <- myClassTermShifts  ]
    , [ className =? b --> doF (W.shift "2:www")   | b <- myClassWWWShifts   ]
    , [ className =? c --> viewShift "3:vi"        | c <- myClassgVimShifts  ]
    , [ className =? d --> viewShift "4:media"     | d <- myClassMediaShifts ]
    , [ className =? e --> viewShift "5:Xnest"     | e <- myClassXnestShifts ]
    , [ className =? f --> doF (W.shift "6:other") | f <- myClassOtherShifts ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassTermShifts  = ["URxvt","XTerm"]
        myClassWWWShifts   = ["Xombrero"]
        myClassgVimShifts  = ["Gvim"]
        myClassMediaShifts = ["feh","MPlayer"]
        myClassXnestShifts = ["Xnest","Xephyr"]
        myClassOtherShifts = ["XConsole","XClock","XLoad","XBiff","Xmessage"]

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
    , ((modMask .|. shiftMask, xK_equal),        sendMessage $ Toggle FULL)

    -- Leave
    , ((modMask .|. shiftMask, xK_backslash),    io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask, xK_q),            spawn "xmonad --recompile; xmonad --restart")
    , ((modMask,               xK_q),            spawn "xmonad --restart")
    , ((modMask,               xK_Escape),       spawn "xscreensaver-command -lock")

    -- Applications
    , ((modMask .|. shiftMask, xK_f),            runOrRaise "xombrero" (className =? "Xombrero"))

    -- Banish Pointer
    , ((modMask,               xK_x),            banishScreen LowerRight)

    -- Menus
    , ((modMask .|. shiftMask, xK_apostrophe),   windowPromptGoto defaultXPConfig)
    , ((modMask .|. shiftMask, xK_period),       sshPrompt defaultXPConfig)

    -- Workspace Keybindings
    , ((modMask,               xK_Tab),          toggleWS)
    , ((modMask,               xK_bracketleft),  prevWS)
    , ((modMask,               xK_bracketright), nextWS)
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
    ++

    -- FloatKeys:
    -- Down:
    [ ((modMask,                 xK_Down),      withFocused (keysMoveWindow   (0,1)))         -- Move
    , ((modMask .|. shiftMask,   xK_Down),      withFocused (keysResizeWindow (0,-1)  (0,1))) -- Decrease
    , ((modMask .|. controlMask, xK_Up),        withFocused (keysResizeWindow (0,1)   (0,1))) -- Increase
    -- Up:
    , ((modMask,                 xK_Up),        withFocused (keysMoveWindow   (0,-1)))        -- Move
    , ((modMask .|. shiftMask,   xK_Up),        withFocused (keysResizeWindow (0,-1)  (1,0))) -- Decrease
    , ((modMask .|. controlMask, xK_Down),      withFocused (keysResizeWindow (0,1)   (1,0))) -- Increase
    -- Left:
    , ((modMask,                 xK_Left),      withFocused (keysMoveWindow   (-1,0)))        -- Move
    , ((modMask .|. shiftMask,   xK_Right),     withFocused (keysResizeWindow (-1,0)  (1,1))) -- Decrease
    , ((modMask .|. controlMask, xK_Left),      withFocused (keysResizeWindow (1,0)   (1,1))) -- Increase
    -- Right:
    , ((modMask,                 xK_Right),     withFocused (keysMoveWindow   (1,0)))         -- Move
    , ((modMask .|. shiftMask,   xK_Left),      withFocused (keysResizeWindow (-1,0)  (0,1))) -- Decrease
    , ((modMask .|. controlMask, xK_Right),     withFocused (keysResizeWindow (1,0)   (0,1))) -- Increase
    -- Northwest:
    , ((modMask,                 xK_Home),      withFocused (keysMoveWindow   (-1,-1)))       -- Move
    , ((modMask .|. shiftMask,   xK_Home),      withFocused (keysResizeWindow (-1,-1) (1,1))) -- Decrease
    , ((modMask .|. controlMask, xK_Home),      withFocused (keysResizeWindow (1,1)   (1,1))) -- Increase
    -- Northeast:
    , ((modMask,                 xK_Page_Up),   withFocused (keysMoveWindow   (1,-1)))        -- Move
    , ((modMask .|. shiftMask,   xK_Page_Up),   withFocused (keysResizeWindow (-1,-1) (0,1))) -- Decrease
    , ((modMask .|. controlMask, xK_Page_Up),   withFocused (keysResizeWindow (1,1)   (0,1))) -- Increase
    -- Southwest:
    , ((modMask,                 xK_End),       withFocused (keysMoveWindow   (-1,1)))        -- Move
    , ((modMask .|. shiftMask,   xK_End),       withFocused (keysResizeWindow (-1,-1) (1,0))) -- Decrease
    , ((modMask .|. controlMask, xK_End),       withFocused (keysResizeWindow (1,1)   (1,0))) -- Increase
    -- Southeast:
    , ((modMask,                 xK_Page_Down), withFocused (keysMoveWindow   (1,1)))         -- Move
    , ((modMask .|. shiftMask,   xK_Page_Down), withFocused (keysResizeWindow (-1,-1) (0,0))) -- Decrease
    , ((modMask .|. controlMask, xK_Page_Down), withFocused (keysResizeWindow (1,1)   (0,0))) -- Increase
    ]

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

-- Layouts (Tall, Mirror Tall, Full, Grid, Spiral)
myLayoutHook = smartBorders
             $ mkToggle (NOBORDERS ?? FULL ?? EOT)
             $ tiled
           ||| Mirror tiled
           ||| Full
           ||| Grid
           ||| spiral (6/7)
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        delta   = 1/100
        ratio   = 1/2

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
        , layoutHook         = avoidStruts $ myLayoutHook
        , startupHook        = myStartupHook
        , modMask            = mod4Mask
        , terminal           = "urxvtc" -- Requires "urxvtd -q -o -f &" in ~/.xinitrc or ~/.xsession
        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        }
