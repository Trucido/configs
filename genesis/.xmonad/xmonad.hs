-- woddfellow2's xmonad Config
-- by woddfellow2 | http://wlair.us.to/

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
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook 
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window

-- Workspaces and what to put in them
myWorkspaces = ["1:term","2:www","3:vi","4:media","5:other"]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? a --> viewShift "1:term"      | a <- myClassTermShifts  ]
    , [ className =? b --> doF (W.shift "2:www")   | b <- myClassWWWShifts   ]
    , [ className =? c --> viewShift "3:vi"        | c <- myClassgVimShifts  ]
    , [ className =? d --> viewShift "4:media"     | d <- myClassMediaShifts ]
    , [ className =? e --> doF (W.shift "5:other") | e <- myClassOtherShifts ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassTermShifts  = ["URxvt","XTerm"]
        myClassWWWShifts   = ["Xombrero"]
        myClassgVimShifts  = ["Gvim"]
        myClassMediaShifts = ["feh","MPlayer"]
        myClassOtherShifts = ["XConsole","XClock","XLoad","XBiff"]

-- Keybindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Launching
    [ ((modMask .|. shiftMask, xK_Return),     spawn $ XMonad.terminal conf)
    , ((modMask,               xK_p),          spawn "dmenu_run -fn -misc-fixed-medium-r-*-*-12-*-*-*-*-*-*-*")
    , ((modMask .|. shiftMask, xK_c),          kill)

    -- Layout
    , ((modMask,               xK_space),      sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space),      setLayout $ XMonad.layoutHook conf)

    -- Redraw
    , ((modMask,               xK_n),          refresh)

    -- Focus
    , ((modMask,               xK_j),          windows W.focusDown)
    , ((modMask,               xK_k),          windows W.focusUp)
    , ((modMask,               xK_m),          windows W.focusMaster)

    -- Moving and Resizing
    , ((modMask,               xK_Return),     windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j),          windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k),          windows W.swapUp)
    , ((modMask,               xK_h),          sendMessage Shrink)
    , ((modMask,               xK_l),          sendMessage Expand)
    , ((modMask,               xK_t),          withFocused $ windows . W.sink)
    , ((modMask,               xK_comma),      sendMessage (IncMasterN 1))
    , ((modMask,               xK_period),     sendMessage (IncMasterN (-1)))

    -- Leave
    , ((modMask .|. shiftMask, xK_q),          io (exitWith ExitSuccess))
    , ((modMask,               xK_q),          spawn "xmonad --recompile; xmonad --restart")

    -- Custom Keybindings
    , ((modMask,               xK_Escape),     spawn "xscreensaver-command -lock")
    , ((modMask,               xK_x),          banishScreen LowerRight)
    , ((modMask,               xK_grave),      focusUrgent)
    , ((modMask,               xK_g),          goToSelected defaultGSConfig)
    , ((modMask .|. shiftMask, xK_apostrophe), windowPromptGoto defaultXPConfig)
    , ((modMask .|. shiftMask, xK_period),     sshPrompt defaultXPConfig)
    ]
    ++

    -- Workspace Keybindings
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
        , layoutHook         = avoidStruts $ layoutHook defaultConfig
        , modMask            = mod4Mask
        , terminal           = "urxvtc"
        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , borderWidth        = 1
        , normalBorderColor  = "#DDDDDD"
        , focusedBorderColor = "#FF0000"
        }
