import           System.IO
import           XMonad
import           XMonad.Actions.SpawnOn
import           XMonad.Config.Xfce
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig     (additionalKeys)
import           XMonad.Util.Run          (spawnPipe)

import qualified Data.Map                 as M
import qualified XMonad.StackSet          as W

myWorkspaces = ["1:web", "2:dev", "3:shell", "4:slack"] ++ map show [5..9]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- launch terminal
    ((modMask,               xK_Return), spawn $ XMonad.terminal conf)

    -- Screen lock
  , ((modMask .|. shiftMask, xK_z), spawn "xflock4")

    -- Logout
  , ((modMask .|. shiftMask, xK_q), spawn "xfce4-session-logout")

    -- Launcher
  , ((modMask,               xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
  , ((modMask .|. shiftMask, xK_p), spawn "xfce4-appfinder")

    -- Close focused window
  , ((modMask .|. shiftMask, xK_c), kill)

    -- Rotate through layout algorithms
  , ((modMask,               xK_space), sendMessage NextLayout)

    -- Reset window layout
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Reset window sizing
  , ((modMask,               xK_n), refresh)

    -- Move focus to the next window
  , ((modMask,               xK_Tab), windows W.focusDown)

    -- Move focus to the previous window
  , ((modMask,               xK_k), windows W.focusUp)

    -- Move focus to the master window
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster )

    -- Swap the focused window and the master window
  , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

    -- Swap the focused window with the previous window
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

    -- Shrink the master area
  , ((modMask,               xK_h), sendMessage Shrink)

    -- Expand the master area
  , ((modMask,               xK_l), sendMessage Expand)

    -- Push window back into tiling
  , ((modMask,               xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
  , ((modMask,               xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
  , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- Restart xmonad
  , ((modMask,               xK_q), restart "xmonad" True)
  ]

  ++

  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  ++

  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myStartupHook = do
  spawnOn "1:web" "firefox"
  spawnOn "2:dev" "emacs"
  spawnOn "3:shell" "xfce4-terminal"
  spawnOn "4:slack" "slack"

main = do
    xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"

    xmonad $ xfceConfig
        { manageHook = manageSpawn <+> manageDocks <+> manageHook xfceConfig
        , layoutHook = avoidStruts  $  layoutHook xfceConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#1abc9c" "" . wrap "[" "]"
                        , ppTitle = xmobarColor "blue" "" . shorten 50
                        }
	, terminal = "xfce4-terminal"
        , workspaces = myWorkspaces
        , startupHook = startupHook xfceConfig <+> myStartupHook
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , keys = myKeys
        }
