import XMonad
import System.Exit
import qualified XMonad.StackSet as W 
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout
import System.IO
import System.Posix.Unistd

statusBarCmd= "xmobar"
dmenuCmd= "dmenu_run -nb '#1a1a1a' -nf '#ffffff' -sb '#aecf96' -sf black -p '>'"


main = do din <- spawnPipe statusBarCmd
          sleep 2
          xmonad $ defaultConfig

	               { manageHook         = myManageHook <+> manageDocks <+> manageHook defaultConfig
                   , layoutHook         = smartBorders (myLayout)
                   , keys               = myKeys
	               , workspaces         = map show ["§"] ++ [1 .. 8 :: Int] ++ ["9-files", "0-torrent", "+-music", "|-IRC", "chat"]
                   , logHook            = dynamicLogWithPP $ myPP din
                   , modMask            = mod4Mask     -- Rebind Mod to the Windows key
                   , normalBorderColor  = "#555555"
                   , focusedBorderColor = "#bbbbbb"
	               , borderWidth 	    = 1
                   , startupHook        = (setWMName "LG3D" >> spawn "killall xbindkeys; xbindkeys") -- LG3D name is a fix for java
                   }

-- ManageHooks
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Pidgin" --> doShift "chat"
    , className =? "Quodlibet" --> doShift "music"
    , className =? "Transmission" --> doShift "torrent"
    , className =? "Nautilus" --> doShift "files"
    , className =? "Wine" --> doFloat
    , className =? "irssi" --> doShift "IRC"
    , isFullscreen --> doFullFloat]


-- DropNumbers removes the number if a workspace is named, i:name -> name
dropNumbers wsId =  if (':' `elem` wsId)
                    then drop 2 wsId
			        else wsId

-- Layouts
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled = Tall 1 (3/100) (1/2)
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

-- Pretty print for xmobar
myPP h = defaultPP
                { ppCurrent = xmobarColor "black" "#aecf96"
                , ppSep     = " | "
                , ppOutput  = hPutStrLn h
                }

-- Keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask .|. shiftMask, xK_c ), kill)

    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)

    -- floating layer support
    , ((modMask, xK_t ), withFocused $ windows . W.sink)
    , ((modMask, xK_b ), sendMessage ToggleStruts) -- Removes top bar

    -- swapping
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )

    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    --, ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    --, ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)

    -- screenshot screen
    , ((modMask,               xK_Print     ), spawn "/usr/bin/screenshot scr")

    -- screenshot window or area
    , ((modMask .|. shiftMask, xK_Print     ), spawn "/usr/bin/screenshot win")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        -- 65105 is the key after the plus key on my keyboard
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_onehalf] ++ [xK_1 .. xK_9] ++ [xK_0, xK_plus, 65105, xK_BackSpace])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
