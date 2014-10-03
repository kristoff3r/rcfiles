{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Main inspiration: http://github.com/mortenbp/config
import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop(desktopConfig)

----- Misc
import Data.Monoid(mempty)
import Data.List((\\))
import Data.Ratio((%))
import Control.Monad(when, void)
import Text.Regex.PCRE((=~))

----- Own packages
import XMonad.Hooks.UrgencyExtra
import XMonad.Layout.TopicExtra
import XMonad.Layout.WorkspaceDirAlt
import XMonad.Util.ScratchpadExtra

----- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowGo (ifWindows)

----- Hooks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus (takeTopFocus)

----- Layout
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

----- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell

----- Util
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

main :: IO ()
main = do
  checkTopicConfig myTopics myTopicConfig
  mapM_ spawn startup
  xmonad myConfig

-- Only start these if they aren't running
startupCond :: [(String, String)]
startupCond = [ ("pidgin","Pidgin")
              --, ("optirun steam steam://open/console","Steam")
              ]

startup :: [String]
startup = ["xcompmgr", "dropbox start", "xmodmap ~/.xmodmap"]

myScratchFloat = (customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3))
myScratchPad = "gnome-terminal --disable-factory --title "

scratchpads =
    [ NS "scratch" (myScratchPad ++ "scratch")        (title =? "scratch") myScratchFloat
    , NS "ipy"     (myScratchPad ++ "ipy -e ipython") (title =? "ipy")         myScratchFloat
    ]

myConfig = withUrgencyHook LibNotifyUrgencyHook $ desktopConfig
   { manageHook         = manageHook desktopConfig <+>
                          composeAll myManageHook <+>
                          namedScratchpadManageHook scratchpads <+>
                          manageDocks
   , layoutHook         = smartBorders $ setWorkspaceDirs myLayout
   , terminal           = "exec terminator"
   , modMask            = mod4Mask
   , normalBorderColor  = "#555555"
   , focusedBorderColor = "#bbbbbb"
   , focusFollowsMouse  = False
   , handleEventHook    = myEventHook <+> docksEventHook
   , startupHook        = do return ()
                             checkKeymap myConfig myKeys
                             mapM_ (uncurry spawnUnlessExists) startupCond
                             startupHook desktopConfig
                             setWMName "LG3D"
   , logHook            = do fadeOutLogHook $ fadeIf isUnfocusedOnCurrentWS 0.8
                             takeTopFocus
   , borderWidth        = 0
   , workspaces         = myTopics
   }
   `removeKeysP` (["M-" ++ m ++ k | m <- ["", "S-"], k <- map show [1..9 :: Int]])
   `additionalKeysP` myKeys


myLayout = smartBorders . avoidStruts $
           onWorkspace "im"    (withIM (1%5) (Role "buddy_list") Grid) $
  --         onWorkspace "steam" (withIM (1%5) (Role "Friends") Grid) $
           GridRatio (4/3) ||| Full

myEventHook = onRescreen customRescreen

customRescreen :: X ()
customRescreen = do
    xinesc <- withDisplay getCleanedScreenInfo
    l <- asks (layoutHook . config)

    windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
        let visible'    = map W.workspace (v:vs)
            needed      = max 0 $ length xinesc - (length . filter (\w -> W.tag w =~ "^monitor-") $ visible')
            newTag      = take needed $ ["monitor-" ++ show n | n <- [0..]] \\ map W.tag (visible' ++ hs)
            new         = visible' ++ map (\t -> W.Workspace t l Nothing) newTag ++ hs
            (xs, ys)    = splitAt (length xinesc) $ new
            (a:as)      = zipWith3 W.Screen xs [0..] $ map SD xinesc
        in  ws { W.current = a
               , W.visible = as
               , W.hidden  = ys }

myTopics :: [String]
myTopics =
  [ "web"
  , "im"
  , "irc"
  , "pwnies"
  , "steam"
  , "organise"
  , "mail"
  , "music"
  , "wireshark"
  , "virtualbox"
  , "download"
  , "xmonad"
  , "ida"
  , "ida64"
  , "ping"
  ]

setWorkspaceDirs layout =
    add "organise" "~/notes"                   $
    add "music"    "~/musik"                   $
    add "pwnies"   "~/pwnies"                  $
    add "download" "~/Downloads"               $
    add "android"  "~/kode/android"            $
    add "xmonad"   "~/.xmonad"                 $
    workspaceDir "~" layout
  where add ws dir = onWorkspace ws (workspaceDir dir layout)

myManageHook :: [ManageHook]
myManageHook = [ className =? "Pidgin"       --> doShift "im"
               , className =? "Steam"        --> doShift "steam"
               , className =? "steam"        --> doShift "steam"
               , className =? "xchat"        --> doShift "irc"
               , isFullscreen                --> doFullFloat
               , className =? "VirtualBox"   --> do name <- title
                                                    case (name =~ "( \\(.*\\))?( \\[[^\\]]+\\])? - Oracle VM VirtualBox$") :: (String,String,String) of
                                                      (_,"",_) -> return mempty
                                                      (n,_,_)  -> do let ws = "vm-" ++ n
                                                                     liftX $ addHiddenWorkspace ws
                                                                     doShift ws
                -- desktop environment specific stuff
               , resource =? "xfce4-settings-manager" --> doFloat
               , resource =? "xfce4-notifyd" --> doIgnore <+> doF W.focusDown
               , resource =? "lxpanel" --> doF W.focusDown
               ]

myBrowser :: String
myBrowser = "google-chrome"

shell :: X ()
shell = spawn (terminal myConfig)

spawnUnlessExists :: String -> String -> X ()
spawnUnlessExists p t = ifWindows (className =? t) (void . return) (spawn p)

browser, incogBrowser, newBrowser, appBrowser :: [String] -> X ()
browser s       = safeSpawn myBrowser s
incogBrowser s  = safeSpawn myBrowser ("--new-window" : "--incognito" : s)
newBrowser s    = safeSpawn myBrowser ("--new-window" : s)
appBrowser      = mapM_ (\s -> safeSpawn myBrowser ["--app=" ++ s])

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { fgColor = "#a8a3f7"
  , bgColor = "#3f3c6d"
  , position = Top
  , promptBorderWidth = 0
  }

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig {gs_navigate = navNSearch}

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs = M.fromList []
  , topicActions = M.fromList
      [ ("web", browser [])
      , ("im", spawn "pidgin")
      , ("irc", spawn "xchat")
--      , ("irc", safeSpawn (terminal myConfig) ["-x", "ssh", "lolbox.pwnies.dk", "-t", "screen", "-U", "-dr", "irc"])
      , ("steam", spawn "steam")
      , ("organise", appBrowser ["https://calendar.google.com"])
      , ("mail", appBrowser ["https://gmail.com"])
      , ("virtualbox", spawn "virtualbox")
      , ("xmonad", shell)
      , ("wireshark", spawn "wireshark")
      , ("ida", spawn "ida 1600x900")
      , ("ida64", spawn "ida64")
      , ("ping", spawn $ (terminal myConfig) ++ "-f -e ping 8.8.8.8")
      ]
  , defaultTopicAction = const $ return ()
  , defaultTopic = "web"
  , maxTopicHistory = 10
  }

myKeys :: [(String, X ())]
myKeys =
  [ ("M-z", goToSelectedWS myTopicConfig True myGSConfig)
  , ("M-<", toggleWS)
  , ("M-g", goToSelected myGSConfig)
  , ("M-S-p", spawn "pkill lxpanel || lxpanel")
  , ("M-p", shellPrompt myXPConfig)
  , ("M-b", browser [])
  , ("M-S-b", incogBrowser [])
  -- Dynamic workspaces
  , ("M-d", changeDir myXPConfig)
  , ("M-n", addWorkspacePrompt myXPConfig)
  , ("M-m", addWorkspaceMoveWindowPrompt myXPConfig)
  , ("M-<Backspace>", killAll >> myRemoveWorkspace)
  , ("M-r", renameWorkspace myXPConfig)
  -- Workspace navigation
  , ("M-a", shiftToSelectedWS True myGSConfig)
  , ("M-<Right>", nextScreen)
  , ("M-<Left>", prevScreen)
  , ("M-S-<Right>", swapNextScreen)
  , ("M-S-<Left>", swapPrevScreen)
  -- Window swapping
  , ("M-S-h", windows W.swapDown)
  , ("M-S-l", windows W.swapUp)
  -- Scratchpads
  , ("M-S-<Space>", namedScratchpadAction scratchpads "scratch")
  , ("M-C-<Space>", namedScratchpadAction scratchpads "ipy")
  -- Global window
  , ("M-S-g", toggleGlobal)
  -- Audio keys
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  , ("<XF86AudioMute>", spawn "amixer sset Master toggle")
  ]

-- Remove workspace unless it's a topic
myRemoveWorkspace :: X ()
myRemoveWorkspace = do
  s <- gets windowset
  case s of
    W.StackSet {W.current = W.Screen { W.workspace = W.Workspace { W.tag = this } } } ->
      when (this `notElem` myTopics) removeWorkspace

instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (W.tag ws) isFg
        -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")
