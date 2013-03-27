{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Main inspiration: http://github.com/mortenbp/config
import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Config.Gnome(gnomeConfig)

----- Misc
import Data.Monoid(mempty, mappend)
import Data.List((\\))
import Data.Ratio((%))
import Control.Monad(when, void)
import Control.Concurrent(threadDelay)
import System.Directory
import System.Locale
import System.Time
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
import XMonad.Actions.WithAll
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowGo (ifWindows)

----- Hooks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus

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
import XMonad.Util.Scratchpad

main :: IO ()
main = do
  checkTopicConfig myTopics myTopicConfig
  mapM_ spawn startup
  xmonad myConfig

startupCond :: [(String, String)]
startupCond = [ ("pidgin","Pidgin")
              , ("xchat","Xchat")
              --, ("optirun steam steam://open/console","Steam")
              ]

startup :: [String]
startup = ["xcompmgr", "dropbox.py start"]

myConfig = withUrgencyHook LibNotifyUrgencyHook $ gnomeConfig
   { manageHook         = manageHook gnomeConfig <+>
                          composeAll myManageHook <+>
                          scratchpadManageHook (W.RationalRect 0.2 0.2 0.6 0.6)
   , layoutHook         = smartBorders $ setWorkspaceDirs myLayout
   , terminal           = "terminator"
   , modMask            = mod4Mask
   , normalBorderColor  = "#555555"
   , focusedBorderColor = "#bbbbbb"
   , focusFollowsMouse  = False
   , handleEventHook    = myEventHook
   , startupHook        = do return ()
                             checkKeymap myConfig myKeys
                             mapM_ (uncurry spawnUnlessExists) startupCond
                             startupHook gnomeConfig
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
           Full ||| GridRatio (4/3) ||| Mirror (Tall 1 (3/100) (1/2))

myEventHook = onRescreen customRescreen `mappend` deleteUnimportant (=~ "^(scratchpad|vm|monitor)-") callback
  where callback dead = withDir $ \tag dir -> if (tag =~ "^scratchpad-" && dir =~ ("^" ++ myScratchpadDir)) then
                                                io $ deleteIfEmpty dir
                                              else
                                                return ()
        deleteIfEmpty dir = do contents <- getDirectoryContents dir
                               if length (contents \\ [".", ".."]) == 0 then
                                 removeDirectory dir
                               else
                                 return ()
                            `catch` \e -> return ()

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
  , "bachelor"
  , "pwnies"
  , "steam"
  , "organise"
  , "mail"
  , "multimedia"
  , "wireshark"
  , "virtualbox"
  , "download"
  , "xmonad"
  , "config"
  , "ida"
  , "ping"
  ]

setWorkspaceDirs layout =
    add "organise" "~/notes"                   $
    add "pwnies"   "~/pwnies"                  $
    add "download" "~/Downloads"               $
    add "android"  "~/kode/android"            $
    add "xmonad"   "~/.xmonad"                 $
    add "config"   "~/git/rcfiles"             $
    workspaceDir "~" layout
  where add ws dir = onWorkspace ws (workspaceDir dir layout)

myManageHook :: [ManageHook]
myManageHook = [ className =? "Xchat"      --> doShift "irc"
               , className =? "Pidgin"     --> doShift "im"
               , className =? "Steam"      --> doShift "steam"
               , isFullscreen              --> doFullFloat
               , className =? "VirtualBox" --> do name <- title
                                                  case (name =~ "( \\(.*\\))?( \\[[^\\]]+\\])? - Oracle VM VirtualBox$") :: (String,String,String) of
                                                    (_,"",_) -> return mempty
                                                    (n,_,_)  -> do let ws = "vm-" ++ n
                                                                   liftX $ addHiddenWorkspace ws
                                                                   doShift ws
               , resource =? "xfce4-notifyd" --> doIgnore <+> doF W.focusDown
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
      , ("steam", spawn "optirun steam steam://open/console")
      , ("organise", appBrowser ["https://calendar.google.com"])
      , ("mail", appBrowser ["https://gmail.com"])
      , ("virtualbox", spawn "virtualbox")
      , ("xmonad", shell)
      , ("config", shell)
      , ("wireshark", spawn "wireshark")
      , ("ida", spawn "ida 1600x900")
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
  , ("M-S-p", spawn "pkill gnome-panel || gnome-panel")
  , ("M-p", shellPrompt myXPConfig)
  , ("M-b", browser [])
  , ("M-S-b", incogBrowser [])
  --, ("M-l", spawn "i3lock")
  -- Dynamic workspaces
  , ("M-d", changeDir myXPConfig)
  , ("M-n", addWorkspacePrompt myXPConfig)
  , ("M-m", addWorkspaceMoveWindowPrompt myXPConfig)
  , ("M-<Backspace>", killAll >> myRemoveWorkspace)
  , ("M-r", renameWorkspace myXPConfig)
  , ("M-s", do dir <- liftIO $ formatCalendarTime defaultTimeLocale (myScratchpadDir ++ "/%Y-%m-%d-%H:%M:%S")  `fmap` (getClockTime >>= toCalendarTime)
               liftIO $ createDirectory dir
               newScratchpad
               changeDir_ dir
               shell)
  -- Workspace navigation
  , ("M-a", shiftToSelectedWS True myGSConfig)
  , ("M-<Right>", nextScreen)
  , ("M-<Left>", prevScreen)
  , ("M-S-<Right>", swapNextScreen)
  , ("M-S-<Left>", swapPrevScreen)
  -- Window swapping
  , ("M-S-h", windows W.swapDown)
  , ("M-S-l", windows W.swapUp)
  -- Scratchpad
  , ("M-S-<Space>", scratchpadSpawnActionCustom "gnome-terminal --disable-factory --name scratchpad")
  -- Global window
  , ("M-S-g", toggleGlobal)
  -- Notifications
  , ("M-8", spawn "notify-send \"$(wireless_notify)\"")
  , ("M-9", spawn "notify-send \"$(acpi)\"")
  , ("M-0", spawn "notify-send \"$(date)\"")
  ]

-- Remove workspace unless it's a topic
myRemoveWorkspace :: X ()
myRemoveWorkspace = do
  s <- gets windowset
  case s of
    W.StackSet {W.current = W.Screen { W.workspace = W.Workspace { W.tag = this } } } ->
      when (this `notElem` myTopics) removeWorkspace

myScratchpadDir = "/home/kris/scratchpads"

--deleteEmptyScratchpad :: String -> X ()
--deleteEmptyScratchpad = do dir <- getCurrentDirectory
--                           contents <- getDirectoryContents dir
--                           safeSpawn "xmessage" [show contents]

instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (W.tag ws) isFg
         -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")
