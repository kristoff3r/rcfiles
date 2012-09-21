module XMonad.Util.ScratchpadExtra where

import XMonad
import XMonad.StackSet as W
import XMonad.Actions.DynamicWorkspaces (addWorkspace)
import XMonad.Layout.WorkspaceDirAlt

import Control.Monad (when)
import Data.Maybe (isJust, catMaybes)
import Data.List (isPrefixOf, (\\))

-- Create a new workspace named "scratchpadX"
newScratchpad :: X ()
newScratchpad = withWindowSet $ \w -> do
  let wss = W.workspaces w
      cws = map W.tag $ Prelude.filter
            (\ws -> sprefix `isPrefixOf`
                    W.tag ws && isJust (W.stack ws)) wss
      num = head $ ([0..] :: [Int]) \\
            catMaybes (map (readMaybe . drop (length sprefix)) cws)
      new = sprefix ++ show num
  when (new `notElem` (map W.tag wss)) $ addWorkspace new
  windows $ W.view new
 where sprefix = "scratchpad-"
       readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing
