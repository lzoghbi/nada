{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types


import Brick
import Brick.Main
import Brick.Types
import qualified Brick.Types as T

import Control.Monad.State
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable (toList)
import qualified Graphics.Vty as V

shortcutInfoBar :: Widget NadaId
shortcutInfoBar = T.Widget T.Fixed T.Fixed $ do
  c <- T.getContext
  let h = T.availHeight c
  T.render $ translateBy (T.Location (0, h-1)) 
    -- $ clickable (NadaId 0)
    $ txt "[Ctrl-C] - Quit"


-- [x] task 1
--       description 1
drawTodo :: Todo -> Widget NadaId
drawTodo Todo{..} = 
  padRight (Pad 1) (drawCompleted todoCompleted) 
  <+> txt todoName
  <=> drawDescription
  where
    drawCompleted True = clickable todoId $ txt "[X]"
    drawCompleted False = clickable todoId $ txt "[ ]"
    drawDescription = padLeft (Pad 6) $ txt todoDescription

nadaAppDraw :: NadaState -> [Widget NadaId]
nadaAppDraw (NadaState nadaState) =
  [ vBox $ toList $ drawTodo <$> nadaState
  , shortcutInfoBar
  ]

appEvent :: BrickEvent NadaId e -> EventM NadaId NadaState ()
appEvent (MouseDown clickedId _ _ _) = do
    NadaState nadaState <- get
    let todoIndex = Seq.findIndexL (\Todo{..} -> clickedId == todoId) nadaState
    case todoIndex of
      Nothing  -> return ()
      Just i   -> do
                    let newState = Seq.adjust' (\t@Todo{..} -> t{todoCompleted = not todoCompleted}) 
                                              i
                                              nadaState
                    put (NadaState newState)

appEvent (VtyEvent vtyE) = case vtyE of
  -- V.EvKey (V.KChar) [V.Modifiers] -> do sth
  V.EvKey (V.KChar 'c') [V.MCtrl] -> do 
    NadaState nadaState <- get
    halt
  _ -> return ()
appEvent _ = return ()

nadaApp :: App NadaState e NadaId
nadaApp = App
  { appDraw = nadaAppDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = do
                      vty <- getVtyHandle
                      liftIO $ V.setMode (V.outputIface vty) V.Mouse True
  , appAttrMap = const $ attrMap V.defAttr []
  }
