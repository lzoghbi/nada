{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types
import Brick

import Control.Monad.State
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text(..))
import qualified Data.Text as Text
import Data.Foldable (toList)
import qualified Graphics.Vty as V

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
nadaAppDraw (NadaState nadaState) = [vBox $ toList $ drawTodo <$> nadaState]

appEvent :: BrickEvent NadaId e -> EventM NadaId NadaState ()
appEvent ev@(MouseDown clickedId _ _ _) = do
    NadaState state <- get
    let todoIndex = Seq.findIndexL (\Todo{..} -> clickedId == todoId) state
    case todoIndex of
      Nothing -> return ()
      Just id -> do
                   let newState = Seq.adjust' (\t@Todo{..} -> t{todoCompleted = not todoCompleted}) 
                                              id 
                                              state
                   put (NadaState newState)
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
