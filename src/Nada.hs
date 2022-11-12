{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada
    ( nadaApp
    , nadaDefaultState
    , nadaWidget
    ) where

import Brick
import Brick.Types (EventM)
import Data.Sequence (Seq(..), adjust')
import qualified Data.Sequence as Seq
import Data.Text (Text(..))
import qualified Data.Text as Text
import Data.Foldable (toList)
import Control.Monad.State
import qualified Graphics.Vty as V

data Todo = Todo 
  { todoName :: Text
  , todoDescription :: Text
  , todoCompleted :: Bool
  , todoId :: NadaId
  }

type NadaState = Seq Todo
type NadaId = Integer

nadaDefaultState :: NadaState
nadaDefaultState = Seq.fromList $ [todo1, todo2]
 where
  todo1 = Todo
            { todoName = "test1"
            , todoDescription = "description 1"
            , todoCompleted = True
            , todoId = 1
            }
  todo2 = Todo
            { todoName = "test2"
            , todoDescription = "description 2"
            , todoCompleted = False
            , todoId = 2
            }

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
nadaAppDraw nadaState = [vBox $ toList $ drawTodo <$> nadaState]

appEvent :: BrickEvent NadaId e -> EventM NadaId NadaState ()
appEvent ev@(MouseDown clickedId _ _ _) = do
    state <- get
    let todoIndex = Seq.findIndexL (\Todo{..} -> clickedId == todoId) state
    case todoIndex of
      Nothing -> return ()
      Just id -> do
                   let newState = adjust' (\t@Todo{..} -> t{todoCompleted = not todoCompleted}) 
                                          id 
                                          state
                   put newState
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

nadaWidget :: Widget Integer
nadaWidget = vBox $ nadaAppDraw nadaDefaultState
