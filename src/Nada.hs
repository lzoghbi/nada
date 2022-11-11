{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada
    ( nadaApp
    , nadaDefaultState
    , nadaWidget
    ) where

import Brick
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text(..))
import qualified Data.Text as Text
import Data.Foldable (toList)

data Todo = Todo 
  { todoName :: Text
  , todoDescription :: Text
  , todoCompleted :: Bool
  }

type NadaState = Seq Todo

nadaDefaultState :: NadaState
nadaDefaultState = Seq.fromList $ [todo1, todo2]
 where
  todo1 = Todo
            { todoName = "test1"
            , todoDescription = "description 1"
            , todoCompleted = True
            }
  todo2 = Todo
            { todoName = "test2"
            , todoDescription = "description 2"
            , todoCompleted = False
            }

-- [x] task 1
--       description 1
drawTodo :: Todo -> Widget n
drawTodo Todo{..} = 
  padRight (Pad 1) (drawCompleted todoCompleted) 
  <+> txt todoName
  <=> drawDescription
  where
    drawCompleted True = txt "[x]"
    drawCompleted False = txt "[ ]"
    drawDescription = padLeft (Pad 6) $ txt todoDescription

nadaAppDraw :: NadaState -> [Widget n]
nadaAppDraw nadaState = toList $ drawTodo <$> nadaState

nadaApp :: Ord n => App NadaState e n
nadaApp = App
  { appDraw = nadaAppDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = undefined
  , appStartEvent = undefined
  , appAttrMap = undefined
  }

nadaWidget :: Widget ()
nadaWidget = vBox $ nadaAppDraw nadaDefaultState
