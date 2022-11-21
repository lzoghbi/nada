{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.Types
  ( NadaState(..)
  , NadaMode(..)
  , Name(..)
  , Todo(..)
  , testNadaState
  ) where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text
import qualified Brick.Widgets.Edit as Ed

data Name = TodoId Integer
          | EditorId Integer
          | NadaVP
  deriving (Eq, Show, Ord)

data NadaMode = Normal | Edit
  deriving (Eq, Show)

data Todo = Todo 
  { todoName :: Ed.Editor Text Name
  , todoDescription :: Text
  , todoCompleted :: Bool
  , todoId :: Name
  }

-- newtype NadaState = NadaState (Seq Todo)
data NadaState = NadaState
  { todoList :: Seq Todo
  , selectedTodo :: Integer
  , mode :: NadaMode
  }

testNadaState :: NadaState
testNadaState = NadaState { todoList = Seq.fromList $ [todo1, todo2]
                          , selectedTodo = 0
                          , mode = Normal
                          }
 where
  todo1 = Todo
            { todoName = Ed.editorText (EditorId 11) Nothing "test1"
            , todoDescription = "description 1"
            , todoCompleted = True
            , todoId = TodoId 11
            }
  todo2 = Todo
            { todoName = Ed.editorText (EditorId 12) Nothing "test2"
            , todoDescription = "description 2"
            , todoCompleted = False
            , todoId = TodoId 12
            }
