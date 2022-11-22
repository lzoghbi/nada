{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Lens.Micro.TH (makeLensesFor)

data Name = TodoId Integer
          | EditorId Integer
          | NadaVP
  deriving (Eq, Show, Ord)

data NadaMode = Normal | Edit
  deriving (Eq, Show)

data Todo = Todo 
  { _todoName :: Ed.Editor Text Name
  , _todoDescription :: Text
  , _todoCompleted :: Bool
  , _todoId :: Name
  }

-- makeLensesFor [("todoName", "_todoName")] ''Todo

data NadaState = NadaState
  { _todoList :: Seq Todo
  , _selectedTodo :: Integer
  , _mode :: NadaMode
  }

testNadaState :: NadaState
testNadaState = NadaState { _todoList = Seq.fromList $ [todo1, todo2]
                          , _selectedTodo = 0
                          , _mode = Normal
                          }
 where
  todo1 = Todo
            { _todoName = Ed.editorText (EditorId 11) Nothing "test1"
            , _todoDescription = "description 1"
            , _todoCompleted = True
            , _todoId = TodoId 11
            }
  todo2 = Todo
            { _todoName = Ed.editorText (EditorId 12) Nothing "test2"
            , _todoDescription = "description 2"
            , _todoCompleted = False
            , _todoId = TodoId 12
            }
