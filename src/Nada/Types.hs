{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Nada.Types where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text
import qualified Brick.Widgets.Edit as Ed
import Lens.Micro.TH (makeLenses)
import Data.Time (Day)

data Name = TodoId Integer
          | EditorId Integer
          | NadaVP
  deriving (Eq, Show, Ord)

data NadaPriority = High 
                  | Medium 
                  | Low 
   deriving (Eq, Show)

data NadaMode = Normal | Edit
  deriving (Eq, Show)

data Todo = Todo 
  { _todoName :: Ed.Editor Text Name
  , _todoDescription :: Text
  , _todoCompleted :: Bool
  , _todoId :: Name
  , _todoDueDate :: Maybe Day
  , _todoPriority :: NadaPriority
  , _todoTags :: [Text]
  }
  deriving (Show)

data NadaState = NadaState
  { _todoList :: Seq Todo
  , _selectedTodo :: Integer
  , _mode :: NadaMode
  , _filterText :: Text
  }
  deriving (Show)

makeLenses ''Todo
makeLenses ''NadaState

testNadaState :: NadaState
testNadaState = NadaState { _todoList = Seq.fromList $ [todo1, todo2]
                          , _selectedTodo = -1
                          , _mode = Normal
                          , _filterText = ""
                          }
 where
  todo1 = Todo
            { _todoName = Ed.editorText (EditorId 100) Nothing "test1"
            , _todoDescription = "description 1"
            , _todoCompleted = True
            , _todoId = TodoId 100
            , _todoDueDate = Nothing
            , _todoPriority = High
            , _todoTags = []
            }
  todo2 = Todo
            { _todoName = Ed.editorText (EditorId 200) Nothing "test2"
            , _todoDescription = "description 2"
            , _todoCompleted = False
            , _todoId = TodoId 200
            , _todoDueDate = Nothing
            , _todoPriority = High
            , _todoTags = []
            }
