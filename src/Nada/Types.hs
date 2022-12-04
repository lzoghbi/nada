{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Nada.Types where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text
import qualified Brick.Widgets.Edit as Ed
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Data.Time (Day)
import qualified Data.Map as Map

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

data TodoList = TodoList
  { _todoListName :: Text
  -- _todoList are the IDs of the Todos in the list
  , _todoList :: [Integer]
  }
  deriving (Show)

data NadaState = NadaState
  { _visibleTodoLists :: [TodoList]
  , _todosMap :: Map.Map Integer Todo
  , _nextAvailableId :: Integer
  -- _selecterItem == (l, t) indicates that the current selection is
  -- the t-th item of the l-th list
  , _selectedItem :: (Integer, Integer)
  , _mode :: NadaMode
  -- , _filterText :: Text
  }
  deriving (Show)

makeLenses ''Todo
makeLenses ''TodoList
makeLenses ''NadaState

resourceNameToInteger :: Name -> Integer
resourceNameToInteger (TodoId n) = n
resourceNameToInteger (EditorId n) = n
resourceNameToInteger NadaVP = error "NadaVp does not have an integer id"

-- Create a default Todo with a given ID
defaultTodo :: Integer -> Todo
defaultTodo intId = Todo {
  _todoName = Ed.editorText (EditorId intId) (Just 1) ""
, _todoDescription = ""
, _todoCompleted = False
, _todoId = TodoId intId
, _todoDueDate = Nothing
, _todoPriority = Medium
, _todoTags = []
}

defaultTodoList :: TodoList
defaultTodoList = TodoList {
  _todoListName = ""
, _todoList = []
}

defaultNadaState :: NadaState
defaultNadaState = NadaState {
  -- Always have atleast one TodoList
  _visibleTodoLists = [defaultTodoList]
, _todosMap = Map.empty
, _nextAvailableId = 0
, _selectedItem = (0, 0)
, _mode = Normal
}

-- Set the ID of a Todo, updating also the ID of the Editor inside
setTodoId :: Integer -> Todo -> Todo
setTodoId newId td = td & todoId .~ TodoId newId
                        & todoName .~ updatedEditor
  where
    oldEditor = td ^. todoName
    updatedEditor = Ed.editor (EditorId newId) (Just 1) (Data.Text.unlines (Ed.getEditContents oldEditor))

-- Add a Todo to the map of Todos, handling the updating of the indices
-- Returns the updated NadaState and the assigned Index
addTodoToState :: NadaState -> Todo -> (NadaState, Integer)
addTodoToState st td = (newSt, thisId)
  where
    newSt = st & nextAvailableId .~ thisId + 1
               & todosMap %~ Map.insert thisId updatedTodo
    updatedTodo = setTodoId thisId td
    thisId = st ^. nextAvailableId

-- Add a list of Todos to the state, and return the list of IDs they get
-- assigned
addTodosToState :: NadaState -> [Todo] -> (NadaState, [Integer])
addTodosToState st [] = (st, [])
addTodosToState st (td:tds) = (st', nextId:ids)
  where
    (nextSt, nextId) = addTodoToState st td
    (st', ids) = addTodosToState nextSt tds

getTodosFromIdxList :: NadaState -> [Integer] -> [Todo]
getTodosFromIdxList st idxs = Prelude.map ((st^.todosMap) Map.!) idxs

-- Get the actual list of Todos for the List at the specified index
getActualTodoList :: NadaState -> Int -> [Todo]
getActualTodoList st i = getTodosFromIdxList st (st^.(visibleTodoLists.ix i.todoList))

-- testNadaState :: NadaState
-- testNadaState = NadaState { _todoList = Seq.fromList $ [todo1, todo2]
--                           , _selectedTodo = -1
--                           , _mode = Normal
--                           , _filterText = ""
--                           }
--  where
--   todo1 = Todo
--             { _todoName = Ed.editorText (EditorId 100) Nothing "test1"
--             , _todoDescription = "description 1"
--             , _todoCompleted = True
--             , _todoId = TodoId 100
--             , _todoDueDate = Nothing
--             , _todoPriority = High
--             , _todoTags = []
--             }
--   todo2 = Todo
--             { _todoName = Ed.editorText (EditorId 200) Nothing "test2"
--             , _todoDescription = "description 2"
--             , _todoCompleted = False
--             , _todoId = TodoId 200
--             , _todoDueDate = Nothing
--             , _todoPriority = High
--             , _todoTags = []
--             }
