{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Nada.Types where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Text
import Data.Time (Day, showGregorian)
import Data.Time.Clock.System (systemEpochDay)

import qualified Brick.Widgets.Edit as Ed

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH (makeLenses)

data Name
  = TodoId Integer
  | NadaVP
  | TodoEditor
  deriving (Eq, Show, Ord)

data NadaPriority
  = High
  | Medium
  | Low
  deriving (Eq, Show)

data NadaMode = ModeNormal | ModeEdit | ModeEditTag | ModeEditDeadline
  deriving (Eq, Show)

data Todo = Todo
  { _todoName :: Text
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
  , -- _todoList are the IDs of the Todos in the list
    _todoList :: Seq.Seq Integer
  , -- The index in the list of the currently selected Todo
    -- This is NOT the global ID of the Todo
    _selectedTodo :: Integer
  }
  deriving (Show)

data NadaState = NadaState
  { _visibleTodoLists :: [TodoList]
  , _todosMap :: Map.Map Integer Todo
  , _todoEditor :: Ed.Editor Text Name
  , _nextAvailableId :: Integer
  , _selectedTodoList :: Integer
  , _currentMode :: NadaMode
  -- , _filterText :: Text
  }
  deriving (Show)

makeLenses ''Todo
makeLenses ''TodoList
makeLenses ''NadaState

resourceNameToInteger :: Name -> Integer
resourceNameToInteger (TodoId n) = n

-- Create a default Todo with a given ID
defaultTodo :: Integer -> Todo
defaultTodo intId =
  Todo
    { _todoName = "new todo c:"
    , _todoDescription = ""
    , _todoCompleted = False
    , _todoId = TodoId intId
    , _todoDueDate = Nothing
    , _todoPriority = Medium
    , _todoTags = []
    }

defaultTodoList :: TodoList
defaultTodoList =
  TodoList
    { _todoListName = "Todos"
    , _todoList = Seq.empty
    , _selectedTodo = 0
    }

defaultNadaState :: NadaState
defaultNadaState =
  NadaState
    { -- Always have atleast one TodoList
      _visibleTodoLists = [defaultTodoList]
    , _todosMap = Map.empty
    , _todoEditor = Ed.editor TodoEditor (Just 1) ""
    , _nextAvailableId = 0
    , _selectedTodoList = 0
    , _currentMode = ModeNormal
    }

-- Set the ID of a Todo, updating also the ID of the Editor inside
setTodoId :: Integer -> Todo -> Todo
setTodoId newId td = td & todoId .~ TodoId newId

-- Add a Todo to the map of Todos, handling the updating of the indices
-- Returns the updated NadaState and the assigned Index
addTodoToState :: NadaState -> Todo -> (NadaState, Integer)
addTodoToState st td = (newSt, thisId)
  where
    newSt =
      st
        & nextAvailableId .~ thisId + 1
        & todosMap %~ Map.insert thisId updatedTodo
    updatedTodo = setTodoId thisId td
    thisId = st ^. nextAvailableId

-- Add a list of Todos to the state, and return the list of IDs they get
-- assigned
addTodosToState :: NadaState -> [Todo] -> (NadaState, [Integer])
addTodosToState st [] = (st, [])
addTodosToState st (td : tds) = (st', nextId : ids)
  where
    (nextSt, nextId) = addTodoToState st td
    (st', ids) = addTodosToState nextSt tds

getTodosFromIdxList :: NadaState -> [Integer] -> [Todo]
getTodosFromIdxList st idxs = Prelude.map ((st ^. todosMap) Map.!) idxs

-- Get the actual list of Todos for the List at the specified index
getActualTodoList :: NadaState -> Integer -> [Todo]
getActualTodoList st i = getTodosFromIdxList st $ toList (st ^. (visibleTodoLists . ix (fromInteger i) . todoList))

getAllTodoIds :: NadaState -> [Integer]
getAllTodoIds st = Map.keys (st ^. todosMap)

addTodoListToState :: TodoList -> NadaState -> NadaState
addTodoListToState tdl st = st & visibleTodoLists %~ (++ [tdl])

getSelectedTodoId :: NadaState -> Integer
getSelectedTodoId st = selTodoId
  where
    selTodoId = st ^?! (selTodoList . todoList . ix (fromInteger selTodoPos))
    selTodoPos = st ^?! selTodoList . selectedTodo
    selTodoList = visibleTodoLists . ix (fromInteger selTodoListPos)
    selTodoListPos = st ^. selectedTodoList

showDate :: Maybe Day -> [Text]
showDate (Just a) = [Data.Text.pack $ showGregorian a]
showDate Nothing  = [Data.Text.pack $ showGregorian systemEpochDay]



