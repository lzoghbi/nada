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
import Data.Time (Day)
import Nada.Calendar (CalendarState(..), WithCalendarName(..))

data NadaName = TodoId Integer
              | TodoEditor
              | NadaVP
  deriving (Eq, Show, Ord)

type Name = WithCalendarName NadaName

mkTodoId :: Integer -> Name
mkTodoId = OtherName . TodoId

mkTodoEditor :: Name
mkTodoEditor = OtherName TodoEditor

mkNadaVP :: Name
mkNadaVP = OtherName NadaVP

data NadaPriority = High 
                  | Medium 
                  | Low 
   deriving (Eq, Show)

data NadaMode = ModeNormal | ModeEdit | ModeEditTag | ModeEditDeadline | ModeCalendar
  deriving (Eq, Show)

data Todo = Todo
  { _todoName :: Text
  , _todoDescription :: Text
  , _todoCompleted :: Bool
  , _todoId :: Name
  , _todoDueDate :: Maybe Day
  , _todoPriority :: NadaPriority
  , _todoTags :: [Text]
  , _todoSubTasks :: [Todo]
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
  , _allTags :: [Text]
  -- , _filterText :: Text
  , _calendarState :: CalendarState Name
  }

makeLenses ''Todo
makeLenses ''TodoList
makeLenses ''NadaState

resourceNameToInteger :: Name -> Integer
resourceNameToInteger (OtherName (TodoId n)) = n

-- Create a default Todo with a given ID
defaultTodo :: Integer -> Todo
defaultTodo intId = Todo {
  _todoName = "todo"
, _todoDescription = ""
, _todoCompleted = False
, _todoId = mkTodoId intId
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

defaultNadaStateFromCalendarState :: CalendarState Name -> NadaState
defaultNadaStateFromCalendarState calendarState = NadaState {
  -- Always have atleast one TodoList
  _visibleTodoLists = [defaultTodoList]
, _todosMap = Map.empty
, _todoEditor = Ed.editor mkTodoEditor (Just 1) ""
, _nextAvailableId = 0
, _selectedTodoList = 0
, _currentMode = ModeNormal
, _calendarState = calendarState
, _allTags = []
}

-- Set the ID of a Todo, updating also the ID of the Editor inside
setTodoId :: Integer -> Todo -> Todo
setTodoId newId td = td & todoId .~ mkTodoId newId

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

getListIds :: [Todo] -> [Integer]
getListIds []     = []
-- getListIds (t:ts) = id : getListIds ts
--   where 
--     TodoId id = t ^. todoId

completedTodos :: NadaState -> [Todo]
completedTodos st = getCompleted (toList $ st ^. todosMap)

activeTodos :: NadaState -> [Todo]
activeTodos st = getActive (toList $ st ^. todosMap)

getActive :: [Todo] -> [Todo] 
getActive [] = []
getActive l = Prelude.filter isActive l

isActive :: Todo-> Bool
isActive todo = not (todo ^. todoCompleted)

getCompleted :: [Todo] -> [Todo] 
getCompleted [] = []
getCompleted l = Prelude.filter isCompleted l

isCompleted :: Todo-> Bool
isCompleted todo = todo ^. todoCompleted

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



