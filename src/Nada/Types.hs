{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.Types
  ( NadaState(..)
  , NadaMode(..)
  , NadaId(..)
  , Todo(..)
  , testNadaState
  ) where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text

newtype NadaId = NadaId Integer
  deriving (Eq, Ord, Show)

data NadaMode = Normal | Edit
  deriving (Eq, Show)

data Todo = Todo 
  { todoName :: Text
  , todoDescription :: Text
  , todoCompleted :: Bool
  , todoId :: NadaId
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
            { todoName = "test1"
            , todoDescription = "description 1"
            , todoCompleted = True
            , todoId = NadaId 11
            }
  todo2 = Todo
            { todoName = "test2"
            , todoDescription = "description 2"
            , todoCompleted = False
            , todoId = NadaId 12
            }
