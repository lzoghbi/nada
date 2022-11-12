{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.Types
  ( NadaState(..)
  , NadaId(..)
  , Todo(..)
  , testNadaState
  ) where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text(..))

data Todo = Todo 
  { todoName :: Text
  , todoDescription :: Text
  , todoCompleted :: Bool
  , todoId :: NadaId
  }

newtype NadaState = NadaState (Seq Todo)
newtype NadaId = NadaId Integer
  deriving (Eq, Ord, Show)

testNadaState :: NadaState
testNadaState = NadaState $ Seq.fromList $ [todo1, todo2]
 where
  todo1 = Todo
            { todoName = "test1"
            , todoDescription = "description 1"
            , todoCompleted = True
            , todoId = NadaId 1
            }
  todo2 = Todo
            { todoName = "test2"
            , todoDescription = "description 2"
            , todoCompleted = False
            , todoId = NadaId 2
            }
