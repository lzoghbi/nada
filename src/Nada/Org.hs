{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nada.Org
  ( -- * Reading
    orgFileToNada
    -- * Writing
  , nadaToOrgFile
  ) where

import Nada.Types

import Data.Foldable (find, toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Org as O
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, dayOfWeek)

import Brick.Widgets.Edit as Ed

-- | Converts an 'OrgFile' to a 'NadaState'
--
-- Nada files are a subset of valid org files. Right now we ignore anything that
-- isn't relevant to Nada. In the future it might be nice to add a warning or
-- error for what we aren't parsing.
--
-- This conversion is hacky right now.
orgFileToNada :: O.OrgFile -> NadaState
-- FIXME: We ignore O.docBlocks entirely and silently ignore any failures to
-- convert a section to a 'Todo' (represented as 'orgSectionToNadaTodo'
-- returning 'Nothing').
-- Reserved 0 - 9 for other clickable widgets.
orgFileToNada org = NadaState{..}
  where
    orgDoc = O.orgDoc org
    _todoList = Seq.fromList . catMaybes $ orgSectionToNadaTodo <$> zip [10..] (O.docSections orgDoc)
    _selectedTodo = 0
    _mode = Normal  
    
orgSectionToNadaTodo :: (Integer, O.Section) -> Maybe Todo
orgSectionToNadaTodo (todoId, O.Section{..}) = do
  -- FIXME: We return 'Nothing' if the 'Section' is missing 'sectionTodo'.
  --
  -- In the future we might consider it valid to have a section without a todo
  -- checkbox.
  --
  -- Even if we demand that each section have a todo, we could change the return
  -- type to 'Either ParseError Todo' and create a new 'ParseError' datatype
  -- representing a parse error.  
  todo <- orgTodoToNadaCompleted <$> sectionTodo
  let name = orgWordsToText sectionHeading
      description = fromMaybe T.empty (findDescription sectionDoc)
      dueDate  = findDueDate sectionDeadline
      priority = orgPrioToNadaPrio sectionPriority      
  pure $ Todo
    { _todoName = Ed.editorText (EditorId todoId) (Just 1) name
    -- FIXME: We might want to change the 'Todo' datatype to have
    -- 'todoCompleted' be 'Maybe Text' instead of 'Text'. Right now we're
    -- converting the 'Nothing' case to the empty string, but they might be
    -- different.
    , _todoDescription = description
    , _todoCompleted = todo
    -- FIXME: We don't keep track of the largest 'todoId' we encounter
    -- throughout this creation process. If we want to add support for creating
    -- new todos we will need to be able to generate "fresh" ids.
    --
    -- One hack to get around this is to assign the ids generated from reading
    -- the org file negative values. Then we can just use positive values for
    -- ids created while running the application.
    --
    -- The more natural thing to do would be to just return the largest id from
    -- 'orgFileToNada'. Or eventually rework this function to make use of
    -- our function to create a new todo (once implemented).
    , _todoId = TodoId todoId
    , _todoDueDate  = dueDate
    , _todoPriority = priority
    }

findDueDate :: Maybe O.OrgDateTime -> Maybe Day
findDueDate (Just O.OrgDateTime{..}) = Just dateDay  
findDueDate _ = Nothing

findDescription :: O.OrgDoc -> Maybe Text
findDescription O.OrgDoc{..} = do
  -- FIXME: Right now we look for the first paragraph in the section and call
  -- that the description. We ignore everything else the section contains.
  -- Presumably we will eventually care about rendering the rest of the section.
  O.Paragraph descWords <- find isParagraph docBlocks
  pure (orgWordsToText descWords)

orgWordsToText :: NonEmpty O.Words -> Text
-- FIXME: This is a hack to convert Data.Org's 'Words' into a 'Text'.
-- Eventually if we support rendering italics, underlines, etc. we should
-- convert this properly.
orgWordsToText = T.intercalate " " . NE.toList . NE.map O.prettyWords 

isParagraph :: O.Block -> Bool
isParagraph (O.Paragraph _) = True
isParagraph _ = False

nadaCompletedToOrgTodo :: Bool -> O.Todo
nadaCompletedToOrgTodo False = O.TODO
nadaCompletedToOrgTodo True  = O.DONE

orgTodoToNadaCompleted :: O.Todo -> Bool
orgTodoToNadaCompleted O.TODO = False
orgTodoToNadaCompleted O.DONE = True

todoDeadline :: Maybe Day -> Maybe O.OrgDateTime
todoDeadline (Just day) = Just ( O.OrgDateTime
  { O.dateDay = day
  , O.dateDayOfWeek = dayOfWeek day
  , O.dateTime = Nothing
  , O.dateRepeat = Nothing
  , O.dateDelay = Nothing
  })
todoDeadline _ = Nothing

orgPrioToNadaPrio :: Maybe O.Priority -> NadaPriority
orgPrioToNadaPrio (Just O.Priority{..}) = case priority of
                                            "A" -> High
                                            "B" -> Medium
                                            "C" -> Low
orgPrioToNadaPrio _                     = Medium

nadaPrioToOrgPrio :: NadaPriority -> O.Priority
nadaPrioToOrgPrio High   = O.Priority { priority = "A" }
nadaPrioToOrgPrio Medium = O.Priority { priority = "B" }
nadaPrioToOrgPrio Low    = O.Priority { priority = "C" }

-- | Represents a 'Todo' as a 'Section'
--
-- A 'Todo' always has a checkbox component and heading. Inside of the section
-- we render the description as a paragraph of plaintext.
nadaTodoToOrgSection :: Todo -> O.Section
nadaTodoToOrgSection Todo{..} = O.Section
  { sectionTodo = Just (nadaCompletedToOrgTodo _todoCompleted)
  , sectionPriority = Just (nadaPrioToOrgPrio _todoPriority)
  -- FIXME: Eventually we may wish to support more than plaintext todos. We
  -- might do this by changing the type of 'todoName' to 'Data.Org.Words'.
  -- Use `unwords` instead of `unlines`, because the latter adds a new line and 
  -- which leads to errors in the section fields below (everythng after a new line
  -- is parsed as sectionDoc)
  , sectionHeading = (NE.singleton (O.Plain $ (T.unwords . Ed.getEditContents) _todoName))
  , sectionTags = []
  , sectionClosed = Nothing
  , sectionDeadline = todoDeadline _todoDueDate
  , sectionScheduled = Nothing
  , sectionTimestamp = Nothing
  , sectionProps = mempty
  -- FIXME: Eventually we will likely have a more complex section contents.
  , sectionDoc = O.emptyDoc{O.docBlocks = [O.Paragraph $ NE.singleton (O.Plain _todoDescription)]}
  }

nadaToOrgFile :: NadaState -> O.OrgFile
-- FIXME: Eventually we will likely do more than just render sections.
nadaToOrgFile (NadaState{..}) = O.emptyOrgFile{O.orgDoc = O.emptyDoc{O.docSections = sections}}
  where
    sections = toList (nadaTodoToOrgSection <$> _todoList)
