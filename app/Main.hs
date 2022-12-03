{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified Brick
import qualified Brick.Widgets.Edit as Ed
import Nada.Types hiding (Edit)
import Nada.App
import Nada.Org
import Data.Text (Text, unpack, splitOn, isInfixOf)
import qualified Data.Text.IO as Text
import qualified Data.Org as O
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Lens.Micro

-- For getting the error message when parsing the org file
import Text.Megaparsec (parse, errorBundlePretty)


data Command
  = Add Text (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)
  | Edit Text
  | Complete Text

commandParser :: Parser Command
commandParser = subparser
  (  command "add" (info addCommand (progDesc "Add a todo to the list"))
  <> command "edit" (info editCommand (progDesc "Open a TUI to edit the list"))
  <> command "complete" (info completeCommand (progDesc "Mark a todo as complete"))
  )
  where
    addCommand = Add
      <$> strArgument
          (  metavar "TODO"
          <> help "Todo to add to the list"
          )
      <*> argument (Just <$> str)
          (  metavar "DUE DATE"
          <> value Nothing
          <> help "(Optional) Due date for the todo"
          )  
      <*> argument (Just <$> str)
          (  metavar "PRIORITY"
          <> value Nothing
          <> help "(Optional) Priority for the todo [high/medium/low]"
          ) 
      <*> argument (Just <$> str)
          (  metavar "TAG"
          <> value Nothing
          <> help "(Optional) One or more tags for the todo"
          )         
      <*> argument (Just <$> str)
          (  metavar "DESC"
          <> value Nothing
          <> help "(Optional) Description for the todo"
          )             
      <**> helper
    editCommand = Edit 
      <$> strOption
          (  long "query"
          <> short 'q'
          <> metavar "QUERY"
          <> value ""
          <> help "Query used to filter shown todos"
          )
      <**> helper
    completeCommand = Complete
      <$> strArgument
          (  metavar "TODO"
          <> help "Todo to complete"
          )
      <**> helper

argParser :: Parser (Maybe FilePath, Command)
argParser = (,)
  <$> option (Just <$> str)
      (  long "file"
      <> short 'f'
      <> metavar "FILE"
      <> value Nothing
      <> help "File containing a todo list; uses environmental variable NADA_FILE if not specified"
      )
  <*> commandParser

opts :: ParserInfo (Maybe FilePath, Command)
opts = info (argParser <**> helper)
  (  fullDesc
  <> progDesc "Edit a todo list"
  <> header "nada - A TUI-based todo list"
  )

openNadaFile :: FilePath -> IO NadaState
openNadaFile filePath = do
  rawOrgFile <- Text.readFile filePath
  -- We use the "raw" org file parsers because Data.Org does not expose a
  -- parser that gives information on why the parse failed.
  case parse O.orgFile filePath rawOrgFile of
    Right orgFile -> orgFileToNada orgFile
    Left parseError -> do
      putStrLn "Encountered an error trying to parse the org file:"
      putStrLn (errorBundlePretty parseError)
      exitFailure

edit :: FilePath -> Text -> IO ()
edit filePath filterText = do
  nadaState <- openNadaFile filePath
  let moreTodoList = defaultTodoList & todoListName .~ "More todos"
                                     & todoList .~ Seq.fromList (getAllTodoIds nadaState)
  let nadaState' = addTodoListToState moreTodoList nadaState
  -- Ignoring the filter text for now
  -- finalNadaState <- Brick.defaultMain nadaApp nadaState{_filterText = filterText}
  finalNadaState <- Brick.defaultMain nadaApp nadaState'
  Text.writeFile filePath (O.prettyOrgFile $ nadaToOrgFile finalNadaState)
  exitSuccess

toNadaDeadline :: Maybe Text -> Maybe Day
-- TODO: error handling when input is wrong - currently it returns `Nothing`
-- Dates need to have 2 digits for days/months
toNadaDeadline (Just d) = parseTimeM True defaultTimeLocale "%Y-%-d-%-m" $ unpack d
toNadaDeadline _        = Nothing

toNadaPriority :: Maybe Text -> NadaPriority
toNadaPriority (Just p) = case p of
                            "high"   -> High
                            "medium" -> Medium
                            "low"    -> Low
toNadaPriority _        = Medium

toNadaTags :: Maybe Text -> [Text]
toNadaTags Nothing  = []
toNadaTags (Just t) = splitOn "," t

add :: FilePath -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO ()
add _ _ _ _ _ _ = return ()
-- ---------------- RECOVER FROM HERE -----------------------
-- add filePath todo date todoPrio tags todoDesc = do
--   nadaState <- openNadaFile filePath
--   -- FIXME: Write a helper function for this
--   let newIntId = toInteger $ Seq.length (_todoList nadaState) + 1
--   let newTodo = Todo 
--               { _todoName = Ed.editorText (mkTodoEditor) Nothing todo
--               , _todoDescription = fromMaybe "" todoDesc
--               , _todoCompleted = False
--               -- FIXME: Use proper id generation
--               , _todoId = mkTodoId newIntId
--               , _todoDueDate  = toNadaDeadline date
--               , _todoPriority = toNadaPriority todoPrio
--               , _todoTags     = toNadaTags tags
--               }            
--       finalNadaState = nadaState{ _todoList = _todoList nadaState Seq.:|> newTodo } 
--   Text.writeFile filePath (O.prettyOrgFile $ nadaToOrgFile finalNadaState)
--   exitSuccess
-- -------------------- TO HERE ------------------------------

complete :: FilePath -> Text -> IO ()
complete _ _ = return ()
-- -------------------- RECOVER FROM HERE -------------------------
-- complete filePath queryText = do
--   nadaState <- openNadaFile filePath
--   case Seq.findIndicesL incompleteTodoMatchesQuery (_todoList nadaState) of
--     [] -> do
--       Text.putStrLn ("No todo matching '" <> queryText <> "' found.")
--       exitFailure
--     [todoIndex] -> do
--       let finalNadaState = nadaState{_todoList = Seq.adjust' completeTodo todoIndex (_todoList nadaState)}
--       Text.writeFile filePath (O.prettyOrgFile $ nadaToOrgFile finalNadaState)
--       exitSuccess
--     _ -> do
--       -- In the case of multiple indices, we don't know which to pick, so have
--       -- the user.
--       edit filePath queryText
--   where
--     incompleteTodoMatchesQuery Todo{..} = any (queryText `isInfixOf`) (Ed.getEditContents _todoName) && not _todoCompleted
--     completeTodo todo = todo{_todoCompleted = True}
-- ---------------------- TO HERE --------------------------------

main :: IO ()
main = do
  defaultNadaFile <- lookupEnv "NADA_FILE"
  (argNadaFile, comm) <- execParser opts
  let maybeNadaFile = argNadaFile <|> defaultNadaFile
  case (maybeNadaFile, comm) of
    (Nothing, _) -> do 
      putStrLn "No file specified, please specify a file using --file or the environmental variable NADA_FILE. For more information run"
      putStrLn "  nada --help"
      exitFailure
    (Just nadaFile, Edit filterText) -> edit nadaFile filterText
    (Just nadaFile, Add todo date todoPrio tags todoDesc) -> add nadaFile todo date todoPrio tags todoDesc 
    (Just nadaFile, Complete queryText) -> complete nadaFile queryText
