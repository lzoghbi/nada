{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Main (main) where

import qualified Brick
import qualified Brick.Widgets.Edit as Ed
import Nada.Types (NadaState(..), Todo(..), Name(..), NadaPriority(..))
import Nada.App
import Nada.Org
import Data.Text (Text, unpack)
import qualified Data.Text.IO as Text
import qualified Data.Org as O
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
-- For getting the error message when parsing the org file
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Time (Day, parseTimeM, defaultTimeLocale)

data Command
  = Add Text (Maybe Text) (Maybe Text) (Maybe Text)
  | Edit

commandParser :: Parser Command
commandParser = subparser
  (  command "add" (info addCommand (progDesc "Add a todo to the list"))
  <> command "edit" (info editCommand (progDesc "Open a TUI to edit the list"))
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
          (  metavar "DESC"
          <> value Nothing
          <> help "(Optional) Description for the todo"
          )             
      <**> helper
    editCommand = pure Edit <**> helper

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
    Right orgFile -> return (orgFileToNada orgFile)
    Left parseError -> do
      putStrLn "Encountered an error trying to parse the org file:"
      putStrLn (errorBundlePretty parseError)
      exitFailure

edit :: FilePath -> IO ()
edit filePath = do
  nadaState <- openNadaFile filePath
  finalNadaState <- Brick.defaultMain nadaApp nadaState
  Text.writeFile filePath (O.prettyOrgFile $ nadaToOrgFile finalNadaState)
  exitSuccess

toNadaDedline :: Maybe Text -> Maybe Day
toNadaDedline (Just d) = parseTimeM True defaultTimeLocale "%Y-%-d-%-m" $ unpack d
toNadaDedline _        = Nothing

toNadaPriority :: Maybe Text -> NadaPriority
toNadaPriority (Just p) = case p of
                            "high"   -> High
                            "medium" -> Medium
                            "low"    -> Low
toNadaPriority _        = Medium

add :: FilePath -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO ()
add filePath todo date todoPrio todoDesc = do
  nadaState <- openNadaFile filePath
  -- FIXME: Write a helper function for this
  let newIntId = toInteger $ Seq.length (_todoList nadaState) + 1
  let newTodo = Todo
              { _todoName = Ed.editorText (EditorId newIntId) Nothing todo
              , _todoDescription = fromMaybe "" todoDesc
              , _todoCompleted = False
              -- FIXME: Use proper id generation
              , _todoId = TodoId newIntId
              , _todoDueDate = toNadaDedline date
              , _todoPriority = toNadaPriority todoPrio
              }
      finalNadaState = nadaState{ _todoList = _todoList nadaState Seq.:|> newTodo }
  Text.writeFile filePath (O.prettyOrgFile $ nadaToOrgFile finalNadaState)
  exitSuccess

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
    (Just nadaFile, Edit) -> edit nadaFile
    (Just nadaFile, Add todo date todoPrio todoDesc) -> add nadaFile todo date todoPrio todoDesc 
