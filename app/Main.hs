{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Main (main) where

import qualified Brick
import Nada.Types (NadaState(..), Todo(..), NadaId(..))
import Nada.App
import Nada.Org
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Org as O
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
-- For getting the error message when parsing the org file
import Text.Megaparsec (parse, errorBundlePretty)

data Command
  = Add Text (Maybe Text)
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

add :: FilePath -> Text -> Maybe Text -> IO ()
add filePath todo todoDesc = do
  nadaState <- openNadaFile filePath
  -- FIXME: Write a helper function for this
  let newTodo = Todo
              { todoName = todo
              , todoDescription = fromMaybe "" todoDesc
              , todoCompleted = False
              -- FIXME: Use proper id generation
              , todoId = NadaId . toInteger $ Seq.length (todoList nadaState) + 1
              }
      finalNadaState = nadaState{ todoList = todoList nadaState Seq.:|> newTodo }
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
    (Just nadaFile, Add todo todoDesc) -> add nadaFile todo todoDesc
