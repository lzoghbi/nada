module Nada.KeyBinds.IniParser
  (
    parseKeyBinds
  , parseKeyBindsFromFile
  ) where

import Nada.KeyBinds.KeyCodes (parseKeyCode)
import Data.Ini.Config
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty.Input.Events

parseField :: Text -> Text -> SectionParser (Text, [Event])
parseField separator identifier = do
  keyCodesMaybe <- fieldMbOf identifier (listWithSeparator separator string)
  -- Any malformed keycodes are ignored. I would prefer to raise an
  -- error but I don't see a way to do so using 'Data.Ini.Config'.
  -- The proper way to circumvent this would be to do your parsing in
  -- two stages.
  pure (identifier, maybe [] (mapMaybe (parseKeyCode . T.unpack)) keyCodesMaybe)

-- | Use this parser if you already have an INI file you're reading configs
-- from. Otherwise, try 'parseKeyBindsFromFile'.
--
-- Requires a 'sectionName' for the section in which the keybinds are contained,
-- a 'separator' to separate the keybinds on each line, and the list of
-- 'identifiers' that you allow keybinds for. It is up to you to enforce
-- that the 'identifiers' you provide have matching handlers in your
-- 'KeyBindings'.
parseKeyBinds :: Text -> Text -> [Text] -> IniParser (Maybe (Map Event Text))
parseKeyBinds sectionName separator identifiers = sectionMb sectionName $ do
  entries <- traverse (parseField separator) identifiers
  pure $ M.fromList
    -- We need to expand 'entries :: [(Text, [Event])]' to [(Event, Text)]
    -- in order to make it into a map.
    [(event, identifier) | (identifier, events) <- entries, event <- events]

parseKeyBindsFromFile :: Text -> Text -> Text -> [Text] -> Either String (Map Event Text)
parseKeyBindsFromFile file sectionName separator identifiers = do
  keyToIdMaybe <- parseIniFile file (parseKeyBinds sectionName separator identifiers)
  case keyToIdMaybe of
    Nothing -> Left $ "No top-level section named " <> T.unpack sectionName
    Just keyToId -> Right keyToId
