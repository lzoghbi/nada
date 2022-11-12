module Main (main) where

import Brick
import Nada.App
import Nada.Org
import Data.Text.IO as Text
import qualified Data.Org as O

main :: IO ()
main = do
  rawOrgFile <- Text.readFile "test.org"
  let initialOrgFile = case O.org rawOrgFile of
                         Just orgFile -> orgFile
                         Nothing      -> error "Failed to parse org file"
  finalState <- defaultMain nadaApp (orgFileToNada initialOrgFile)
  Text.writeFile "test.org" (O.prettyOrgFile $ nadaToOrgFile finalState)
