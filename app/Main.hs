module Main (main) where

import Brick
import Nada

main :: IO ()
main = defaultMain nadaApp nadaDefaultState >> return ()
