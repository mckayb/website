module Main where

import Prelude
import Model

main :: IO ()
main = runAppDB runAppMigrationsSafe