{-# LANGUAGE LambdaCase #-}
module Main where

import           Lib
import           System.Environment (getArgs)

getChildCommand :: IO (Command, [Argument])
getChildCommand =
  getArgs >>= \case
    []           -> error "No child command specified. Usage: envuserdata cmd [arg1 [arg2 [...]]"
    (cmd : args) -> return (Command cmd, map Argument args)

main :: IO ()
main = do
  (cmd, args) <- getChildCommand
  execWithKnobs cmd args =<< ec2Metadata
