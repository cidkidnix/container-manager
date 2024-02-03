module Main where

import ContainerManager.Server
import ContainerManager.Client
import ContainerManager.Cli
import System.Environment

main :: IO ()
main = do
    arg1 <- head <$> getArgs
    case arg1 of
      "server" -> server
      "client" -> client
      "client2" -> client2
      "cli" -> cli
      _ -> pure()
