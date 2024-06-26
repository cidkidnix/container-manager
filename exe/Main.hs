{-# LANGUAGE OverloadedStrings #-}
module Main where

import ContainerManager.Server
import ContainerManager.Client
import ContainerManager.Cli
import System.Environment
import Network.HostName
import qualified Data.Text as T

main :: IO ()
main = do
    arg1 <- head <$> getArgs
    case arg1 of
      "server" -> server
      "client" -> (setupClient . T.pack) =<< getHostName
      "client2" -> client2
      "client3" -> setupClient "client3"
      "client4" -> setupClient "client4"
      "stress" -> stress
      "cli" -> cli
      _ -> pure()
