{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module ContainerManager.Cli where

import ContainerManager.Types
import ContainerManager.Shared

import Control.Concurrent

import Network.Socket

cli :: IO ()
cli = do
    containerConnect <- socket AF_UNIX (GeneralSocketType 1) 1
    connect containerConnect (SockAddrUnix "/tmp/container-manager-cli.sock")
    sendMessage containerConnect (FileEvent (Container "Steam") (Bind "/home/cidkid/test"))
    close containerConnect

