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
    connect containerConnect (SockAddrUnix "/tmp/container-manager.sock")
    sendMessage containerConnect (BindHost "/home/cidkid/test" "client2" "")
    close containerConnect
    threadDelay (2 * second)
    containerConnect <- socket AF_UNIX (GeneralSocketType 1) 1
    connect containerConnect (SockAddrUnix "/tmp/container-manager.sock")
    sendMessage containerConnect (UnbindHost "/home/cidkid/test" "client2")

