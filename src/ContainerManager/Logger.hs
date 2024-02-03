{-# LANGUAGE OverloadedStrings #-}
module ContainerManager.Logger where

import ContainerManager.Types

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.STM.TQueue
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc

logContainer :: TQueue Text -> LogLevel -> Text -> Text -> IO ()
logContainer logQ level container msg = do
    let level' = (withColor level ("[" <> prettyName level <> "]"))
        container' = ("\x1b[34m" <> "[" <> container <> "] " <> defaultC)
        full = level' <> container' <> msg
    liftIO $ atomically $ do
        writeTQueue logQ full


logLevel :: TQueue Text -> LogLevel -> Text -> IO ()
logLevel logQ lvl msg = do
    let level' = (withColor lvl ("[" <> prettyName lvl <> "] "))
        full = level' <> msg
    liftIO $ atomically $ do
        writeTQueue logQ full

handleLogs :: TQueue Text -> IO ()
handleLogs logQ = void $ forkIO $ forever $ do
    msg <- atomically $ readTQueue logQ
    putStrLn $ T.unpack $ msg
