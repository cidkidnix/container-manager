{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module ContainerManager.Client where

import ContainerManager.Types
import ContainerManager.Shared

import Network.Socket hiding (Debug)
import Control.Concurrent
import Control.Monad

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Time
import qualified Data.Text as T
import Data.Text (Text)
import Data.IORef
import Safe


client :: IO ()
client = setupClient "client1"

client2 :: IO ()
client2 = setupClient "client2"

setupClient :: Text -> IO ()
setupClient name = do
    time <- getCurrentTime
    queue <- newIORef ([] :: [(Message, Socket)])
    heartbeatRef <- newIORef time

    srvMsg <- socket AF_UNIX (GeneralSocketType 1) 1
    connect srvMsg (SockAddrUnix "/tmp/container-manager.sock")

    -- Ask for setup
    sendMessage srvMsg (Setup name)

    flip runReaderT (ClientContext heartbeatRef queue) $ do
      messageHandler
      liftIO $ heartBeat srvMsg name
      heartBeatAck

      liftIO $ clientMessageServer srvMsg queue
      liftIO $ forever $ threadDelay second

heartBeat :: Socket -> Text -> IO ()
heartBeat sock containerName = void $ forkIO $ forever $ do
    threadDelay second
    time <- getCurrentTime
    logContainer Info containerName $ "Sending Heartbeat at " <> (T.pack $ show time)
    sendMessage sock (HeartBeat time containerName)

heartBeatAck :: MonadIO m => ReaderT ClientContext m ()
heartBeatAck = do
    (ClientContext heartbeatRef _) <- ask
    void $ liftIO $ forkIO $ forever $ do
      threadDelay second
      time1 <- readIORef heartbeatRef
      threadDelay (5 * second)
      time2 <- readIORef heartbeatRef
      when (time1 == time2) $ do
          logLevel Error "Host Deamon is Dead, Forcing program stop, Goodbye"
          exit

messageHandler :: MonadIO m => ReaderT ClientContext m ()
messageHandler = do
   (ClientContext heartbeatAck queue) <- ask
   void $ liftIO $ forkIO $ forever $ do
     res <- atomicModifyIORef queue $ \x -> (drop 1 x, (headMay x))
     case res of
       Nothing -> threadDelay 10000
       Just (msg, _conn) -> case msg of
          (LinkContainer source _dest) -> do
              putStrLn source
          (Acknowledge ACK (HeartBeat time _)) -> do
              writeIORef heartbeatAck time
          (UDevEvent action node) -> do
              putStrLn $ "Node: " <> (T.unpack $ unNode node)
              putStrLn $ "Action: " <> show action

          a -> logLevel Warning $ prettyName a
