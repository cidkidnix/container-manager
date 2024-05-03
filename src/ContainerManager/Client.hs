{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module ContainerManager.Client where

import ContainerManager.Types
import ContainerManager.Shared
import qualified ContainerManager.Mount as Mount

import Network.Socket hiding (Debug)
import Control.Concurrent
import Control.Monad

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader


import qualified Data.ByteString as BS

import Control.Concurrent.Async
import Data.Time
import qualified Data.Text as T
import Data.Text (Text)

import Control.Concurrent.STM
import qualified Data.Aeson as A
import System.Directory
import System.FilePath

client :: IO ()
client = setupClient "Steam"

client2 :: IO ()
client2 = setupClient "client1"

stress :: IO ()
stress = void $ flip mapConcurrently [1..5] $ \i -> do
    setupClient $ T.pack $ "client" <> show i

setupClient :: Text -> IO ()
setupClient name = do
    time <- getCurrentTime
    queue <- newTQueueIO
    logQ <- newTQueueIO
    heartbeatRef <- newTVarIO (time :: UTCTime)
    putStrLn "setup client"

    srvMsg <- socket AF_UNIX (GeneralSocketType 1) 1
    connect srvMsg (SockAddrUnix "/tmp/container-manager-bouncer.sock")
    threadDelay second

    -- Ask for setup
    sendMessage srvMsg (Setup name)
    resp <- recvMessage srvMsg
    close srvMsg

    case (A.decode . BS.fromStrict) <$> resp of
      Just (Just (MoveToSocket (MoveTo sock))) -> do
        newSrv <- socket AF_UNIX (GeneralSocketType 1) 1
        connect newSrv (SockAddrUnix $ T.unpack $ sock)

        flip runReaderT (ClientContext heartbeatRef queue logQ) $ do
          heartBeatAck
          heartBeat newSrv name
          liftIO $ clientMessageServer newSrv queue
          liftIO $ handleLogs logQ
          messageHandler
          liftIO $ forever $ threadDelay $ 1000 * second
      _ -> pure ()


heartBeat :: MonadIO m => Socket -> Text -> ReaderT ClientContext m ()
heartBeat sock containerName = do
    logQ <- asks _clientLog
    liftIO $ void $ forkIO $ forever $ do
      threadDelay (1 * second)
      time <- getCurrentTime
      logContainer logQ Info containerName $ "Sending Heartbeat at " <> (T.pack $ show time)
      forkIO $ sendMessage sock (HeartBeat time containerName)

heartBeatAck :: MonadIO m => ReaderT ClientContext m ()
heartBeatAck = do
    heartbeatRef <- asks _clientHeartbeatRef
    logQ <- asks _clientLog
    void $ liftIO $ forkIO $ forever $ do
      time1 <- atomically $ readTVar heartbeatRef
      threadDelay (5 * second)
      time2 <- atomically $ readTVar heartbeatRef
      liftIO $ print time1
      liftIO $ print time2
      when (time1 == time2) $ do
          logLevel logQ Error "Host Deamon is Dead, Forcing program stop, Goodbye"
          --exit

messageHandler :: MonadIO m => ReaderT ClientContext m ()
messageHandler = do
   (ClientContext heartbeatAck queue logQ) <- ask
   void $ liftIO $ forkIO $ forever $ do
     (msgB, _conn) <- atomically $ readTQueue queue
     let msg = A.decode $ BS.fromStrict msgB
     case msg of
       Just (Acknowledge ACK (HeartBeat time _)) -> do
           logLevel logQ Info $ "Got heartbeat back for " <> (T.pack $ show time)
           atomically $ writeTVar heartbeatAck time
       Just Shutdown -> exit
       Just (FileEvent _ event) -> do
           let path = "/yacc" </> "binds"
           case event of
             Bind fp -> do
                 Mount.bind (path </> fp) fp
             BindDiffPath  _ _ -> pure ()
             Unbind fp -> do
                 Mount.umount fp
       Just (UDevEvent action node) -> do
           let fileName = takeFileName $ T.unpack $ unNode node
               hackPath = "/yacc/udev"
           case action of
             Add -> do
                Mount.bind (hackPath </> fileName) $ T.unpack $ unNode node
             Remove -> do
                exists <- doesPathExist $ T.unpack $ unNode node
                when exists $ do
                    Mount.umount $ T.unpack $ unNode node
                    removeFile $ T.unpack $ unNode node

       Just a -> logLevel logQ Warning $ prettyName a
       Nothing -> pure ()
