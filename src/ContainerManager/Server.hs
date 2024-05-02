{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ContainerManager.Server where

import ContainerManager.Types
import ContainerManager.Shared

import System.Directory
import Network.Socket
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Time.Clock
import Safe
import qualified Data.Text as T
import Data.Text (Text)
import System.FilePath
import System.Posix.Files as Files
import qualified Data.Map as Map
import qualified System.Linux.Mount as Mount
import Data.Map (Map)
import Data.Set (Set)
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import System.UDev
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-strin

udevWatcher :: Socket -> IO ()
udevWatcher conn = withUDev $ \interface -> do
    enumerate <- newEnumerate interface
    monitor <- newFromNetlink interface UDevId
    filterAddMatchSubsystemDevtype monitor (BS.toStrict $ BLU.fromString "hidraw") Nothing
    enableReceiving monitor
    monitorFD <- getFd monitor
    forever $ do
      threadWaitRead monitorFD
      print $ "RECEIVING DEVICE"
      dev <- receiveDevice monitor
      let node = getDevnode dev
      let action = getAction dev
      case (node, action) of
        (Just node', Just action') -> do
            sendMessage conn $ UDevEvent (convertAction action') (convertNode node')
        _ -> pure ()

server :: IO ()
server = do
    containerConnect <- socket AF_UNIX (GeneralSocketType 1) 1
    bind containerConnect (SockAddrUnix "/tmp/container-manager.sock")
    listen containerConnect 100
    heartbeatRef <- newIORef (mempty :: Map Text UTCTime)
    queue <- newIORef ([] :: [(Message, Socket)])
    mounts <- newIORef (mempty :: Map Text (Set FilePath))
    flip runReaderT (HostContext mounts heartbeatRef queue) $ do
      messageHandler
      liftIO $ messageServer containerConnect queue
      heartbeat
      liftIO $ forever $ threadDelay 1000000

heartbeat :: MonadIO m => ReaderT HostContext m ()
heartbeat = do
   (HostContext _ heartbeatRef _) <- ask
   void $ liftIO $ forkIO $ forever $ do
     threadDelay 1000000
     time1 <- readIORef heartbeatRef
     threadDelay 5000000
     time2 <- readIORef heartbeatRef

     let k = Map.keys time2
     flip mapConcurrently k $ \client -> do
        let lookupTime = Map.lookup client time1
            lookupTime2 = Map.lookup client time2
        when (lookupTime == lookupTime2) $ do
         logContainer Error client "Container Has Died"

messageHandler :: MonadIO m => ReaderT HostContext m ()
messageHandler = do
   (HostContext _ heartbeatRef queue) <- ask
   void $ liftIO $ forkIO $ forever $ do
     res <- atomicModifyIORef queue $ \x -> (drop 1 x, (headMay x))
     case res of
       Nothing -> threadDelay 10000
       Just (msg, conn) -> case msg of
           (BindHost path container _) -> do
               pure ()
               {-logContainer Warning container $ "Binding " <> T.pack path
               let pat' = "/tmp/" <> T.unpack container <> "/" <> path
               createDirectoryIfMissing True pat'
               Mount.bind path pat'-}
           (UnbindHost path container) -> do
               logContainer Info container $ "Unbinding " <> T.pack path <> " from container " <> container
               Mount.umount ("/tmp/" <> T.unpack container <> "/" <> "test")
           (UDevEvent _ _) -> error "Why did you send this?"
           (HeartBeat beat client) -> do
               r <- readIORef heartbeatRef
               let exists = Map.lookup client r
                   r' = case exists of
                     Just _ -> Map.update (\_ -> Just beat) client r
                     Nothing -> Map.insert client beat r
               atomicModifyIORef heartbeatRef $ \x -> ((r' <> x), ())
               logContainer Info client ("Recieved HeartBeat: " <> (T.pack $ show beat))
               sendMessage conn (Acknowledge ACK (HeartBeat beat client))
           (Setup container) -> do
               logContainer Info container "Requested Setup!"
               forkIO $ udevWatcher conn
               sendMessage conn (Configure def)
           -- We don't respond to these requests
           a -> do
               logLevel Warning $ "NACKing " <> prettyName a
               sendMessage conn (Acknowledge NACK a)
