{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ContainerManager.Server where

import ContainerManager.Types
import ContainerManager.Shared

import Network.Socket
import Control.Monad
import Control.Concurrent
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import qualified System.Linux.Mount as Mount
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import System.UDev
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Binary as BN
import Control.Exception
import qualified Data.Aeson as A

import Control.Concurrent.Async
import qualified System.Posix.Types as POSIX

udevWatcher :: TQueue Message -> IO ()
udevWatcher conn = withUDev $ \interface -> do
    monitor <- newFromNetlink interface UDevId
    filterAddMatchSubsystemDevtype monitor (BS.toStrict $ BLU.fromString "hidraw") Nothing
    enableReceiving monitor
    monitorFD <- getFd monitor
    forever $ do
      threadWaitRead monitorFD
      putStrLn $ "Udev Event"
      dev <- receiveDevice monitor
      let node = getDevnode dev
      let action = getAction dev
      case (node, action) of
        (Just node', Just action') -> do
            putStrLn $ "Notified container about new hidraw devices " <> show action' <> " " <> show node'
            sendMessageQ conn $ UDevEvent (convertAction action') (convertNode node')
        _ -> pure ()

initializeSocket :: Text -> IO Socket
initializeSocket moveToLOC = do
    containerConnect <- socket AF_UNIX (Stream) 1
    withFdSocket containerConnect setCloseOnExecIfNeeded
    bind containerConnect (SockAddrUnix $ T.unpack moveToLOC)
    listen containerConnect 5
    pure containerConnect

--setupSTM :: IO ()
--setupSTM = do
--    heartbeat <- newTVarIO (mempty :: Map Text (Socket, UTCTime))
--    queue <- newTQueueIO
--    mounts <- newTVarIO (mempty :: Map Text (Set FilePath))
--    logQ <- newTQueueIO
--    outboundQ <- newTQueueIO
--    pure (heartbeat, queue, mounts, logQ, outboundQ)


server :: IO ()
server = do
    containerConnect <- initializeSocket "/tmp/container-manager-bouncer.sock"
    logQ <- newTQueueIO
    forkIO $ handleLogs logQ


    forever $ do
      print "Got Data from socket"
      (sock', _peer) <- accept containerConnect
      (recv'' :: Maybe BS.ByteString) <- recvMessage sock'
      case (A.decode . BS.fromStrict) <$> recv'' of
        Just (Just (Setup container)) -> do
          putStrLn $ "Okay Setting up conatiner " <> T.unpack container
          newSocket <- initializeSocket $ "/tmp/container-manager-" <> container <> ".sock"
          sendMessage sock' $ MoveToSocket $ MoveTo $ "/tmp/container-manager-" <> container <> ".sock"
          heartbeat' <- newTVarIO (mempty :: Map Text (Socket, UTCTime))
          queue <- newTQueueIO
          mounts <- newTVarIO (mempty :: Map Text (Set FilePath))
          outboundQ <- newTQueueIO
          void $ forkIO $ flip runReaderT (HostContext mounts heartbeat' queue logQ outboundQ) $ do
            messageHandler
            liftIO $ messageServer newSocket queue outboundQ
            heartbeat
            liftIO $ forkIO $ udevWatcher outboundQ
            liftIO $ forever $ threadDelay 1000000
        _ -> pure ()
    --(heartbeat', queue, mounts, logQ, outboundQ) <- setupSTM
    --clientCount <- newTVarIO 0
    --flip runReaderT (HostContext mounts heartbeat' queue clientCount logQ outboundQ) $ do
    --  messageHandler
    --  liftIO $ handleLogs logQ
    --  liftIO $ messageServer containerConnect clientCount queue
    --  heartbeat
    --  liftIO $ forever $ threadDelay 1000000

heartbeat :: MonadIO m => ReaderT HostContext m ()
heartbeat = do
   (HostContext _ heartbeatRef _ logQ _) <- ask
   liftIO $ putStrLn "Starting Heartbeat"
   void $ liftIO $ forkIO $ forever $ do
     time1 <- atomically $ readTVar heartbeatRef
     threadDelay (5 * second)
     time2 <- atomically $ readTVar heartbeatRef

     let clients = Map.intersectionWith (\(_, client1) -> \(sock, client') -> (sock, client1 == client')) time1 time2
     forkIO $ void $ flip mapConcurrently (Map.toList clients) $ \(client, (_sock, y)) -> do
         when y $ do
             atomically $ do
                 current <- readTVar heartbeatRef
                 let fixedMap = Map.delete client current
                 writeTVar heartbeatRef fixedMap
             logContainer logQ Error client "Container Has Died"

messageHandler :: MonadIO m => ReaderT HostContext m ()
messageHandler = do
   (HostContext _ heartbeatRef queue logQ outboundQ) <- ask
   void $ liftIO $ forkIO $ forever $ do
     threadDelay 1000
     queue' <- atomically $ flushTQueue queue
     flip mapM_ queue' $ \(msgB, conn) -> do
       let (msg :: Maybe Message) = A.decode $ BS.fromStrict msgB
       case msg of
          Nothing -> pure ()
          --putStrLn "Nothing"
          --pure ()
          Just (BindHost _path _container _) -> do
              pure ()
              {-logContainer Warning container $ "Binding " <> T.pack path
              let pat' = "/tmp/" <> T.unpack container <> "/" <> path
              createDirectoryIfMissing True pat'
              Mount.bind path pat'-}
          Just (UnbindHost path container) -> do
              logContainer logQ Info container $ "Unbinding " <> T.pack path <> " from container " <> container
              --Mount.umount ("/tmp/" <> T.unpack container <> "/" <> "test")
          Just (HeartBeat beat client) -> do
              r <- atomically $ readTVar heartbeatRef
              let exists = Map.lookup client r
                  r' = case exists of
                    Just _ -> Map.update (\_ -> Just (conn, beat)) client r
                    Nothing -> Map.insert client (conn, beat) r
              atomically $ writeTVar heartbeatRef r'
              logContainer logQ Info client ("Recieved HeartBeat: " <> (T.pack $ show beat))
              sendMessageQ outboundQ (Acknowledge ACK (HeartBeat beat client))
              logContainer logQ Info client "Sending Heartbeat Acknowledgement"

          Just (Setup container) -> do
              logContainer logQ Info container "Requested Setup!"
              -- Run Client specific Daemons, such as udev events
              forkIO $ udevWatcher outboundQ
              sendMessageQ outboundQ (Configure def)
          -- We don't respond to these requests
          Just a -> do
              logLevel logQ Warning $ "NACKing " <> prettyName a
              sendMessageQ outboundQ (Acknowledge NACK a)
