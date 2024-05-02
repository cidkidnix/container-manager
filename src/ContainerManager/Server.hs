{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ContainerManager.Server where

import ContainerManager.Types
import ContainerManager.Shared
import qualified ContainerManager.Mount as Mount

import Network.Socket
import Control.Monad
import Control.Concurrent
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import System.UDev hiding (Action, Add, Remove)
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
import qualified System.Posix.Files as POSIX
import System.FilePath
import System.Directory

udevEventStarter :: TChan Message -> IO ()
udevEventStarter conn = withUDev $ \interface -> do
    enum <- newEnumerate interface
    addMatchSubsystem enum "hidraw"
    scanDevices enum
    entry <- getListEntry enum
    case entry of
      Just l -> do
          name <- getName l
          v <- newFromSysPath interface name
          devices <- getDevices interface l
          flip mapM_ ((Set.fromList [getDevnode v]) <> devices) $ \case
            Just dev -> do
              atomically $ writeTChan conn $ UDevEvent Add (convertNode dev)
              print dev
            Nothing -> pure ()
      _ -> pure ()

getDevices :: UDev -> List -> IO (Set (Maybe BS.ByteString))
getDevices udev list  = getNext list >>= \case
    Nothing -> do
        path <- getName list
        v <- newFromSysPath udev path
        pure $ Set.fromList [getDevnode v]
    Just l -> do
        devices <- getDevices udev l
        curName <- getName l
        v <- newFromSysPath udev curName
        pure $ devices <> Set.fromList [getDevnode v]

udevWatcher :: TChan Message -> IO ()
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
            atomically $ writeTChan conn $ UDevEvent (convertAction action') (convertNode node')
        _ -> pure ()

initializeSocket :: Text -> IO Socket
initializeSocket moveToLOC = do
    containerConnect <- socket AF_UNIX (Stream) 1
    withFdSocket containerConnect setCloseOnExecIfNeeded
    bind containerConnect (SockAddrUnix $ T.unpack moveToLOC)
    listen containerConnect 5
    pure containerConnect

server :: IO ()
server = do
    containerConnect <- initializeSocket "/tmp/container-manager-bouncer.sock"
    logQ <- newTQueueIO
    broadcast <- newBroadcastTChanIO
    forkIO $ handleLogs logQ
    forkIO $ udevWatcher broadcast
    forever $ do
      logLevel logQ Info "Got Data from socket"
      (sock', _peer) <- accept containerConnect
      (recv'' :: Maybe BS.ByteString) <- recvMessage sock'
      case (A.decode . BS.fromStrict) <$> recv'' of
        Just (Just (Setup container)) -> do
          putStrLn $ "Okay Setting up conatiner " <> T.unpack container
          newSocket <- initializeSocket $ T.pack $ "/yacc" </> T.unpack container </> "container-manager.sock"
          sendMessage sock' $ MoveToSocket $ MoveTo $ T.pack $ "/yacc" </> "container-manager.sock"
          heartbeat' <- newTVarIO (mempty :: Map Text (Socket, UTCTime))
          queue <- newTQueueIO
          mounts <- newTVarIO (mempty :: Map Text (Set FilePath))
          outboundQ <- newTQueueIO
          void $ forkIO $ flip runReaderT (HostContext mounts heartbeat' queue logQ outboundQ) $ do
            messageHandler
            liftIO $ messageServer newSocket queue outboundQ
            heartbeat
            liftIO $ forkIO $ do
                forkIO $ do
                    threadDelay (3 * second)
                    udevEventStarter broadcast
                udevChan <- atomically $ dupTChan broadcast
                forever $ do
                  message <- atomically $ readTChan udevChan
                  sendMessageQ outboundQ message
                  case message of
                    UDevEvent Add node -> do
                      let fileName = takeFileName $ T.unpack $ unNode node
                          hackPath = "/yacc" </> T.unpack container </> "udev"
                      createDirectoryIfMissing True hackPath
                      Mount.bind (T.unpack $ unNode node) $ hackPath </> fileName
                    UDevEvent Remove node -> do
                        let fileName = takeFileName $ T.unpack $ unNode node
                            hackPath = "/yacc" </> T.unpack container </> "udev"
                        exists <- doesPathExist $ hackPath </> fileName
                        when exists $ do
                            Mount.umount $ hackPath </> fileName

                    _ -> pure ()
            liftIO $ forever $ threadDelay 1000000
        _ -> pure ()

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
              sendMessageQ outboundQ (Configure def)
          -- We don't respond to these requests
          Just a -> do
              logLevel logQ Warning $ "NACKing " <> prettyName a
              sendMessageQ outboundQ (Acknowledge NACK a)
