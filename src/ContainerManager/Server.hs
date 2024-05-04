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

serveUDevEvent :: Text -> TQueue Message -> TChan Message -> IO ()
serveUDevEvent container outboundQ udevChan = forever $ do
  message <- atomically $ readTChan udevChan
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
            removeFile $ hackPath </> fileName
    _ -> pure ()
  sendMessageQ outboundQ message

initializeSocket :: Text -> IO Socket
initializeSocket moveToLOC = do
    containerConnect <- socket AF_UNIX (Stream) 1
    withFdSocket containerConnect setCloseOnExecIfNeeded
    bind containerConnect (SockAddrUnix $ T.unpack moveToLOC)
    listen containerConnect 10
    pure containerConnect

server :: IO ()
server = do
    containerConnect <- initializeSocket "/tmp/container-manager-bouncer.sock"
    cliConnect <- initializeSocket "/tmp/container-manager-cli.sock"
    logQ <- newTQueueIO
    mounts <- newTVarIO (mempty :: Map Text (Set FilePath))
    (track :: TVar (Map Text (TQueue Message))) <- newTVarIO mempty
    broadcast <- newBroadcastTChanIO
    forkIO $ handleLogs logQ
    forkIO $ udevWatcher broadcast
    forkIO $ do
        queue <- newTQueueIO
        outboundQ <- newTQueueIO
        heartbeatQ <- newTVarIO =<< getCurrentTime
        print "hI"
        liftIO $ messageServer cliConnect queue outboundQ
        liftIO $ forkIO $ forever $ do
          (msgB, conn) <- atomically $ readTQueue queue
          print "After Queue"
          let (msg :: Maybe Message) = A.decode $ BS.fromStrict msgB
          print msgB
          case msg of
            Just (FileEvent (Container container) event) -> do
                val <- atomically $ readTVar track
                let containerExists = Map.lookup container val
                case containerExists of
                  Just containerQueue -> do
                      logLevel logQ Info $ "Forwarding File Event into the server queue"
                      messageLogic mounts heartbeatQ containerQueue logQ msg
                      print container
                      print event
                  Nothing -> do
                      logLevel logQ Info "Container does not exist!"

            _ -> print "error"
        forever $ threadDelay (second * 100)
    forever $ do
      logLevel logQ Info "Got Data from socket"
      (sock', _peer) <- accept containerConnect
      (recv'' :: Maybe BS.ByteString) <- recvMessage sock'
      case (A.decode . BS.fromStrict) <$> recv'' of
        Just (Just (Setup container)) -> do
          putStrLn $ "Okay Setting up conatiner " <> T.unpack container
          newSocket <- initializeSocket $ T.pack $ "/yacc" </> T.unpack container </> "container-manager.sock"
          sendMessage sock' $ MoveToSocket $ MoveTo $ T.pack $ "/yacc" </> "container-manager.sock"
          currentTime <- getCurrentTime
          heartbeat' <- newTVarIO currentTime
          queue <- newTQueueIO
          outboundQ <- newTQueueIO
          void $ forkIO $ flip runReaderT (HostContext mounts heartbeat' queue logQ outboundQ) $ do
            messageHandler
            liftIO $ messageServer newSocket queue outboundQ
            heartbeat
            liftIO $ forkIO $ do
                prevVal <- atomically $ readTVar track
                atomically $ writeTVar track $ Map.insert container outboundQ prevVal
            liftIO $ forkIO $ do
                forkIO $ do
                    threadDelay (3 * second)
                    udevEventStarter broadcast
                udevChan <- atomically $ dupTChan broadcast
                serveUDevEvent container outboundQ udevChan
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
     case time1 == time2 of
       True -> print "Container died"
       False -> pure ()

messageHandler :: MonadIO m => ReaderT HostContext m ()
messageHandler = do
   (HostContext mounts heartbeatRef queue logQ outboundQ) <- ask
   void $ liftIO $ forkIO $ forever $ do
     (msgB, conn) <- atomically $ readTQueue queue
     let (msg :: Maybe Message) = A.decode $ BS.fromStrict msgB
     messageLogic mounts heartbeatRef outboundQ logQ msg

messageLogic :: TVar (Map Text (Set FilePath)) -> TVar UTCTime -> TQueue Message -> TQueue Text -> Maybe Message -> IO ()
messageLogic mounts heartbeatRef outboundQ logQ msg = case msg of
  Nothing -> pure ()
  Just (FileEvent (Container container) event) -> do
      mount <- atomically $ readTVar mounts
      let containerMounts' = Map.lookup container mount
          containerMounts = case containerMounts' of
            Just mount -> mount
            Nothing -> Set.empty
      print "Got File Event!"
      let containerPath = "/yacc" </> T.unpack container </> "binds"
      createDirectoryIfMissing True containerPath
      case event of
        Bind fp -> do
            print mount
            case Set.member fp containerMounts of
              True -> logLevel logQ Info "Refusing to mount, already mounted!"
              False -> do
                let newMounts = Set.insert fp containerMounts
                    modifiedMap = Map.update (\_ -> Just newMounts) container mount
                    name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                print $ containerPath </> name
                Mount.bind fp $ containerPath </> name
                sendMessageQ outboundQ $ FileEvent (Container container) $ Bind name
                atomically $ writeTVar mounts modifiedMap
        Unbind fp -> do
            let newMounts = Set.delete fp containerMounts
                modifiedMap = Map.update (\_ -> Just newMounts) container mount
            let name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
            print $ containerPath </> name
            sendMessageQ outboundQ $ FileEvent (Container container) $ Unbind name
            Mount.umount $ containerPath </> name
            atomically $ writeTVar mounts modifiedMap
        _ -> logContainer logQ Info container "Not Implemented!"

  Just (HeartBeat beat client) -> do
      r <- atomically $ readTVar heartbeatRef
      atomically $ writeTVar heartbeatRef beat
      logContainer logQ Info client ("Recieved HeartBeat: " <> (T.pack $ show beat))
      sendMessageQ outboundQ (Acknowledge ACK (HeartBeat beat client))
      logContainer logQ Info client "Sending Heartbeat Acknowledgement"

  Just (Setup container) -> do
      logContainer logQ Info container "Requested Setup!"
      sendMessageQ outboundQ (Configure def)
  Just a -> do
      logLevel logQ Warning $ "Rejecting " <> prettyName a
      sendMessageQ outboundQ (Acknowledge NACK a)
