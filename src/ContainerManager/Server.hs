{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ContainerManager.Server where

import ContainerManager.Types
import ContainerManager.Shared
import qualified ContainerManager.Mount as Mount
import ContainerManager.Logger

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
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Concurrent.STM
import qualified Data.Aeson as A
import Data.Maybe
import qualified System.Posix.Files as POSIX

import System.FilePath
import System.Directory
import System.INotify
import System.IO

inotifyWatcher :: [(String, BindType)] -> (((String, Filter), BindType) -> (Event -> IO ())) -> IO ()
inotifyWatcher directories cb = do
  inotify <- initINotify
  flip mapM_ directories $ \(path, bindType) -> forkIO $ do
    fileStatus <- POSIX.getFileStatus path
    let dir = case POSIX.isDirectory fileStatus of
                True -> (path, NoFilter)
                False -> (takeDirectory path, Filter path)
    void $ addWatch inotify [Delete, Modify, Create] (BS.toStrict $ BLU.fromString (fst dir)) (cb (dir, bindType))
  forever $ threadDelay $ second * 100

udevEventStarter :: [String] -> TChan (Message, Text) -> IO ()
udevEventStarter subsystems conn = withUDev $ \interface -> do
    enum <- newEnumerate interface
    flip mapM_ subsystems $ \subsystem ->
        addMatchSubsystem enum (BS.toStrict $ BLU.fromString subsystem)
    scanDevices enum
    entry <- getListEntry enum
    case entry of
      Just l -> do
          name <- getName l
          v <- newFromSysPath interface name
          devices <- getDevices interface l
          flip mapM_ ((Set.fromList [(getDevnode v, getSubsystem v)]) <> devices) $ \m ->
            case m of
              (Just dev, Just subsystem) -> do
                atomically $ writeTChan conn $ (UDevEvent Add (convertNode dev), (convertSubsystem subsystem))
              _ -> pure ()
      _ -> pure ()

getDevices :: UDev -> List -> IO (Set (Maybe BS.ByteString, Maybe BS.ByteString))
getDevices udev list  = getNext list >>= \case
    Nothing -> do
        path <- getName list
        v <- newFromSysPath udev path
        pure $ Set.fromList [(getDevnode v, getSubsystem v)]
    Just l -> do
        devices <- getDevices udev l
        curName <- getName l
        v <- newFromSysPath udev curName
        pure $ devices <> Set.fromList [(getDevnode v, getSubsystem v)]

udevWatcher :: [String] -> TChan (Message, Text) -> IO ()
udevWatcher subsystems conn = withUDev $ \interface -> do
    monitor <- newFromNetlink interface UDevId
    flip mapM_ subsystems $ \x ->
        filterAddMatchSubsystemDevtype monitor (BS.toStrict $ BLU.fromString x) Nothing
    enableReceiving monitor
    monitorFD <- getFd monitor
    forever $ do
      threadWaitRead monitorFD
      putStrLn $ "Udev Event"
      dev <- receiveDevice monitor
      let node = getDevnode dev
      let action = getAction dev
      let subsystem = getSubsystem dev
      case (node, action, subsystem) of
        (Just node', Just action', Just subsystem') -> do
            atomically $ writeTChan conn $ (UDevEvent (convertAction action') (convertNode node'), convertSubsystem subsystem')
        _ -> pure ()

serveUDevEvent :: [String] -> Text -> TVar (Map Text (Set FilePath)) -> TQueue Message -> TChan (Message, Text) -> IO ()
serveUDevEvent allowed container mounts outboundQ udevChan = forever $ do
  message <- atomically $ readTChan udevChan
  mount <- atomically $ readTVar mounts
  let containerMounts' = Map.lookup container mount
      containerMounts = case containerMounts' of
        Just mounts' -> mounts'
        Nothing -> Set.empty
  case message of
    (UDevEvent Add node, subsystem) -> do
      let useEvent = (T.unpack subsystem) `elem` allowed
      when useEvent $ do
        putStrLn $ "Add Event for subsystem: " <> show subsystem <> " allowed!"
        let fileName = joinPath $ filter (\x -> x /= "/") $ splitPath $ T.unpack $ unNode $ node
            hackPath = "/yacc" </> T.unpack container </> "udev"
            directory = takeDirectory $ hackPath </> fileName
        createDirectoryIfMissing True directory

        mounted <- Mount.alreadyMounted $ hackPath </> fileName
        case mounted of
          True -> do
            let newSet = Set.insert (T.unpack $ unNode node) containerMounts
                newMap = Map.insert container newSet mount
            atomically $ writeTVar mounts newMap
          False -> do
            let newSet = Set.insert (T.unpack $ unNode node) containerMounts
                newMap = Map.insert container newSet mount
            atomically $ writeTVar mounts newMap
            Mount.bind (T.unpack $ unNode node) $ hackPath </> fileName
    (UDevEvent Remove node, subsystem) -> do
        let useEvent = (T.unpack subsystem) `elem` allowed
        when useEvent $ do
          putStrLn $ "Remove Event for subsystem: " <> show subsystem <> " allowed!"
          let fileName = joinPath $ filter (\x -> x /= "/") $ splitPath $ T.unpack $ unNode $ node
              hackPath = "/yacc" </> T.unpack container </> "udev"
          exists <- doesPathExist $ hackPath </> fileName
          when exists $ do
              Mount.umount $ hackPath </> fileName
              removeFile $ hackPath </> fileName
          let newSet = Set.delete (T.unpack $ unNode node) containerMounts
              newMap = Map.insert container newSet mount
          atomically $ writeTVar mounts newMap
    _ -> pure ()
  sendMessageQ outboundQ (fst message)

initializeSocket :: Text -> IO Socket
initializeSocket moveToLOC = do
    containerConnect <- socket AF_UNIX (Stream) 1
    withFdSocket containerConnect setCloseOnExecIfNeeded
    bind containerConnect (SockAddrUnix $ T.unpack moveToLOC)
    listen containerConnect 10
    pure containerConnect

server :: IO ()
server = do
    hSetBuffering stdout LineBuffering
    (config :: Maybe Config) <- (A.decode . BLU.fromString) <$> readFile "/etc/container-manager.conf"
    let config' = case config of
                   Nothing -> def
                   Just conf -> conf
        configMap = containerConfigs config'
        allSubSystems = concat $ catMaybes $ map (_udev_filters . snd) (Map.toList configMap)
    print config'
    containerConnect <- initializeSocket "/tmp/container-manager-bouncer.sock"
    cliConnect <- initializeSocket "/tmp/container-manager-cli.sock"
    logQ <- newTQueueIO
    mounts <- newTVarIO (mempty :: Map Text (Set FilePath))
    (track :: TVar (Map Text (TQueue Message))) <- newTVarIO mempty
    broadcast <- newBroadcastTChanIO
    void $ forkIO $ handleLogs logQ
    void $ forkIO $ udevWatcher allSubSystems broadcast
    void $ forkIO $ do
        queue <- newTQueueIO
        outboundQ <- newTQueueIO
        heartbeatQ <- newTVarIO =<< getCurrentTime
        liftIO $ messageServer cliConnect queue outboundQ
        liftIO $ void $ forkIO $ forever $ do
          (msgB, _) <- atomically $ readTQueue queue
          let (msg :: Maybe Message) = A.decode $ BS.fromStrict msgB
          case msg of
            Just (FileEvent (Container container) _) -> do
                val <- atomically $ readTVar track
                let containerExists = Map.lookup container val
                case containerExists of
                  Just containerQueue -> do
                      logLevel logQ Info $ "Forwarding File Event into the server queue"
                      messageLogic mounts heartbeatQ containerQueue logQ msg
                  Nothing -> do
                      logLevel logQ Info "Container does not exist!"

            _ -> pure ()
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
            liftIO $ void $ forkIO $ do
                prevVal <- atomically $ readTVar track
                atomically $ writeTVar track $ Map.insert container outboundQ prevVal

            liftIO $ void $ forkIO $ do
              let inotifyDirectories = case _inotify_watch <$> (Map.lookup container configMap) of
                                         Just (Just dirs) -> dirs
                                         _ -> mempty

              inotifyWatcher (Map.toList inotifyDirectories) $ \((dir, filterT), bindType) -> \event -> do
                let bindEventType = case bindType of
                                        Absolute -> \x -> BindDiffPath x x
                                        Host -> Bind
                    unbindEventType = case bindType of
                                        Absolute -> UnbindABS
                                        Host -> Unbind
                case event of
                  Created _ filePath' -> do
                    let fullDir = dir </> (BLU.toString $ BS.fromStrict filePath')
                    case filterT of
                      Filter fp -> when ((takeFileName fp) == (takeFileName fullDir)) $ do
                        logLevel logQ Info $ "Inotify With Filter " <> T.pack fp
                        let event' = Just $ FileEvent (Container container) $ bindEventType $ fullDir
                        threadDelay second
                        messageLogic mounts heartbeat' outboundQ logQ event'
                        logLevel logQ Info $ "INOTIFY Create Event"
                      NoFilter -> do
                        logLevel logQ Info $ "Inotify No Filter"
                        let event' = Just $ FileEvent (Container container) $ bindEventType $ fullDir
                        threadDelay second
                        messageLogic mounts heartbeat' outboundQ logQ event'
                        logLevel logQ Info $ "INOTIFY Create Event"
                  Deleted _ filePath' -> do
                    let fullDir = dir </> (BLU.toString $ BS.fromStrict filePath')
                        event' = Just $ FileEvent (Container container) $ unbindEventType $ fullDir
                    threadDelay second
                    messageLogic mounts heartbeat' outboundQ logQ event'
                    logLevel logQ Info $ "INOTIFY Delete Event"
                  a -> logLevel logQ Info $ T.pack $ show a
            liftIO $ void $ forkIO $ do
                let allowedEvents = _udev_filters <$> (Map.lookup container configMap)
                    allowedEvents' = case allowedEvents of
                                       Just (Just events) -> events
                                       _ -> []
                    runUdev = case _filter_udev_events <$> (Map.lookup container configMap) of
                                Just a -> a
                                _ -> False

                    automount = _automount <$> (Map.lookup container configMap)
                    automount' = case automount of
                                    Just (Just mounts') -> mounts'
                                    _ -> mempty
                void $ forkIO $ do
                    flip mapM_ (Map.toList automount') $ \(path, btype) -> do
                        let bindEventType = case btype of
                                        Absolute -> BindDiffPath path
                                        Host -> Bind
                        logLevel logQ Info $ "Mounting path: " <> T.pack path
                        messageLogic mounts heartbeat' outboundQ logQ $
                          Just $ FileEvent (Container container) $ bindEventType $ path
                    logLevel logQ Info $ "Starting Heartbeat"
                    messageLogic mounts heartbeat' outboundQ logQ $
                        Just $ StartHeartBeat
                    flip runReaderT (HostContext mounts heartbeat' queue logQ outboundQ) $
                        heartbeat
                when runUdev $ do
                  logLevel logQ Info $ "Starting Udev listener"
                  void $ forkIO $ do
                      threadDelay (3 * second)
                      udevEventStarter allSubSystems broadcast
                  udevChan <- atomically $ dupTChan broadcast
                  serveUDevEvent allowedEvents' container mounts outboundQ udevChan
            liftIO $ forever $ threadDelay 1000000
        _ -> pure ()

heartbeat :: MonadIO m => ReaderT HostContext m ()
heartbeat = do
   (HostContext _ heartbeatRef _ _ _) <- ask
   liftIO $ putStrLn "Starting Heartbeat"
   void $ liftIO $ forkIO $ forever $ do
     time1 <- atomically $ readTVar heartbeatRef
     threadDelay (120 * second)
     time2 <- atomically $ readTVar heartbeatRef
     case time1 == time2 of
       True -> putStrLn "Container died"
       False -> pure ()

messageHandler :: MonadIO m => ReaderT HostContext m ()
messageHandler = do
   (HostContext mounts heartbeatRef queue logQ outboundQ) <- ask
   void $ liftIO $ forkIO $ forever $ do
     (msgB, _) <- atomically $ readTQueue queue
     let (msg :: Maybe Message) = A.decode $ BS.fromStrict msgB
     messageLogic mounts heartbeatRef outboundQ logQ msg

messageLogic :: TVar (Map Text (Set FilePath)) -> TVar UTCTime -> TQueue Message -> TQueue Text -> Maybe Message -> IO ()
messageLogic mounts heartbeatRef outboundQ logQ msg = case msg of
  Nothing -> pure ()
  Just (FileEvent (Container container) event) -> do
      mount <- atomically $ readTVar mounts
      let containerMounts' = Map.lookup container mount
          containerMounts = case containerMounts' of
            Just mounts' -> mounts'
            Nothing -> Set.empty
      logLevel logQ Info "Got File Event!"
      let containerPath = "/yacc" </> T.unpack container </> "binds"
      createDirectoryIfMissing True containerPath
      case event of
        Bind fp -> do
            case Set.member fp containerMounts of
              True -> logLevel logQ Info "Refusing to mount, already mounted!"
              False -> do
                let newMounts = Set.insert fp containerMounts
                    modifiedMap = Map.insert container newMounts mount
                    name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                    directory = takeDirectory $ containerPath </> name
                logLevel logQ Info $ T.pack $ containerPath </> name
                logLevel logQ Info $ "New mount stack " <> (T.pack $ show modifiedMap)
                createDirectoryIfMissing True directory

                mounted <- Mount.alreadyMounted $ containerPath </> name
                case mounted of
                  True -> do
                    logLevel logQ Info $ "Path already mounted, refreshing mount"
                    Mount.refreshMount fp $ containerPath </> name
                    sendMessageQ outboundQ $ FileEvent (Container container) $ Bind name
                    atomically $ writeTVar mounts modifiedMap
                  False -> do
                    Mount.bind fp $ containerPath </> name
                    sendMessageQ outboundQ $ FileEvent (Container container) $ Bind name
                    atomically $ writeTVar mounts modifiedMap
        Unbind fp -> do
            let newMounts = Set.delete fp containerMounts
                modifiedMap = Map.insert container newMounts mount
            logLevel logQ Info $ "New mount stack " <> (T.pack $ show modifiedMap)
            case Set.member fp containerMounts of
              False -> logLevel logQ Info "Refusing to unmount, not mounted"
              _ -> do
                let name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                logLevel logQ Info $ T.pack $ containerPath </> name
                sendMessageQ outboundQ $ FileEvent (Container container) $ Unbind name
                Mount.umount $ containerPath </> name
                atomically $ writeTVar mounts modifiedMap

        UnbindABS fp -> do
            let newMounts = Set.delete fp containerMounts
                modifiedMap = Map.insert container newMounts mount
            logLevel logQ Info $ "New mount stack " <> (T.pack $ show modifiedMap)
            case Set.member fp containerMounts of
              False -> logLevel logQ Info "UNBINDABS: Refusing to unmount, not mounted"
              _ -> do
                  let name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                  mounted <- Mount.alreadyMounted $ containerPath </> name
                  case mounted of
                    True -> do
                      Mount.umount $ containerPath </> name
                      atomically $ writeTVar mounts modifiedMap
                    False -> do
                      atomically $ writeTVar mounts modifiedMap
                  sendMessageQ outboundQ $ FileEvent (Container container) $ UnbindABS fp

        BindDiffPath fp to -> do
            let newMounts = Set.insert fp containerMounts
                modifiedMap = Map.insert container newMounts mount
                name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                directory = takeDirectory $ containerPath </> name
            createDirectoryIfMissing True directory
            logLevel logQ Info $ "New mount stack " <> (T.pack $ show modifiedMap)

            mounted <- Mount.alreadyMounted $ containerPath </> name
            case mounted of
              True -> do
                  logLevel logQ Info $ "Path already mounted, refreshing mount"
                  Mount.refreshMount fp $ containerPath </> name
                  atomically $ writeTVar mounts modifiedMap
                  sendMessageQ outboundQ $ FileEvent (Container container) $ BindDiffPath name to
              False -> do
                  atomically $ writeTVar mounts modifiedMap
                  Mount.bind fp $ containerPath </> name
                  sendMessageQ outboundQ $ FileEvent (Container container) $ BindDiffPath name to

  Just (HeartBeat beat client) -> do
      atomically $ writeTVar heartbeatRef beat
      logContainer logQ Info client ("Recieved HeartBeat: " <> (T.pack $ show beat))
      sendMessageQ outboundQ (Acknowledge ACK (HeartBeat beat client))
      logContainer logQ Info client "Sending Heartbeat Acknowledgement"

  Just (StartHeartBeat) -> do
      logLevel logQ Info $ "Sending Heartbeat Start to container"
      threadDelay $ 5 * second
      sendMessageQ outboundQ StartHeartBeat

  Just (Setup container) -> do
      logContainer logQ Info container "Requested Setup!"
      sendMessageQ outboundQ (Configure def)
  Just a -> do
      logLevel logQ Warning $ "Rejecting " <> prettyName a
      sendMessageQ outboundQ (Acknowledge NACK a)
