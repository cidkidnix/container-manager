{-# LANGUAGE OverloadedStrings #-}
module ContainerManager.Client where

import ContainerManager.Types
import ContainerManager.Shared
import qualified ContainerManager.Mount as Mount
import ContainerManager.Logger

import Network.Socket hiding (Debug)
import Control.Concurrent
import Control.Monad

import qualified Data.Set as Set

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import qualified Data.ByteString as BS

import Data.Time
import qualified Data.Text as T
import Data.Text (Text)

import Control.Concurrent.STM
import qualified Data.Aeson as A
import System.Directory
import System.FilePath

setupClient :: Text -> IO ()
setupClient name = do
    queue <- newTQueueIO
    logQ <- newTQueueIO
    clientMounts <- newTVarIO mempty
    heartbeatRef <- newTVarIO =<< getCurrentTime
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

        flip runReaderT (ClientContext heartbeatRef queue logQ clientMounts) $ do
          liftIO $ clientMessageServer newSrv queue
          liftIO $ handleLogs logQ
          messageHandler $
            flip runReaderT (ClientContext heartbeatRef queue logQ clientMounts) $ cb newSrv name
          liftIO $ forever $
              threadDelay $ 1000 * second
      _ -> pure ()
    where
        cb :: MonadIO m => Socket -> Text -> ReaderT ClientContext m ()
        cb newSrv name' = do
          heartBeatAck
          heartBeat newSrv name'


heartBeat :: MonadIO m => Socket -> Text -> ReaderT ClientContext m ()
heartBeat sock containerName = do
    logQ <- asks _clientLog
    liftIO $ do
      logContainer logQ Info containerName $ "Starting heartbeat"
      void $ forkIO $ forever $ do
        threadDelay (30 * second)
        time <- getCurrentTime
        logContainer logQ Info containerName $ "Sending Heartbeat at " <> (T.pack $ show time)
        forkIO $ sendMessage sock (HeartBeat time containerName)

heartBeatAck :: MonadIO m => ReaderT ClientContext m ()
heartBeatAck = do
    heartbeatRef <- asks _clientHeartbeatRef
    logQ <- asks _clientLog
    liftIO $ do
      logLevel logQ Info "Heartbeat Acknowledgement started"
      void $ forkIO $ forever $ do
        time1 <- atomically $ readTVar heartbeatRef
        threadDelay (60 * second)
        time2 <- atomically $ readTVar heartbeatRef
        liftIO $ print time1
        liftIO $ print time2
        when (time1 == time2) $ do
            logLevel logQ Error "Host Deamon is Dead, Forcing program stop, Goodbye"

messageHandler :: MonadIO m => IO () -> ReaderT ClientContext m ()
messageHandler cb = do
   (ClientContext heartbeatAck queue logQ mounts) <- ask
   void $ liftIO $ forkIO $ forever $ do
     (msgB, _conn) <- atomically $ readTQueue queue
     let msg = A.decode $ BS.fromStrict msgB
     case msg of
       Just (StartHeartBeat) -> do
           logLevel logQ Info $ "Starting Heartbeat"
           void $ forkIO $ cb
       Just (Acknowledge ACK (HeartBeat time _)) -> do
           logLevel logQ Info $ "Got heartbeat back for " <> (T.pack $ show time)
           atomically $ writeTVar heartbeatAck time
       Just Shutdown -> exit
       Just (FileEvent _ event) -> do
           let path = "/yacc" </> "binds"
           mount <- atomically $ readTVar mounts
           case event of
             Bind fp -> do
                 mounted <- Mount.alreadyMounted fp
                 case mounted of
                   True -> do
                       let newSet = Set.insert fp mount
                       atomically $ writeTVar mounts newSet
                   False -> case Set.member fp mount of
                       True -> logLevel logQ Info "Refusing to mount, already mounted!"
                       False -> do
                         let name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                             newSet = Set.insert fp mount
                             directory = takeDirectory $ "/host" </> name
                         atomically $ writeTVar mounts newSet
                         createDirectoryIfMissing True directory
                         Mount.bind (path </> fp) $ "/host" </> name
             BindDiffPath fp to -> do
                 case Set.member to mount of
                      True -> logLevel logQ Info "Already mounted"
                      False -> do
                         let name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                             newSet = Set.insert to mount
                             directory = takeDirectory $ name
                         atomically $ writeTVar mounts newSet
                         createDirectoryIfMissing True directory
                         Mount.bind (path </> name) $ to

             UnbindABS fp -> do
                 case Set.member fp mount of
                   False -> logLevel logQ Info "Refusing to unmount, not mounted"
                   True -> do
                       let name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                       let newSet = Set.delete fp mount
                       Mount.umount fp
                       -- Make sure we remove the host binds
                       Mount.umount $ path </> name
                       atomically $ writeTVar mounts newSet
             Unbind fp -> do
                 case Set.member fp mount of
                   False -> logLevel logQ Info "Refusing to unmount, not mounted"
                   True -> do
                     let name = joinPath $ filter (\x -> x /= "/") $ splitPath fp
                         newSet = Set.delete fp mount
                     atomically $ writeTVar mounts newSet
                     Mount.umount $ "/host" </> name
                     -- Make sure we remove the host binds
                     Mount.umount $ path </> name
       Just (UDevEvent action node) -> do
           let fileName = joinPath $ filter (\x -> x /= "/") $ splitPath $ T.unpack $ unNode node
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
       Nothing -> logLevel logQ Info $ "Rejecting message: " <> (T.pack $ show $ msgB)
