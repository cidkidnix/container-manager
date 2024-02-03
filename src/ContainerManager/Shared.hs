{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
module ContainerManager.Shared where

import ContainerManager.Types

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Monad
import Control.Exception.Base
import qualified Data.Text as T
import Data.Text (Text)
import qualified System.UDev as UDev
import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-strin
import Control.Concurrent.STM hiding (retry)
import GHC.IO.Exception
import qualified Data.Aeson as A

sendMessage :: A.ToJSON a => Socket -> a -> IO ()
sendMessage sock msg = do
    let size = MsgSize $ BS.length encodedMsg
        eSize = BS.toStrict $ A.encode size
    void $ safeSend sock eSize $ do
      (_ :: Maybe ACK) <- (A.decode . BS.fromStrict) <$> recv sock 256
      void $ safeSend sock encodedMsg $ pure ()
    where
        encodedMsg :: ByteString
        encodedMsg = BS.toStrict $ A.encode msg

recvMessage :: Socket -> IO (Maybe ByteString)
recvMessage sock = do
    size <- recv sock 32
    let (size' :: Maybe MsgSize) = A.decode $ BS.fromStrict size
    (r :: Either IOException Int) <- try $ send sock (BS.toStrict $ A.encode ACK)
    case r of
      Left _ -> do
          throw $ IOError Nothing Interrupted "" "" Nothing Nothing
      Right _ -> do
        case size' of
          Nothing -> pure Nothing
          Just sizeF -> do
            msg <- recv sock ((unMsgSize sizeF) * 2)
            pure $ Just msg

queueMessages :: Socket -> TQueue (ByteString, Socket) -> IO ()
queueMessages conn queue = do
    (msg :: (Maybe ByteString)) <- recvMessage conn
    case msg of
      (Just a) -> void $ forkIO $ atomically $ do
          writeTQueue queue (a, conn)
      Nothing -> do
          pure ()

clientMessageServer :: Socket -> TQueue (ByteString, Socket) -> IO ()
clientMessageServer sock queue = void $ forkIO $ forever $ do
    queueMessages sock queue

messageServer :: Socket -> TQueue (ByteString, Socket) -> TQueue Message -> IO ()
messageServer sock queue outboundQ = void $ do
    (exception :: TQueue ThreadId) <- newTQueueIO
    void $ forkIO $ forever $ do
      (a :: Either IOException (Socket, SockAddr)) <- try $ accept sock
      case a of
        Left _ -> pure ()
        Right (conn, _peer) -> do
            outboundQueue conn outboundQ
            rec
              (t :: ThreadId) <- forkOS $ forever $ do
                (attempt :: Either IOException ()) <- try $ queueMessages conn queue
                case attempt of
                    Left _ -> do
                        atomically $ do
                            writeTQueue exception t
                    Right _ -> pure ()

            void $ forkIO $ forever $ do
              thread <- atomically $ readTQueue exception
              close conn
              killThread thread


safeSend :: Socket -> ByteString -> (IO () -> IO ())
safeSend sock msg f = do
    (r :: Either IOException ()) <- try $ sendAll sock msg
    case r of
      Left _  -> pure ()
      Right _ -> f

second :: Int
second = 1000000

convertAction :: UDev.Action -> Action
convertAction = \case
    UDev.Add -> Add
    UDev.Remove -> Remove
    _ -> error "Not yet implemented!"

convertNode :: BS.ByteString -> Node
convertNode = Node . T.pack . BLU.toString . BS.fromStrict

convertSubsystem :: BS.ByteString -> Text
convertSubsystem = T.pack . BLU.toString . BS.fromStrict

outboundQueue :: Socket -> TQueue Message -> IO ()
outboundQueue conn queue = void $ forkIO $ forever $ do
        msg <- atomically $ readTQueue queue
        sendMessage conn msg

sendMessageQ :: TQueue Message -> Message -> IO ()
sendMessageQ queue msg = do
    atomically $ writeTQueue queue $ msg
