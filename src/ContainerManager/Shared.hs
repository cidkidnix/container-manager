{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module ContainerManager.Shared where

import ContainerManager.Types

import Network.Socket
import Network.Socket.ByteString
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Binary as BN
import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Monad
import Control.Exception.Base
import qualified Control.Exception as E
import qualified Data.Text as T
import Data.Text (Text)
import Data.IORef
import qualified System.UDev as UDev
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-strin

sendMessage :: ToJSON a => Socket -> a -> IO ()
sendMessage sock msg = do
    let size = BS.length encodedMsg
        eSize = BS.toStrict $ BN.encode size
    void $ safeSend sock eSize $ do
      (ack :: Maybe ACK) <- (A.decode . BS.fromStrict) <$> recv sock 1024
      ackFailed ack
      void $ safeSend sock encodedMsg $ pure ()
    where
        ackFailed = \case
            Just ACK -> do
                pure ()
            _ -> close sock
        encodedMsg :: ByteString
        encodedMsg = BS.toStrict $ A.encode msg

recvMessage :: FromJSON a => Socket -> IO (Maybe a)
recvMessage sock = do
    (size'' :: Either IOException ByteString) <- try $ recv sock 256
    case size'' of
      Left _ -> do
          close sock
          pure Nothing
      Right size -> do
        let (size' :: Int) = BN.decode $ BS.fromStrict size
        (r :: Either IOException Int) <- try $ send sock (BS.toStrict $ A.encode ACK)
        case r of
          Left _ -> do
              close sock
              pure Nothing
          Right _ -> do
            (msg' :: Either IOException ByteString) <- try $ recv sock (size' + 1)
            case msg' of
              Left _ -> do
                  close sock
                  pure Nothing
              Right msg -> pure $ A.decode $ BS.fromStrict $ msg

queueMessages :: Socket -> IORef [(Message, Socket)] -> IO ()
queueMessages conn queue = do
    (msg :: Maybe a) <- recvMessage conn
    case msg of
      Just Shutdown -> close conn
      Just a -> do
        atomicModifyIORef queue $ \x -> ((a, conn):x, ())
      _ -> close conn

clientMessageServer :: Socket -> IORef [(Message, Socket)] -> IO ()
clientMessageServer sock queue = void $ forkIO $ forever $ queueMessages sock queue

messageServer :: Socket -> IORef [(Message, Socket)] -> IO ()
messageServer sock queue = void $ forkIO $ forever $
    E.bracketOnError (accept sock) (close . fst)
      $ \(conn, _peer) -> void $ forkOS $ forever $ queueMessages conn queue

safeSend :: Socket -> ByteString -> (IO () -> IO ())
safeSend sock msg f = do
    (r :: Either IOException Int) <- try $ send sock msg
    case r of
      Left _  -> close sock
      Right _ -> f


logContainer :: LogLevel -> Text -> Text -> IO ()
logContainer level container msg = putStrLn $ (T.unpack (withColor level ("[" <> prettyName level <> "]"))) <> "[" <> T.unpack container <> "] " <> T.unpack msg

logLevel :: LogLevel -> Text -> IO ()
logLevel lvl msg = putStrLn $ (T.unpack (withColor lvl ("[" <> prettyName lvl <> "] "))) <> T.unpack msg

second :: Int
second = 1000000

convertAction :: UDev.Action -> Action
convertAction = \case
    UDev.Add -> Add
    UDev.Remove -> Remove
    _ -> error "Not yet implemented!"

convertNode :: BS.ByteString -> Node
convertNode = Node . T.pack . BLU.toString . BS.fromStrict
