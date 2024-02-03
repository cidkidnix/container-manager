{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ContainerManager.Types where

import GHC.Generics
import Network.Socket
import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import Data.Aeson hiding (Error)
import Data.Text (Text)
import Data.Time

-- Forgive me
foreign import ccall "exit" exit :: IO ()

class Default a where
    def :: a

class PrettyName a where
    prettyName :: a -> Text

data HostContext = HostContext
    { _hostMountRef :: IORef (Map Text (Set FilePath))
    , _hostHeartbeatRef :: IORef (Map Text UTCTime)
    , _hostQueue :: IORef [(Message, Socket)]
    } deriving (Eq)

data ClientContext = ClientContext
    { _clientHeartbeatRef :: IORef UTCTime
    , _clientQueue :: IORef [(Message, Socket)]
    } deriving (Eq)

data Message = BindHost FilePath FilePath Text
             | UnbindHost FilePath Text
             | LinkContainer FilePath FilePath
             | RunCommand [Text]
             | HeartBeat UTCTime Text
             | Setup Text
             | Configure Config
             | Acknowledge ACK Message
             | Shutdown
             deriving (Show, Eq, Ord, Generic)

instance ToJSON Message
instance FromJSON Message

data Config = Config
  { _socket_dir :: FilePath
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Config
instance FromJSON Config

data LogLevel = Error
              | Warning
              | Info
              | Quiet
        deriving (Show, Eq, Ord, Generic)

instance ToJSON LogLevel
instance FromJSON LogLevel

data ACK = ACK | NACK
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ACK
instance FromJSON ACK

instance Default Message where
    def = LinkContainer "/tmp/test" "/srv/tmp/test"

instance Default Config where
    def = Config "/tmp/test.sock"

instance PrettyName Message where
    prettyName = \case
      BindHost _ _ _ -> "BindHost"
      UnbindHost _ _ -> "UnbindHost"
      LinkContainer _ _ -> "LinkContainer"
      RunCommand _ -> "RunCommand"
      HeartBeat _ _ -> "HeartBeat"
      Setup _ -> "Setup"
      Configure _ -> "Configuration"
      Acknowledge a msg -> case a of
        ACK -> "ACK " <> prettyName msg
        NACK -> "NACK " <> prettyName msg
      Shutdown -> "Shutdown"

instance PrettyName LogLevel where
    prettyName = \case
      Error -> "Error"
      Warning -> "Warning"
      Info -> "Info"
      Quiet -> "Quiet"

defaultC :: Text
defaultC = "\x1b[0m"

toColor :: LogLevel -> Text
toColor = \case
  Error -> "\x1b[31m"
  Warning -> "\x1b[33m"
  Info -> "\x1b[36m"
  Quiet -> ""

withColor :: LogLevel -> Text -> Text
withColor lvl msg = toColor lvl <> msg <> defaultC
