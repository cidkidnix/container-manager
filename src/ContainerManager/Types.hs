{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ContainerManager.Types where

import GHC.Generics
import Network.Socket
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as Map

-- Forgive me
foreign import ccall "exit" exit :: IO ()

class Default a where
    def :: a

class PrettyName a where
    prettyName :: a -> Text

newtype MsgSize = MsgSize { unMsgSize :: Int }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON MsgSize
instance FromJSON MsgSize

data FileType = Folder
              | File
    deriving (Show, Eq, Ord, Generic)

instance ToJSON FileType
instance FromJSON FileType

data HostContext = HostContext
    { _hostMountRef :: TVar (Map Text (Set FilePath))
    , _hostHeartbeatRef :: TVar UTCTime
    , _hostQueue :: TQueue (ByteString, Socket)
    , _hostLog :: TQueue Text
    , _outboundQueue :: TQueue Message
    } deriving (Eq)

data ClientContext = ClientContext
    { _clientHeartbeatRef :: TVar UTCTime
    , _clientQueue :: TQueue (ByteString, Socket)
    , _clientLog :: TQueue Text
    , _clientMounts :: TVar (Set FilePath)
    } deriving (Eq)


data Message = FileEvent Container EventFile
             | RunCommand !([Text])
             | HeartBeat !UTCTime !Text
             | Setup !Text
             | Configure !Config
             | Acknowledge !ACK !Message
             | Shutdown
             | UDevEvent Action Node
             | MoveToSocket MoveTo
             deriving (Show, Eq, Ord, Generic)

instance ToJSON Message
instance FromJSON Message

newtype Container = Container { unContainer :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Container
instance FromJSON Container

data EventFile = Bind FilePath
               | BindDiffPath FilePath FilePath
               | Unbind FilePath
    deriving (Show, Eq, Ord, Generic)

instance ToJSON EventFile
instance FromJSON EventFile

newtype MoveTo = MoveTo { unMoveTo :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MoveTo
instance FromJSON MoveTo

newtype Node = Node { unNode :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Node
instance FromJSON Node

data Action = Remove
            | Add
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Action
instance FromJSON Action

data ContainerConfig = ContainerConfig
  { _filter_udev_events :: Bool
  , _udev_filters :: Maybe [String]
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON ContainerConfig
instance FromJSON ContainerConfig

data Config = Config
  { containerConfigs :: Map Text ContainerConfig
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Config
instance FromJSON Config

data LogLevel = Error
              | Warning
              | Info
              | Quiet
        deriving (Show, Eq, Ord, Generic)

data ACK = ACK | NACK
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ACK
instance ToJSON ACK

instance Default ContainerConfig where
    def = ContainerConfig {
      _filter_udev_events = True,
      _udev_filters = Just ["hidraw"]
    }

instance Default Config where
    def = Config {
      containerConfigs = Map.fromList [("default", def)]
    }

instance PrettyName Message where
    prettyName = \case
      RunCommand _ -> "RunCommand"
      HeartBeat _ _ -> "HeartBeat"
      Setup _ -> "Setup"
      Configure _ -> "Configuration"
      UDevEvent _ _ -> "UDevEvent"
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
