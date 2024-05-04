import ContainerManager.Types
import ContainerManager.Shared

import qualified Data.Text as T
import Network.Socket

import Options.Applicative

data Cli = Cli
  { container :: String
  , scommand :: Command
  } deriving (Show, Eq, Ord)

data Command
  = BindCli String
  | UnbindCli String
  deriving (Show, Eq, Ord)

programOptions :: Parser Cli
programOptions =
    Cli
      <$> strOption
        ( long "container"
            <> short 'c'
            <> metavar "CONTAINER"
            <> value "default"
            <> help "Container to operate on"
            <> showDefault
        )
      <*> hsubparser
        ( bindCommand
        <> unbindCommand )

bindCommand :: Mod CommandFields Command
bindCommand =
    command "bind"
    (info bindOpts (progDesc "Bind path to container"))

bindOpts :: Parser Command
bindOpts =
    BindCli
      <$> strArgument
         ( metavar "PATH"
             <> help "Path to bind into container")

unbindCommand :: Mod CommandFields Command
unbindCommand =
    command "unbind"
    (info unbindOpts (progDesc "unbind path from container"))

unbindOpts :: Parser Command
unbindOpts =
    UnbindCli
      <$> strArgument
         ( metavar "PATH"
             <> help "Path to unbind from container")


withSocketCli :: (Socket -> IO ()) -> IO ()
withSocketCli f = do
    containerConnect <- socket AF_UNIX (GeneralSocketType 1) 1
    connect containerConnect (SockAddrUnix "/tmp/container-manager-cli.sock")
    f containerConnect
    close containerConnect


main :: IO ()
main = cli

cli :: IO ()
cli = withSocketCli $ \cliSocket -> do
    opts <- execParser fullOptions
    let container' = container opts
    case (scommand opts) of
      BindCli fp -> do
          sendMessage cliSocket $
              FileEvent (Container $ T.pack container') $ Bind $ fp
      UnbindCli fp -> do
          sendMessage cliSocket $
            FileEvent (Container $ T.pack container') $ Unbind $ fp

    print (container opts)

  where
      fullOptions = info (programOptions <**> helper)
        (fullDesc
        <> progDesc "ContainerManager helper"
        <> header "container-manager - centralized container management")
