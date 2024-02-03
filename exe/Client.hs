module Main where
import ContainerManager.Client
import Network.HostName
import qualified Data.Text as T

main :: IO ()
main = (setupClient . T.pack) =<< getHostName
