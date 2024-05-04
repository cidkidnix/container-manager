module ContainerManager.Mount where

import ContainerManager.Types

import System.Process
import System.Directory
import System.Exit

bind :: FilePath -> FilePath -> IO ()
bind src dst = do
    ft <- determineFileType src
    case ft of
      File -> callProcess "touch" [dst]
      Folder -> createDirectoryIfMissing True dst
    callProcess "mount" [ "--bind", src, dst ]

umount :: FilePath -> IO ()
umount path = callProcess "umount" ["-R", path]

determineFileType :: FilePath -> IO FileType
determineFileType fp = do
    r <- doesDirectoryExist fp
    case r of
      True -> pure Folder
      False -> pure File

alreadyMounted :: FilePath -> IO Bool
alreadyMounted fp = do
    let args = proc "findmt" ["-T", fp]
    (exitcode, _, _) <- readProcessWithExitCode "findmnt" ["-T", fp] ""
    case exitcode of
      ExitSuccess -> pure True
      ExitFailure i -> case i of
        1 -> pure False
        _ -> do
            print "Mount stack is full"
            pure False
