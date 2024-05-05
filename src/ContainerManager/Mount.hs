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

--bindWithRetry :: FilePath -> FilePath -> IO ()
--bindWithRetry src dst = do
--    ft <- determineFileType src
--    case ft of
--      File -> callProcess "touch" [dst]
--      Folder -> createDirectoryIfMissing True dst
--    (exitcode, _, _) <- readProcessWithExitCode "mount" ["--bind", src, dst]
--    case exitcode of
--      ExitSuccess -> pure ()
--      ExitFailure _ -> do
--          bindWithRetry src dst

umount :: FilePath -> IO ()
umount path = do
    (exitcode, _, _) <- readProcessWithExitCode "umount" ["-R", path] ""
    pure ()

determineFileType :: FilePath -> IO FileType
determineFileType fp = do
    r <- doesDirectoryExist fp
    case r of
      True -> pure Folder
      False -> pure File

alreadyMounted :: FilePath -> IO Bool
alreadyMounted fp = do
    (exitcode, _, _) <- readProcessWithExitCode "findmnt" ["-T", fp] ""
    case exitcode of
      ExitSuccess -> pure True
      ExitFailure i -> case i of
        1 -> pure False
        _ -> do
            print "Mount stack is full"
            pure False
