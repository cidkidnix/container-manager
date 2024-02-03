{-# LANGUAGE ScopedTypeVariables #-}
module ContainerManager.Mount where

import ContainerManager.Types

import Control.Exception
import System.Process
import System.Directory
import System.Exit

bind :: FilePath -> FilePath -> IO ()
bind src dst = do
    ft <- determineFileType src
    case ft of
      File -> callProcess "touch" [dst]
      Folder -> createDirectoryIfMissing True dst
    (exitcode, _, _) <- readProcessWithExitCode "mount" ["--bind", src, dst] ""
    pure ()
    --(res :: Either IOException ()) <- try $ bind src dst
    --case res of
    --  Left e -> print e
    --  Right _ -> pure ()

umount :: FilePath -> IO ()
umount fp = do
    (exitcode, _, _) <- readProcessWithExitCode "umount" ["-R", fp] ""
    pure ()

refreshMount :: FilePath -> FilePath -> IO ()
refreshMount src dst = do
    print src
    print dst
    umount dst
    bind src dst

determineFileType :: FilePath -> IO FileType
determineFileType fp = do
    r <- doesDirectoryExist fp
    case r of
      True -> pure Folder
      False -> pure File

alreadyMounted :: FilePath -> IO Bool
alreadyMounted fp = do
    (exitcode, _, _) <- readProcessWithExitCode "findmnt" [fp] ""
    print fp
    case exitcode of
      ExitSuccess -> pure True
      ExitFailure i -> case i of
        1 -> pure False
        _ -> do
            print "Mount stack is full"
            pure False
