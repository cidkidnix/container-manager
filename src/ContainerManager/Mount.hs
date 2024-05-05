{-# LANGUAGE ScopedTypeVariables #-}
module ContainerManager.Mount where

import ContainerManager.Types

import qualified System.Linux.Mount as Mount

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
    (res :: Either IOException ()) <- try $ bind src dst
    case res of
      Left _ -> putStrLn "Error!"
      Right _ -> pure ()

umount :: FilePath -> IO ()
umount fp = do
    (res :: Either IOException ()) <- try $ Mount.umount fp
    case res of
      Left _ -> putStrLn "Error!"
      Right _ -> pure ()

refreshMount :: FilePath -> FilePath -> IO ()
refreshMount src dst = do
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
    (exitcode, _, _) <- readProcessWithExitCode "findmnt" ["-T", fp] ""
    case exitcode of
      ExitSuccess -> pure True
      ExitFailure i -> case i of
        1 -> pure False
        _ -> do
            print "Mount stack is full"
            pure False
