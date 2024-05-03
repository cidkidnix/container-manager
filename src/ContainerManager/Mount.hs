module ContainerManager.Mount where

import ContainerManager.Types

import System.Process
import System.Directory

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
