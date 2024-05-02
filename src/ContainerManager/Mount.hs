module ContainerManager.Mount where

import System.Process

bind :: FilePath -> FilePath -> IO ()
bind src dst = do
    callProcess "touch" [dst]
    callProcess "mount" [ "--bind", src, dst ]

umount :: FilePath -> IO ()
umount path = callProcess "umount" ["-R", path]
