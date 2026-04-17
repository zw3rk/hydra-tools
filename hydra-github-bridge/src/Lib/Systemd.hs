{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lib.Systemd (notifyReady, notifyWatchdog) where

#ifdef linux_HOST_OS
import System.Systemd.Daemon qualified as Systemd
#endif

notifyReady :: IO ()
#ifdef linux_HOST_OS
notifyReady = Systemd.notifyReady >> pure ()
#else
notifyReady = pure ()
#endif

notifyWatchdog :: IO ()
#ifdef linux_HOST_OS
notifyWatchdog = Systemd.notifyWatchdog >> pure ()
#else
notifyWatchdog = pure ()
#endif
