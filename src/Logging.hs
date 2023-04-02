{-# LANGUAGE CPP #-}

module Logging (trace,debug) where

import Control.Monad.Trans
import Data.IORef
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Locale
import System.Time


logFile :: Handle
logFile = unsafePerformIO $ do h <- openFile ".hscurses.log" AppendMode
                               debug_ h "logging initialized"
                               return h
{-# NOINLINE logFile #-}

formatTime :: IO String
formatTime =
    do let fmt = "%Y-%m-%d %H:%M:%S"
       clockT <- getClockTime
       calTime <- toCalendarTime clockT
       let maxSdecLen = 5
           sdec' = show $ ctPicosec calTime
           sdec = if length sdec' > maxSdecLen
                     then take maxSdecLen sdec'
                     else sdec'
       return (formatCalendarTime defaultTimeLocale fmt calTime
               ++ ":" ++ sdec)

trace :: String -> a -> a
trace s x =
    unsafePerformIO $ do debug s
                         return x
trace _ x = x

debug :: MonadIO m => String -> m ()
debug s = liftIO $ debug_ logFile s
debug _ = return ()

debug_ f s =
    do ts <- formatTime
       hPutStrLn f ("[" ++ ts ++ "] " ++ s)
       hFlush f



