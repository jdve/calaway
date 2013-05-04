{-# LANGUAGE DeriveGeneric #-}

module CalendarAway (
    pluginInit,
    Context
) where

import Control.Concurrent
import Control.Concurrent.Timer
import Control.Concurrent.STM.TVar
import Control.Concurrent.Suspend
import Control.Monad
import Control.Monad.STM
import Data.Aeson (eitherDecode, FromJSON, DotNetTime, fromDotNetTime)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Data
import Data.Maybe
import Data.Time
import GHC.Generics (Generic)
import Network.IRC.XChat.Plugin
import System.FilePath (joinPath)
import System.Locale (defaultTimeLocale)
import System.Process (readProcessWithExitCode)

norm :: PriorityA
norm = abstractPriority Norm

eatAll :: Eating
eatAll = Eating { eatPlugin = True, eatXChat = True }

eatNone :: Eating
eatNone = Eating { eatPlugin = False, eatXChat = False }

data Event = FromTo
    {
        from :: DotNetTime,
        to :: DotNetTime
    } deriving (Generic, Show)

instance FromJSON Event

data Context = Context
    {
        tevts :: TVar (Either String [Event]),
        timer :: TimerIO
    }

inMeeting :: UTCTime -> Event -> Bool
inMeeting now evt = ((fromDotNetTime $ from evt) <= now) && ((fromDotNetTime $ to evt) >= now)

isBusy :: UTCTime -> [Event] -> Bool
isBusy now evts = foldr (\ evt accum -> accum || (inMeeting now evt)) False evts

busyUntil :: UTCTime -> [Event] -> UTCTime
busyUntil now evts = foldr lastMeeting now evts
    where lastMeeting = (\ evt accum -> if inMeeting accum evt
                                        then (max accum (fromDotNetTime $ to evt))
                                        else accum)

updateCalendar :: Context -> String -> IO ()
updateCalendar ctx dir = do
    let script = joinPath [dir, "addons", "get-events.ps1"]

    (_, stdout, stderr) <- readProcessWithExitCode "powershell" [script] ""

    let result = case eitherDecode (B.pack stdout) of
                    Left err -> Left ("Failed to parse calendar: " ++ stderr ++ "\n\n" ++ stdout ++ "\n\n" ++ err)
                    Right evts -> Right evts

    atomically $ writeTVar (tevts ctx) $ result

    oneShotStart (timer ctx) (updateCalendar ctx dir) (mDelay 5)

    return ()

showCalendar :: XChatPlugin Context -> String -> Context -> IO (Eating, Context)
showCalendar ph _ ctx = do
    val <- atomically $ readTVar (tevts ctx)

    case val of
        Left err -> xChatPrint ph err
        Right evts -> xChatPrint ph (show evts)

    return (eatAll, ctx)

checkBusy :: XChatPlugin Context -> () -> Context -> IO (Eating, Context)
checkBusy ph _ ctx = do
    now <- getCurrentTime
    away <- xChatGetInfo ph "away"
    tz <- getCurrentTimeZone
    val <- atomically $ readTVar (tevts ctx)

    case val of
        Left err -> return ()
        Right evts -> do
            let busy = isBusy now evts
                local = (utcToZonedTime tz $ busyUntil now evts)
                formatted = formatTime (defaultTimeLocale) "%I:%M%P %Z" local

            when (busy && (isNothing away)) (xChatCommand ph ("AWAY In meetings until " ++ formatted))
            when ((not busy) && (isJust away)) (xChatCommand ph "BACK")

    return (eatAll, ctx)

pluginInit :: (Context -> IO (XChatPlugin Context)) -> IO PluginDescriptor
pluginInit fph =
    let pd = PluginDescriptor "CalendarAway" "Set away message from Exchange calendar" "0.1"
        noMemoryMgmt b = return ((), b)
    in

    do
       tevts <- atomically $ newTVar $ Right []
       timer <- newTimer

       let ctx = Context { tevts = tevts, timer = timer }

       ph <- fph ctx

       xChatHookCommand "Calendar" norm
                        (Just "Usage: CALENDAR, Shows calendar events for today")
                        ph (showCalendar ph) noMemoryMgmt
       xChatHookTimer (30*1000) ph (checkBusy ph) noMemoryMgmt
       xChatPrint ph "CalendarAway loaded successfully\n"

       Just dir <- xChatGetInfo ph "xchatdir"
       oneShotStart timer (updateCalendar ctx dir) (sDelay 10)

       return pd

