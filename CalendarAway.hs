{-# LANGUAGE DeriveGeneric #-}

module CalendarAway (
    pluginInit,
    Context
) where

import Control.Monad
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

type Context = Either String [Event]

inMeeting :: UTCTime -> Event -> Bool
inMeeting now evt = ((fromDotNetTime $ from evt) <= now) && ((fromDotNetTime $ to evt) >= now)

isBusy :: UTCTime -> [Event] -> Bool
isBusy now evts = foldr (\ evt accum -> accum || (inMeeting now evt)) False evts

busyUntil :: UTCTime -> [Event] -> UTCTime
busyUntil now evts = foldr lastMeeting now evts
    where lastMeeting = (\ evt accum -> if inMeeting accum evt
                                        then (max accum (fromDotNetTime $ to evt))
                                        else accum)

getCalendar :: IO Context
getCalendar = do
    (_, stdout, stderr) <- readProcessWithExitCode "powershell" ["config/addons/get-events.ps1"] ""

    let ctx = case eitherDecode (B.pack stdout) of
                 Left err -> Left ("Failed to parse calendar: " ++ stderr ++ "\n\n" ++ stdout ++ "\n\n" ++ err)
                 Right evts -> Right evts

    return ctx

updateContext :: XChatPlugin Context -> () -> Context -> IO (Eating, Context)
updateContext ph _ _ = do
    ctx <- getCalendar
    return (eatAll, ctx)

showContext :: XChatPlugin Context -> String -> Context -> IO (Eating, Context)
showContext ph _ ctx = do
    case ctx of
        Left err -> xChatPrint ph err
        Right evts -> xChatPrint ph (show evts)

    return (eatAll, ctx)

checkBusy :: XChatPlugin Context -> () -> Context -> IO (Eating, Context)
checkBusy ph _ ctx = do
    now <- getCurrentTime
    away <- xChatGetInfo ph "away"
    tz <- getCurrentTimeZone

    case ctx of
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
       ctx <- getCalendar
       ph <- fph ctx

       xChatHookCommand "Calendar" norm
                        (Just "Usage: CALENDAR, Shows calendar events for today")
                        ph (showContext ph) noMemoryMgmt
       xChatHookTimer (30*1000) ph (checkBusy ph) noMemoryMgmt
       xChatHookTimer (15*60*1000) ph (updateContext ph) noMemoryMgmt
       xChatPrint ph "CalendarAway loaded successfully\n"

       return pd

