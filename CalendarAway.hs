{-# LANGUAGE DeriveGeneric #-}

module CalendarAway (
    pluginInit,
    Context
) where

import Control.Exception
import Control.Monad
import Data.Aeson (eitherDecode, FromJSON, DotNetTime, fromDotNetTime)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Data
import Data.Maybe
import Data.Time
import Foreign.Ptr (nullPtr)
import GHC.Generics (Generic)
import Network.IRC.XChat.Plugin
import System.FilePath (joinPath)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import System.Win32.DLL (getModuleFileName)

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

data Context = Events [Event] deriving (Generic, Show)

instance FromJSON Event
instance FromJSON Context

inMeeting :: UTCTime -> Event -> Bool
inMeeting now evt = ((fromDotNetTime $ from evt) <= now) && ((fromDotNetTime $ to evt) >= now)

isBusy :: [Event] -> IO Bool
isBusy evts = do
    now <- getCurrentTime
    return (foldr (\ evt accum -> accum || (inMeeting now evt)) False evts)

busyUntil :: [Event] -> IO UTCTime
busyUntil evts = do
    now <- getCurrentTime
    return (foldr
        (\ evt accum ->
            if inMeeting accum evt
            then (max accum (fromDotNetTime $ to evt))
            else accum)
        now
        evts)

refreshCalendarCb :: XChatPlugin Context -> () -> Context -> IO (Eating, Context)
refreshCalendarCb ph _ ctx = do
    path <- getModuleFileName nullPtr
    let script = joinPath [path, "config", "addons", "get-events.ps1"]
    r <- try $ readProcess "powershell" [script] ""
    case r of
        Left e -> xChatPrint ph ("Failed to read calendar with " ++ script ++ ": " ++ show (e :: SomeException)) >> return (eatAll, ctx)
        Right json ->
            case (eitherDecode (B.pack json)) of
                Left error -> xChatPrint ph ("Failed to parse " ++ json ++ ": " ++ error) >> return (eatAll, ctx)
                Right evts -> return (eatAll, Events evts)

checkLockedCb :: XChatPlugin Context -> () -> Context -> IO (Eating, Context)
checkLockedCb ph _ (Events evts) = do
    busy <- isBusy evts
    until <- busyUntil evts
    away <- xChatGetInfo ph "away"
    tz <- getCurrentTimeZone

    let local = (utcToZonedTime tz until)
        formatted = formatTime (defaultTimeLocale) "%I:%M%P" local

    when (busy && (isNothing away)) (xChatCommand ph ("AWAY In meetings until " ++ formatted))
    when ((not busy) && (isJust away)) (xChatCommand ph "BACK")

    return (eatAll, (Events evts))

showCalendarCb :: XChatPlugin Context -> String -> Context -> IO (Eating, Context)
showCalendarCb ph _ (Events evts) =
    xChatPrint ph (show evts) >> return (eatAll, (Events evts))

pluginInit :: (Context -> IO (XChatPlugin Context)) -> IO PluginDescriptor
pluginInit fph =
    let pd = PluginDescriptor "CalendarAway" "Set away message from Exchange calendar" "0.1"
        noMemoryMgmt b = return ((), b)
    in

    do ph <- fph (Events [])
       xChatHookCommand "Calendar" norm
                        (Just "Usage: CALENDAR, Shows calendar events for today")
                        ph (showCalendarCb ph) noMemoryMgmt
       xChatHookTimer (15*60*1000) ph (refreshCalendarCb ph) noMemoryMgmt
       xChatHookTimer (30*1000) ph (checkLockedCb ph) noMemoryMgmt
       xChatPrint ph "CalendarAway loaded successfully\n"
       return pd

