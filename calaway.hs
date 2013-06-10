{-# LANGUAGE ForeignFunctionInterface #-}
module Mcalaway () where
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Network.IRC.XChat.Plugin
import CalendarAway(Context, pluginInit)

xChatPluginInitM :: Ptr (XchatPlugin Context) ->
                      Ptr CString -> Ptr CString -> Ptr CString ->
                      CString -> IO CInt
xChatPluginInitM ph ps1 ps2 ps3 _ =
  do descr <- pluginInit (xChatPluginInit ph)
     s1 <- newCString (pluginName descr)
     s2 <- newCString (pluginDescription descr)
     s3 <- newCString (pluginVersion descr)
     poke ps1 s1
     poke ps2 s2
     poke ps3 s3
     return 1
foreign export ccall "xchat_plugin_init" xChatPluginInitM
  :: Ptr (XchatPlugin Context) ->
     Ptr CString -> Ptr CString -> Ptr CString -> CString -> IO CInt
foreign export ccall "hexchat_plugin_init" xChatPluginInitM
  :: Ptr (XchatPlugin Context) ->
     Ptr CString -> Ptr CString -> Ptr CString -> CString -> IO CInt

xChatPluginDeinitM :: IO CInt
xChatPluginDeinitM = return 1
foreign export ccall "xchat_plugin_deinit" xChatPluginDeinitM
  :: IO CInt
foreign export ccall "hexchat_plugin_deinit" xChatPluginDeinitM
  :: IO CInt
