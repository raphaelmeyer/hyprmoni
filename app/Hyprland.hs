{-# LANGUAGE OverloadedStrings #-}

module Hyprland (allMonitors, change) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Json
import qualified Monitors
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Environment as System
import qualified Types

allMonitors :: IO [Types.MonitorInfo]
allMonitors = do
  address <- mkSocketAddress
  response <- makeRequest address "-j/monitors all"
  pure $ Monitors.info . Json.parse $ response

change :: Text.Text -> Text.Text -> IO ()
change name mode = do
  address <- mkSocketAddress
  Monad.void
    . makeRequest address
    . BS.pack
    . Text.unpack
    . Text.concat
    $ ["/keyword monitor ", name, ",", mode, ",auto,1"]

mkSocketAddress :: IO Socket.SockAddr
mkSocketAddress = do
  maybePath <- System.lookupEnv "XDG_RUNTIME_DIR"
  path <- case maybePath of
    Just p -> pure p
    Nothing -> Exception.throw $ Types.UndefinedEnvironmentVariable "XDG_RUNTIME_DIR"
  maybeSignature <- System.lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
  signature <- case maybeSignature of
    Just s -> pure s
    Nothing -> Exception.throw $ Types.UndefinedEnvironmentVariable "HYPRLAND_INSTANCE_SIGNATURE"
  pure . Socket.SockAddrUnix $ path ++ "/hypr/" ++ signature ++ "/.socket.sock"

makeRequest :: Socket.SockAddr -> BS.ByteString -> IO Text.Text
makeRequest address request = Socket.withSocketsDo $ do
  socket <- Socket.socket Socket.AF_UNIX Socket.Stream 0
  Socket.connect socket address
  Socket.sendAll socket request
  recvAll socket

recvAll :: Socket.Socket -> IO Text.Text
recvAll socket = do
  content <- Socket.recv socket 1024
  if BS.null content
    then pure ""
    else do
      remaining <- recvAll socket
      pure $ Text.append (Text.pack . BS.unpack $ content) remaining
