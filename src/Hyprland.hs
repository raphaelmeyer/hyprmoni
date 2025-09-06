{-# LANGUAGE OverloadedStrings #-}

module Hyprland (Event (..), Subscription, allMonitors, change, subscribe, unsubscribe) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Json
import qualified Monitors
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Environment as System
import qualified System.IO
import qualified Types

data Event = Event
  { evName :: Text.Text,
    evData :: Text.Text
  }

data Socket = Requests | Events

data Subscription = Subscription
  { subHandle :: System.IO.Handle,
    subTid :: Concurrent.ThreadId
  }

allMonitors :: IO [Types.MonitorInfo]
allMonitors = do
  address <- mkSocketAddress Requests
  response <- makeRequest address "-j/monitors all"
  case Json.parse response of
    Just info -> pure $ Monitors.info info
    Nothing -> Exception.throw Types.InvalidJson

change :: Text.Text -> Text.Text -> IO ()
change name mode = do
  address <- mkSocketAddress Requests
  Monad.void
    . makeRequest address
    . BS.pack
    . Text.unpack
    . Text.concat
    $ ["/keyword monitor ", name, ",", mode, ",auto,1"]

subscribe :: (Event -> IO ()) -> IO Subscription
subscribe onEvent = do
  handle <- listen
  System.IO.hSetBuffering handle System.IO.LineBuffering
  tid <- Concurrent.forkIO $ Monad.forever $ do
    line <- System.IO.hGetLine handle
    case mkEvent . Text.pack $ line of
      Just event -> onEvent event
      Nothing -> pure ()
  pure $ Subscription handle tid

unsubscribe :: Subscription -> IO ()
unsubscribe subscription = do
  Concurrent.killThread . subTid $ subscription
  System.IO.hClose . subHandle $ subscription

listen :: IO System.IO.Handle
listen = do
  address <- mkSocketAddress Events
  socket <- Socket.socket Socket.AF_UNIX Socket.Stream 0
  Socket.connect socket address
  Socket.socketToHandle socket System.IO.ReadWriteMode

mkEvent :: Text.Text -> Maybe Event
mkEvent raw =
  case split of
    [name, arguments] -> Just $ Event name arguments
    _ -> Nothing
  where
    split = Text.splitOn ">>" raw

mkSocketAddress :: Socket -> IO Socket.SockAddr
mkSocketAddress socket = do
  maybePath <- System.lookupEnv "XDG_RUNTIME_DIR"
  path <- case maybePath of
    Just p -> pure p
    Nothing -> Exception.throw $ Types.UndefinedEnvironmentVariable "XDG_RUNTIME_DIR"
  maybeSignature <- System.lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
  signature <- case maybeSignature of
    Just s -> pure s
    Nothing -> Exception.throw $ Types.UndefinedEnvironmentVariable "HYPRLAND_INSTANCE_SIGNATURE"
  pure . Socket.SockAddrUnix $ path ++ "/hypr/" ++ signature ++ "/" ++ name
  where
    name = case socket of
      Requests -> ".socket.sock"
      Events -> ".socket2.sock"

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
