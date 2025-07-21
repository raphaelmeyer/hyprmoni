{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Json
import qualified Monitors
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Environment as System
import qualified System.Exit as System

main :: IO ()
main = do
  maybePath <- System.lookupEnv "XDG_RUNTIME_DIR"
  maybeSignature <- System.lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
  case (maybePath, maybeSignature) of
    (Just path, Just signature) -> allMonitors (buildSocketAddress path signature)
    _ -> do
      putStrLn "Hyprland socket not found."
      System.exitFailure

allMonitors :: Socket.SockAddr -> IO ()
allMonitors address = Socket.withSocketsDo $ do
  socket <- Socket.socket Socket.AF_UNIX Socket.Stream 0
  Socket.connect socket address
  Socket.sendAll socket "-j/monitors all"
  monitors <- recvAll socket
  let json = Json.parse monitors
  print $ Monitors.info json

buildSocketAddress :: String -> String -> Socket.SockAddr
buildSocketAddress path signature = Socket.SockAddrUnix $ path ++ "/hypr/" ++ signature ++ "/.socket.sock"

recvAll :: Socket.Socket -> IO Text.Text
recvAll socket = do
  content <- Socket.recv socket 1024
  if BS.null content
    then pure ""
    else do
      remaining <- recvAll socket
      pure $ Text.append (Text.pack . BS.unpack $ content) remaining
