{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
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
  foo <- Socket.recv socket 1024
  putStrLn $ BS.unpack foo

buildSocketAddress :: String -> String -> Socket.SockAddr
buildSocketAddress path signature = Socket.SockAddrUnix $ path ++ "/hypr/" ++ signature ++ "/.socket.sock"
