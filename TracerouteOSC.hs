#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Clock
import Data.Word
import Data.Fixed
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Network.Socket{- network -}
import Network.Socket.ByteString (recv, sendAll, send, sendTo, recvFrom)
import Control.Concurrent
import Control.Monad
import System.Environment
import System.Timeout
import qualified Sound.OSC as OSC

type IpTuple = (Word8, Word8, Word8, Word8)
type PacketTry = Int
type PacketTTL = Int
type SendSock = Socket
type RecvSock = Socket
type PacketReply = (SockAddr, Pico)

maxHops :: Int
maxHops = 30

ip4ToTuple :: String -> Maybe IpTuple
ip4ToTuple s = let xs = ip4ToList s
               in case length xs of
                 4 -> Just $ ((xs!!0, xs!!1, xs!!2, xs!!3) :: IpTuple)
                 _ -> Nothing

ip4ToList :: String -> [Word8]
ip4ToList s = case dropWhile (== '.')  s of
                "" -> []
                s' -> w':(ip4ToList s'')
                  where (w, s'') = break (== '.') s'
                        w' = (read w) :: Word8
                        
inPort :: PortNumber
inPort = 33434

oscPort :: PortNumber
oscPort = 3333


triesPerTTL :: Int
triesPerTTL = 3

maxRecv :: Int
maxRecv = 1024 -- max bytes received

secToMicrosec :: Int -> Int
secToMicrosec s = s * 1000000

sendOSCTraced :: SendSock -> (Maybe PacketReply) -> IO ()
sendOSCTraced outSock preply = do
  case preply of 
    Nothing -> return ()
    Just x -> OSC.sendTo udpSock msg targetaddr
      where udpSock = OSC.UDP outSock
            timeElapsed = snd x
            addrStr = show $ fst x
            msg = OSC.p_message "/traced" [OSC.float timeElapsed, OSC.string addrStr]
            targetaddr = SockAddrInet oscPort $ tupleToHostAddress ((127,0,0,1) :: IpTuple)

  
udpSocket :: IO SendSock
udpSocket = do
  sock <- socket AF_INET Datagram 0
  setSocketOption sock ReuseAddr 1
  return sock


icmpSocket :: IO RecvSock
icmpSocket = do
  let hints = defaultHints {
        addrFlags = [AI_PASSIVE]
        }
  sock <- socket AF_INET Raw 1
  curAddr <- getAddrInfo (Just hints) Nothing (Just $ show inPort)
  -- bind sock $ addrAddress $ head curAddr
  return sock

respPrinter :: (Maybe PacketReply) -> PacketTry -> IO ()
respPrinter packetreply curtry = do
  case packetreply of
    Nothing -> putStrLn $ mconcat ["(", show curtry, ") * * * * *"]
    Just x -> putStrLn $ mconcat ["(", show curtry, ") ", show addr, " --- ", show elapsedTime, " ms"]
      where elapsedTime = snd x
            addr = fst x

packetSender :: SendSock -> RecvSock -> IpTuple -> PacketTTL -> IO (Maybe PacketReply)
packetSender outsock insock iptup curttl = do
  setSocketOption outsock TimeToLive curttl
  startTime <- getCurrentTime
  sendTo outsock (BSU.fromString "") $ SockAddrInet inPort (tupleToHostAddress iptup)
  ans <- timeout (secToMicrosec 2) $ recvFrom insock maxRecv
  stopTime <- getCurrentTime
  let timeElapsed = 1000 * (nominalDiffTimeToSeconds $ diffUTCTime stopTime startTime)
  case ans of
    Nothing -> return Nothing
    Just x -> return $ Just $ (snd x, timeElapsed)

packetHandler :: SendSock -> RecvSock -> IpTuple -> PacketTTL -> IO ()
packetHandler outsock insock iptup curttl = do
  mbreplies <- replicateM triesPerTTL $ packetSender outsock insock iptup curttl
  let mbreply = msum mbreplies
  respPrinter mbreply $ (curttl :: PacketTry)
  sendOSCTraced outsock mbreply
  return ()
  
targetHandler :: IpTuple -> IO ()
targetHandler iptup = do
  icmpSock <- icmpSocket
  udpSock <- udpSocket
  mapM_ (packetHandler udpSock icmpSock iptup) $ ([1..maxHops] :: [PacketTTL])
  return ()
  
main :: IO ()
main = do
  inStr <- fmap head getArgs
  case (ip4ToTuple inStr) of
    Nothing -> return ()
    Just cur -> targetHandler cur

