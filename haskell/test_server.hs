#!/usr/bin/runghc
-- Author: Thorsten Rangwich
-- See file LICENSE for details on using this code
-- This is a sample test script to test the remote procedure call interface.
-- It starts up a sample server. Test have to be done through the client (probably telnet).
import qualified BaseTools
import qualified SocketServer
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

add :: BaseTools.Dictionary -> Bool -> [String] -> (BaseTools.Dictionary,[[String]], [[String]])
add s _ xs = (s, [[show ((foldl (+) 0 . map read) xs)]], [])

cnt :: BaseTools.Dictionary -> Bool -> [String] -> (BaseTools.Dictionary,[[String]], [[String]])
cnt s _ _ = let cnt = (BaseTools.getInt (head (Maybe.fromMaybe [BaseTools.CfInt 0] (Map.lookup "cnt" s)))) + 1
            in (Map.insert "cnt" [BaseTools.CfInt cnt] s, [[show cnt]], [])
            

main :: IO ()
main = do
  --let funcReg = Map.empty
  let storage = BaseTools.emptyConfig
  let connection = SocketServer.connectionDefault
  let funcReg = SocketServer.pushHandler Map.empty "add" (SocketServer.SyncLess add)
  let funcReg = SocketServer.pushHandler Map.empty "cnt" (SocketServer.SyncLess cnt)
  SocketServer.serveSocketUntilShutdown funcReg storage connection

  