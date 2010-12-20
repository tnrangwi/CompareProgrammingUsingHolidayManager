#!/usr/bin/runghc
-- Author: Thorsten Rangwich
-- See file LICENSE for details on using this code
-- This is a sample test script to test the remote procedure call interface.
-- It starts up a sample server. Test have to be done through the client (probably telnet).
import qualified BaseTools
import qualified SocketServer
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- | Network function adding its input. The function returns the state dictionary unmodified.
add :: BaseTools.Dictionary -- ^ State dictionary passed into any function
    -> Bool -- ^ Not used here.
    -> [String] -- ^ List of string parameters passed into the function.
    -> (BaseTools.Dictionary,[[String]], [[String]]) -- ^ Unmodified state dictionary, result, commands to pass to IO function
add s _ xs = (s, [[show ((foldl (+) 0 . map read) xs)]], [])

-- | Network function to count its calls. This function is stateful and
-- | modifies the internal status that is passed into stateful functions and
-- | returned modified by them.
cnt :: BaseTools.Dictionary -- ^ State dictionary, with or without counter
    -> Bool -- ^ Not used here.
    -> [String] -- ^ One result row, one column: The new counter.
    -> (BaseTools.Dictionary,[[String]], [[String]]) -- ^ Modified counter in state dictionary, output, no IO commands.
cnt s _ _ = let cnt = (BaseTools.getInt (head (Maybe.fromMaybe [BaseTools.CfInt 0] (Map.lookup "cnt" s)))) + 1
            in (Map.insert "cnt" [BaseTools.CfInt cnt] s, [[show cnt]], [])
            
-- | Test function. Register network functions and start server.
main :: IO ()
main = do
  --let funcReg = Map.empty
  let storage = BaseTools.emptyConfig
  let connection = SocketServer.connectionDefault
  -- FIXME: this adds the last function only. Use foldl to add a number of functions.
  let funcReg = SocketServer.pushHandler Map.empty "add" (SocketServer.SyncLess add)
  let funcReg = SocketServer.pushHandler Map.empty "cnt" (SocketServer.SyncLess cnt)
  SocketServer.serveSocketUntilShutdown funcReg storage connection
