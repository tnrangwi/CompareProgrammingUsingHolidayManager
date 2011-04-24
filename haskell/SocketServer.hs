{-# LANGUAGE ScopedTypeVariables #-} -- Needed to catch exceptions properly
-- | Author: Thorsten Rangwich
-- | See file LICENSE for details on using this code.
-- | This module contains a TCP/IP socket server that executes functions
-- | over a network socket. A particular protocol is used to send a one
-- | line request with a function name and parameters. The function is
-- | executed and the result sent back to the caller. Results are provided
-- | in a table like form, where each row may even contain a different number
-- | of columns.
-- | A particular function interface is specified. Users of the socket server
-- | implement these non network functions with this protocol and the server provides
-- | them over the network. Every function is a either a pure function or a pair
-- | of a pure function and an IO function. The pure function returns a possibly
-- | modified state dictionary, but is not allowed to do IO.
-- | In the case of a pair of functions, the pure function returns the modified
-- | state dictionary *and* a number of arguments the non pure function can
-- | read as commands. The non pure function is not allowed to return a modified
-- | state but its is effect are side effects only.
-- | There is a special case of a shutdown function (pure function), that returns
-- | a flag, if the server shall shut down.
-- | Once started, the server waits for network calls and calls the registered
-- | network functions until receiving a shutdown call.
-- | By default, a shutdown is initiated if the server receives a function call
-- | named shutdown. You can override by defining a function named shutdown.
-- | 
module SocketServer
(
 -- * Public API functions
 serveSocketUntilShutdown,
 pushHandler,
 -- * Type declarations for puclic API
 HandlerFunc,
 SyncFunc,
 -- * API interface
 -- ** Initialisation arguments
 ConnectionArguments(..),
 -- **Default arguments
 connectionDefault,
 -- * Implementation API (you will not need that)
 -- ** Argument type declarations
 SocketFunction(..),
 FunctionRegistry,
 -- ** Default separator
 separator,
 -- ** Charactor marking really empty lines
 nullCharacter,
 -- * Test functions
 test
)
where

import qualified Network.Socket as Socket
import qualified System.IO as SysIO
import qualified Data.String.Utils as StringUtils
import qualified Data.Map as Map
import qualified BaseTools
import qualified Control.Exception as Exception

-- | Separator used by network functions. Only a native client will need that. 
-- Better use an integrated client from here (not yet implemented).
separator :: Char
separator = '|'

-- | Character sent within a line, if the line originally was empty. This is to distinguish empty lines
-- | from lines containing really no fields.
nullCharacter :: Char
nullCharacter = '\x1f'

-- | Prototype of pure network function.
-- 
-- * State dictionary 
-- 
-- * Privileged access
-- 
-- * List of command arguments
-- 
-- * Return: Updated state dictionary, result table, update info
type HandlerFunc = BaseTools.Dictionary -> Bool -> [String] -> (BaseTools.Dictionary,[[String]], [[String]])

-- | Prototype of a network function that may shutdown the server.
-- | This prototype is very similar to the pure network function, but returns an additional value signalling
-- | a shutdown after processing. You do not need one, but should define one named "shutdown" if you don't want
-- | the server to shutdown if it reveices a function call with a function not in the registry and named
-- | "shutdown". Arguments:
-- |
-- | * State dictionary
-- |
-- | * Privileged flag
-- |
-- | * Return: List of result (empty, if not necessary), Information for synchronisation function, shutdown flag
type ShutdownFunc = BaseTools.Dictionary -> Bool -> [String] -> ([[String]], [[String]], Bool)

-- | Prototype of network function doing file synchrisations if necessary. Takes this arguments:
-- |
-- | * State dictionary
-- |
-- | * Update commands
-- |
-- | * Update commands
-- |
-- | * State dictionary
type SyncFunc = [[String]] -> BaseTools.Dictionary -> IO ()

-- | Socket function type. Either a pure function or a pair of a pure function and a non pure function.
data SocketFunction = SyncLess HandlerFunc -- ^ Only pass a pure function.
                    | Syncing HandlerFunc SyncFunc -- ^ Pass pure function and synchronisation function
                    | SyncLessShutdown ShutdownFunc -- ^ Pure function may signal shutdown, no synchronisation
                    | SyncingShutdown ShutdownFunc SyncFunc -- ^ Pure function may signal shutdown, synchronise eventually

-- | Map by name to a socket function.
type FunctionRegistry = Map.Map String SocketFunction


-- | Argument type for serveSocketUntilShutdown function
data ConnectionArguments  = ConnectionArguments { 
      serverName :: String, -- ^ Server name or IP address
      portName :: String, -- ^ Port as string or service name
      privileged :: [String] -- ^ List of privileged addresses
    } deriving Show

-- | Internal type to return a function for synchronisation and the data that function will need.
-- | The data is in a format the pure function would return. The function is packaged together with
-- | the pure function given on service initialisation.
data SyncingPair = SyncingPair SyncFunc [[String]]
                 | NoSync

-- | Extract contents from syncing pair. One could define defaults for NoSync, but
-- | this really is not needed.
syncData :: SyncingPair -- ^ Package of function and data
         -> [[String]] -- ^ The data
syncData (SyncingPair _ d) = d

-- | Extract function from the syncing pair. One could as well define a default here for NoSync,
-- | but we really do not need it currently.
syncFunc :: SyncingPair -- ^ Package of function and data
         -> SyncFunc -- ^ The function
syncFunc (SyncingPair f _) = f

-- | Default arguments. Use record syntax to update with your settings.
connectionDefault :: ConnectionArguments
connectionDefault = ConnectionArguments { 
                      serverName = "localhost"
                    , portName = "1970"
                    , privileged = []
                    }


-- | Add pair of handler functions to registry
pushHandler :: FunctionRegistry -- ^ Current function registry
            -> String           -- ^ Name of this function to register
            -> SocketFunction   -- ^ SocketFunction to map
            -> FunctionRegistry -- ^ Updated function registry
pushHandler r n f = case Map.lookup n r of
                      Nothing -> Map.insert n f r
                      _ -> error "Override handler function not supported. Most probably implementation error"


-- | Accept a registry of functions, a state dictionary, a server name and port/service name and
-- a list of privileged addresses and execute the functions as requested via the TCP socket.
serveSocketUntilShutdown :: FunctionRegistry -- ^ Registry of functions to execute via the socket.
                         -> BaseTools.Dictionary -- ^ State dictionary passed through all calls.
                         -> ConnectionArguments -- ^ Connection arguments
                         -> IO ()
serveSocketUntilShutdown registry storage server = do
  addrs <- Socket.getAddrInfo
                    (Just (Socket.defaultHints { Socket.addrFlags = [Socket.AI_CANONNAME],
                                        Socket.addrFamily = Socket.AF_INET,
                                        Socket.addrSocketType = Socket.Stream }))
                    (Just $ serverName server)
                    (Just $ portName server)
  let addr = head addrs
  sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr) 
  Socket.bindSocket sock (Socket.addrAddress addr)
  Socket.listen sock 5
  privList <- mapM Socket.inet_addr $ privileged server
  print "Waiting for clients..."
  _serveSocketRequest sock registry storage privList


-- | Extract address part from socket address. There should be a Network.Socket function for that ?!
_getAddressPart :: Socket.SockAddr -> Socket.HostAddress
_getAddressPart s = case s of
                      Socket.SockAddrInet port address -> address
                      _ -> error "Only IPV4 connections supported"


-- | Process one request via socket. This is called blocking.
_serveSocketRequest :: Socket.Socket -- ^ Bound socket
                   -> FunctionRegistry -- ^ Function registry
                   -> BaseTools.Dictionary -- ^ Current state.
                   -> [Socket.HostAddress] -- ^ List of privileged addresses
                   -> IO ()
_serveSocketRequest sock registry storage privilegedAddresses =
  -- (error "xyz") `catches` [Handler (\ (e :: ErrorCall) -> print e)]
  -- catch (using catches, but this maybe what Prelude.catch does): 
                           -- IOException
                           -- NoMethodError (if using Dynamic?)
                           -- PatternMatchFail
                           -- ErrorCall
  -- catch most errors and print them
  -- (mem, shutdown, caught) <- catch _handleSocketRequest (\e -> return (storage, False, (show e)))
  let handleError e = print ("Error in socket function:" ++ show e) >> return (storage, False)
      handleRequest = do
        (conn, addr) <- Socket.accept sock
        let addrPart = _getAddressPart addr
        let privileged = addrPart `elem` privilegedAddresses
        print $ "Incoming connection:" ++ show addr ++ ", privileged:" ++ show privileged
        hdl <- Socket.socketToHandle conn SysIO.ReadWriteMode
        SysIO.hSetBuffering hdl SysIO.LineBuffering
        line <- SysIO.hGetLine hdl
        let strippedLine = BaseTools.splitBy separator $ take (length line - 1 ) line -- strip trailing \r, break into pieces
        let command = head strippedLine
        let arguments = drop 1 strippedLine
        print $ "Got command:" ++ command ++ ", args:" ++ show arguments
        let (newMem, dbg, res, sync, sdown) = case Map.lookup command registry of
                                                Nothing -> (storage
                                                           , Just $ "Function " ++ command ++ " not mapped, ignored"
                                                           , []
                                                           , NoSync
                                                           , command=="shutdown")
                                                Just func -> let (s,m,r,sy,f) = _servePureCall func arguments storage privileged
                                                             in (s, m, r, sy, f)
        case dbg of
          Just message -> print message
          Nothing -> return ()
        case sync of
          NoSync -> return ()
          _ -> syncFunc sync (syncData sync) newMem
        _writeOutput hdl res
        SysIO.hClose hdl
        return (newMem, sdown) 
  in do
    -- [Handler (\ (e :: ErrorCall) -> print e)]
    (mem, shutdown) <- Exception.catches handleRequest [
                        Exception.Handler (\ (e :: Exception.ErrorCall) -> handleError e),
                        Exception.Handler (\ (e :: Exception.IOException) -> handleError e)]
    if shutdown then print "Shutdown request received." else _serveSocketRequest sock registry mem privilegedAddresses


-- | Internal function. Press one pure network function request
_servePureCall :: SocketFunction -- ^ Socket function to execute
               -> [String] -- ^ Arguments for socket function
               -> BaseTools.Dictionary -- ^ State dictionary
               -> Bool -- ^ Privileged flag
               -> (BaseTools.Dictionary, Maybe String, [[String]], SyncingPair, Bool)
_servePureCall (SyncLess handler) args storage priv =
    let (s, res,  _) = handler storage priv args
    in (s, Nothing, res, NoSync, False)
_servePureCall (Syncing handler sync) args storage priv =
    let (s, res, sy) = handler storage priv args
    in (s, Nothing, res, SyncingPair sync sy, False)
_servePureCall (SyncLessShutdown handler) args storage priv =
    let (res, _, sd) = handler storage priv args
    in (storage, Just "Shutdown", res, NoSync, sd)
_servePureCall (SyncingShutdown handler sync) args storage priv =
    let (res, sy, sd) = handler storage priv args
    in (storage, Nothing, res, SyncingPair sync sy, sd)

-- | Write result list via socket to client. Every call is terminated with an empty line terminated by \r\n.
-- | The implementation simply does a fold from the right printing any row and terminating the result.
_writeOutput ::  SysIO.Handle -- ^ Socket handle
             -> [[String]] -- ^ Result table (possibly not rectangular)
             -> IO ()
_writeOutput fd =
    foldr
     (\x ->
      (>>)
      (if null x then SysIO.hPutStr fd (nullCharacter:"\r\n")
       else SysIO.hPutStr fd $ BaseTools.mergeWith separator x ++ "\r\n")
     )
     (SysIO.hPutStr fd "\r\n")

test :: IO ()
test = serveSocketUntilShutdown Map.empty BaseTools.emptyConfig connectionDefault
