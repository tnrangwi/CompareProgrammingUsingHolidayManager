-- | Author: Thorsten Rangwich
-- | See file LICENSE for details on using this code.
-- | This file contains the code needed for the holiday server:
-- | A server process maintaining holiday schedules of several
-- | people in the file system, being available via a network socket.
module HolidayServer
(
 startHolidayServer
)
where

-- FIXME: UsrSettings should have record format

import qualified BaseTools
import qualified SocketServer
import qualified Data.Map as Map
import qualified Data.Maybe as MayBe
import System.FilePath ((</>), addExtension)
import qualified System.IO.Error as IOError
import qualified System.IO as FileIO
import qualified System.Directory as SysDir
import qualified Control.Monad as MControl


-- | File extension for files containing user holidays.
usrExtension :: String
usrExtension = "usr"

-- | Separator used for the configuraton files.
fieldSeparator :: Char
fieldSeparator = '|'

-- | File suffix including dot for files containing user holidays
usrFileSuffix :: String
usrFileSuffix = '.' : usrExtension

-- | Data type used to express a list of user holidays. FIXME: How to express this in a safe data type? How
-- | is a data constructor restricted to a range of values? The second value only has tp be > 0, the
-- | first valua has to represent a valid date. How?
type UsrGroup = String


-- | Data type representing a single holiday schedule.
data Holiday = Holiday {
      firstOfHoliday :: BaseTools.DateInt, -- ^ Start date of holiday
      lengthOfHoliday :: BaseTools.PositiveInt -- ^ Number of days. Must be greater than zero.
    }
               deriving Show


-- | Function to extract last day of the desired holiday.
lastOfHoliday :: Holiday -- ^ Holiday to examine
              -> BaseTools.DateInt -- ^ Last day of the holiday.
lastOfHoliday (Holiday s d) = BaseTools.dateAdd s $ BaseTools.fromPositiveInt d - 1


-- | Settings of one single user. Consists of the user's group and the list of holidays.
data UsrSettings = UsrSettings { 
      usrGroup :: UsrGroup, -- ^ group of this user
      holidayList :: [Holiday] -- ^ list of holidays of this user
    }


-- | Read file containing settings for one single user.
readUsrFile :: String -- ^ File name.
            -> IO UsrSettings -- ^ User's settings. See type definition for details.
readUsrFile = MControl.liftM (readUsrFromList . lines) . FileIO.readFile


-- | Read user configuration file from a list of lines consisting of the file's contents.
readUsrFromList :: [String] -> UsrSettings
readUsrFromList (x:xs) = UsrSettings x (readUsrHolidays xs)


-- | Read in a one string containing start day and length of holiday in days, extract
-- | the corresponding holiday type-safe. Throws an exception if format does not match.
createHolidayFromString :: String -- ^ String like "20122512|12", separator is taken from SocketServer module.
                        -> Holiday -- ^ Packaged holiday.
createHolidayFromString s = let splitted =  BaseTools.splitBy fieldSeparator s
                            in case length splitted of
                                 2 -> Holiday (BaseTools.getDateInt . read . head $ splitted) (read $ splitted!!1)
                                 otherwise -> error $ "Invalid string representation of holiday:" ++ s


-- | Read in the holiday list from a string list. This is not only a simple map.
-- | The function has to check for overlapping holidays (which is unlikely) and
-- | throw an exception in this case.
readUsrHolidays :: [String] -- ^ List of string representations of holidays.
                -> [Holiday] -- ^ List of holidays in packaged format.
readUsrHolidays xs = foldr step [] xs
    where step x [] = [createHolidayFromString x]
          step x xs | lastOfHoliday extractedHoliday < firstOfHoliday followingHoliday = extractedHoliday:xs
                    | otherwise = error $
                                  "Overlapping holidays:" ++ (show extractedHoliday) ++ "->" ++ (show followingHoliday)
              where extractedHoliday = createHolidayFromString x
                    followingHoliday = head xs


-- | Filter a name of user configuration file names like "gast.usr", "heiko.usr", "x.conf" and
-- | return a list of users like "gast", "heiko"
usrNameListFromFileList :: [String] -- ^ List of file names to match
                        -> [String] -- ^ Returns list with user names
usrNameListFromFileList xs = foldr step [] xs
    where step x ys | drop (l x) x == usrFileSuffix = take (l x) x : ys
                    | otherwise = ys
              where  l x = length x - length usrFileSuffix 

-- | Package a single user's settings into general structure suitable for SocketServer. All values must be
-- | lists of ConfigItem. A (hierarchical) dirctionary has string keys and always lists of
-- | ConfigItem values.
packageUsr :: UsrSettings -- ^ User settings to package
           -> [BaseTools.ConfigItem] -- ^ One dictionary in this list containing all user settings.
packageUsr settings =
    [
     BaseTools.CfDict $ Map.fromList [
                   ("group", [BaseTools.CfString $ usrGroup settings]),
                   ("start", map 
                               (BaseTools.CfInt . BaseTools.fromDateInt . firstOfHoliday)
                               (holidayList settings)),
                   ("length", map
                                (BaseTools.CfInt . BaseTools.fromPositiveInt . lengthOfHoliday)
                                (holidayList settings))
                  ]
    ]

-- | Small stub to get tuple into IO monad. Used to combine it with some standard monad helpers.
extractIO :: (x, IO y) -> IO (x, y)
-- | Desugared do syntax (exercise), retrieve y from monad, pack it into tuple with x and return it to the monad.
-- | Remember for any Monad m: (>>=)  :: m a' -> (a' -> m b') -> m b'
extractIO (a, b) = let f r = return (a, r) in b >>= f

-- Network functions
--map (\x -> [x]) (Map.keys ((Map.!) state "user-config"))

-- | Return the list of all users registered in the "Holiday" system.
_getAllUsers :: BaseTools.Dictionary -- ^ Status input
             -> Bool -- ^ If access via privileged connection
             -> [String] -- ^ Parameter list input. This function has no parameters.
             -> (BaseTools.Dictionary,[[String]],[[String]]) -- ^ Unmodified state, list of users, empty list of modifications
_getAllUsers state _ _ = ( 
                          state,
                          map (\x->[x]) $
                              Map.keys (
                                        (BaseTools.get . head $ (Map.!) state "user")::BaseTools.Dictionary
                                       ), -- ghc needs a bit help here - does not derive the needed type properly
                          []
                         )


-- | Function starting holiday server, running until shutdown received via network
startHolidayServer :: Maybe String -- ^ Config file name without path, defaults to "holiday.conf"
                   -> Maybe String -- ^ Config directory path, not file name. Defaults to ".".
                   -> Maybe String -- ^ Working directory. Read users from there. Defaults to ".".
                   -> IO () -- ^ Returns nothing, only side effects
startHolidayServer cFile cDir wDir = do
  let workDir = MayBe.fromMaybe "." wDir

  -- Read in global configuration file with general settings. Will fail on I/O or parse error.
  let configFileName = MayBe.fromMaybe "." cDir </> MayBe.fromMaybe "holiday.conf" cFile
  print ("Read config file:" ++ configFileName ++ ".")
  config <- MControl.liftM (head . (\x->(Map.!) x "global")) (BaseTools.readConfigFile configFileName)
  print ("Config:" ++ (show config))
  -- Wow! BaseTools.get extracts the values from CfItem. Inheritance and type system does the rest.
  let privileged = (map BaseTools.get (MayBe.fromMaybe [] $ Map.lookup "privileged" (BaseTools.get config)))::[String]


  -- Read in all users stored by the holiday system and their settings. Will fail on I/O or parse error.
  usrNamesFromFiles <- MControl.liftM usrNameListFromFileList (SysDir.getDirectoryContents workDir)
  usrData <- MControl.liftM Map.fromList (MControl.sequence (map (\x -> extractIO (x, readUsrFile (x ++ usrFileSuffix))) usrNamesFromFiles))
  -- This is sufficient, but cannot be transferred via the socket server. Only fixed data types, no arbitrary
  -- data types.
  -- We build a BaseTools.Dictionary from that now. Dynamic would be a better idea. SocketServer has to be changed for that.
  let packageableDataDict = Map.map packageUsr usrData

  let state = Map.fromList [("workdir", [BaseTools.CfString workDir]), ("user", [BaseTools.CfDict packageableDataDict])]

  -- Setup socket connection, map network functions. This may fail as well.
  let socket = SocketServer.connectionDefault
  -- FIXME: Use foldl to add the complete list
  let funcReg = SocketServer.pushHandler Map.empty "getu" (SocketServer.SyncLess _getAllUsers)
  -- Start socket server, process requests. Errors on a single request have to be caught.
  SocketServer.serveSocketUntilShutdown funcReg state socket
  -- Garbage received over the network must not crash the server.

  return ()
