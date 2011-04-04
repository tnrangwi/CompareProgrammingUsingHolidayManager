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

-- FIXME: We have to evaluate the structures after reading in and starting up the holiday server.

import qualified BaseTools
import qualified SocketServer
import qualified Data.Map as Map
import qualified Data.Maybe as MayBe
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.IO.Error as IOError
import qualified System.IO as FileIO
import qualified System.Directory as SysDir
import qualified Control.Monad as MControl

--import Debug.Trace (trace)

-- | File extension for files containing user holidays.
usrExtension :: String
usrExtension = ".usr"

-- | File extension for temporary files.
tmpExtension :: String
tmpExtension = ".tmp"

-- | Separator used for the configuraton files.
fieldSeparator :: Char
fieldSeparator = '|'


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
                   deriving Show


-- | Read file containing settings for one single user.
readUsrFile :: String -- ^ File name without extension
            -> IO UsrSettings -- ^ User's settings. See type definition for details.
readUsrFile = MControl.liftM (readUsrFromList . lines) . FileIO.readFile . (`FilePath.addExtension` usrExtension)


-- | Read user configuration file from a list of lines consisting of the file's contents.
readUsrFromList :: [String] -> UsrSettings
readUsrFromList (x:xs) = UsrSettings x (readUsrHolidays xs)


-- | Read in a one string containing start day and length of holiday in days, extract
-- | the corresponding holiday type-safe. Throws an exception if format does not match.
createHolidayFromString :: String -- ^ String like "20122512|12", separator is taken from SocketServer module.
                        -> Holiday -- ^ Packaged holiday.
createHolidayFromString s = let splitted =  BaseTools.splitBy fieldSeparator s
                            in case length splitted of
                                 2 -> Holiday
                                      (BaseTools.getDateIntFromDayOfYearInt . read . head $ splitted)
                                      (BaseTools.getPositiveInt . read $ (splitted!!1))
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
                                  "Overlapping holidays:" ++ show extractedHoliday ++ "->" ++ show followingHoliday
              where extractedHoliday = createHolidayFromString x
                    followingHoliday = head xs


-- | Filter a name of user configuration file names like "gast.usr", "heiko.usr", "x.conf" and
-- | return a list of users like "gast", "heiko"
usrNameListFromFileList :: [String] -- ^ List of file names to match
                        -> [String] -- ^ Returns list with user names
usrNameListFromFileList xs = foldr step [] xs
    where step x ys | FilePath.takeExtension x == usrExtension = FilePath.dropExtension x : ys
                    | otherwise = ys

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

-- | Small function converting a string into a DateInt. This throws a nicer error than the standard read would do.
-- | It both converts a DateInt compatible integer and a day of year integer.
_stringToDate :: String -> BaseTools.DateInt
_stringToDate s = case readsPrec 1 s of
                    (i, []):[] -> if i >= 10000000 then -- DateInt format has exactly 8 digits, dayofyear 5-7 digits
                                      BaseTools.getDateInt i
                                  else
                                      BaseTools.getDateIntFromDayOfYearInt i
                    otherwise -> error $ "No date int value received via network:" ++ s

-- | Small function converting a string into a PositiveInt. This throws a nicer error than the standard read would do.
_stringToPositiveInt :: String -> BaseTools.PositiveInt
_stringToPositiveInt s = case readsPrec 1 s of
                           (i, []):[] -> BaseTools.getPositiveInt i
                           otherwise -> error "No positive int value received via network"



-- Helper functions to surfe through the state dictionary

-- | Extract one of this configuration items only there once in the configuration
_extractVariable :: BaseTools.Dictionary
                 -> String
                 -> BaseTools.ConfigItem
_extractVariable state = head . (Map.!) state

-- | Retrieve dictionary containing all the users. This is mainly for further processing
_extractPackagedUsers :: BaseTools.Dictionary -- ^ The state dictionary
                      -> BaseTools.Dictionary -- ^ The dictionary with the user data
_extractPackagedUsers = BaseTools.get . (`_extractVariable` "user")

-- | Extract single user from nested dictionary into proper UsrSettings structure. Does not check contents.
_extractUsr :: BaseTools.Dictionary -- ^ The network dictionary containing all users
             -> String -- ^ The name of the user
             -> Maybe UsrSettings -- ^ The extracted user settings
_extractUsr users name =
    case Map.lookup name users of
      Just (BaseTools.CfDict userDict : []) -> Just $
                                               UsrSettings (BaseTools.get . head $ (Map.!) userDict "group")
                                                           (map extractHoliday (zip ((Map.!) userDict "start")
                                                                                ((Map.!) userDict "length")))
                                                               where extractHoliday (s, l) = Holiday (gDate s) (gLength l)
                                                                     gDate = BaseTools.getDateInt . BaseTools.get
                                                                     gLength = BaseTools.getPositiveInt . BaseTools.get
      Nothing -> Nothing
      _ -> error "Invalid user packaging (internal error)"

-- | Helper function to check two holidays for overlaps
_checkOverlap :: Holiday -- ^ Sorted: 1st holiday
              -> Holiday -- ^ Second holiday
              -> Bool -- ^ True or False
_checkOverlap (Holiday s l) (Holiday s' l') =
    BaseTools.dateAdd s (BaseTools.fromPositiveInt l) > s'

-- | Helper function to add one holiday to the list
_addHol :: [Holiday] -- ^ Input holidays definition
        -> Holiday -- ^ Holiday to insert
        -> [Holiday] -- ^ Modified holidays definition
_addHol [] h = [h]
_addHol (h'@(Holiday s' l'):h''@(Holiday s'' l''):xs) h@(Holiday s l)
    | s'' <= s =  h':_addHol (h'':xs) h
    | s' <= s && s'' >= s = if _checkOverlap h' h || _checkOverlap h h''
                            then
                                error "Overlapping holidays"
                            else
                                h':h:h'':xs
    | s < s' = if _checkOverlap h h'
               then
                   error "Overlapping holidays"
               else
                   h:h':h'':xs

_addHol (h'@(Holiday s' l'):[]) h@(Holiday s l)
    | s <= s' = if _checkOverlap h h'
                then
                    error "Overlapping holidays"
                else
                    [h, h']
    | otherwise = if _checkOverlap h' h
                  then
                      error "Overlapping holidays"
                  else
                      [h', h]

-- Helper functions having IO effects


-- | Save a user's configuration in the file system.
-- | FIXME: Does not handle IO errors. Close has to be done in any case, rename only on success.
_saveUsr :: UsrSettings -- ^ The configuration of the user
         -> String -- ^ The name of the user to save it
         -> String -- ^ The directory to store the data
         -> IO ()
_saveUsr usr name dir = do
  let tmpFile = dir </> FilePath.addExtension name tmpExtension
  fd <- FileIO.openFile tmpFile FileIO.WriteMode
  FileIO.hPutStrLn fd (usrGroup usr)
  let w h = FileIO.hPutStrLn fd $ show (firstOfHoliday h) ++ [fieldSeparator] ++ show (lengthOfHoliday h)
    in mapM w (holidayList usr)
  FileIO.hClose fd
  SysDir.renameFile tmpFile (FilePath.replaceExtension tmpFile usrExtension)


-- Network functions

-- | Dummy function, for debugging purposes. Non IO part.
_debug :: BaseTools.Dictionary -- ^ Status input
       -> Bool -- ^ connection is privileged
       -> [String] -- ^ Parameter list input
       -> (BaseTools.Dictionary, [[String]], [[String]]) -- ^ State, result, modification list
_debug state priv args = error $ (show (Map.keys state)) -- ++ (show ((Map.!) state "user"))

-- | Return the list of all users registered in the "Holiday" system.
_getAllUsers :: BaseTools.Dictionary -- ^ Status input
             -> Bool -- ^ If access via privileged connection
             -> [String] -- ^ Parameter list input. This function has no parameters.
             -> (BaseTools.Dictionary,[[String]],[[String]]) -- ^ Unmodified state, list of users, empty list of modifications
-- The colon looks a bit strange at my first sight. Every x has to be packaged in a list by map. Instead of \x->[x]
-- it is natural to use \x->x`:`[], which is (: []) applied to x. The infix operator given with a second argument
-- returns a function. The only remaining argument then is the first argument. Currying in the 1st argument.
-- Never have seen that up to now... hlint knows better. ((:) []) x gives an error and would expect ((:) x) [].
_getAllUsers state _ _ = (
                          state,
                          map (: []) $ Map.keys (_extractPackagedUsers state),
                          []
                         )

-- | Add one user to the list of users. Non IO function.

_addUser :: BaseTools.Dictionary -- ^ Status input
         -> Bool -- ^ If access via privileged connection. Does only work when not set to true.
         -> [String] -- ^ Parameter list input. The name of the single user to add. May accept more than one later.
         -> (BaseTools.Dictionary,[[String]],[[String]]) -- ^ Modified state, empty result, list of one added user.
-- The colon looks a bit strange at my first sight. Every x has to be packaged in a list by map. Instead of \x->[x]
-- it is natural to use \x->x`:`[], which is (: []) applied to x. The infix operator given with a second argument
-- returns a function. The only remaining argument then is the first argument. Currying in the 1st argument.
-- Never have seen that up to now... hlint knows better. ((:) []) x gives an error and would expect ((:) x) [].
_addUser state priv (newu:group:[]) = if priv
                                      then
                                          let users = _extractPackagedUsers state
                                          in
                                            if Map.member newu users 
                                            then
                                                error "user already exists, not added"
                                            else
                                                let updated = Map.insert newu (packageUsr $ UsrSettings group []) users
                                                in
                                                  (
                                                   Map.insert "user" [(BaseTools.CfDict updated)] state, --update state with new user dict
                                                   [], -- no result
                                                   [[newu]]) -- tell postprocessing to create the corresponding file
                                      else
                                          error "addu only works over a privileged connection"

-- | IO part of some network functions.

-- | Dummy function - IO part - for debugging purposes
_debugIo :: [[String]] -- ^ Input commands
           -> BaseTools.Dictionary -- ^ State
           -> IO () -- ^ Returns nothing, just saves to file system for synchronisation.
_debugIo  args state = error $ "Debug IO not yet implemented" ++ (head (head args))


-- | Saves a particular user.
_saveUserIo :: [[String]] -- ^ List of one user added or modified in the list of users
           -> BaseTools.Dictionary -- ^ State dictionary, there we will find the user.
           -> IO () -- ^ Returns nothing, just saves to file system for synchronisation.
_saveUserIo ((usr:[]):[]) state = let uSetgs = (_extractUsr (_extractPackagedUsers state) usr)
                                  in case uSetgs of
                                       Just settings -> _saveUsr 
                                                        settings 
                                                        usr
                                                        (BaseTools.get (_extractVariable state "workdir"))
                                       otherwise -> error "Internal error: User not in state dictionary"
                                 
_saveUserIo _ _ = error "Internal error, no user or more than one users to save"


-- | Add holiday for user - non IO part
_addHoliday :: BaseTools.Dictionary -- ^ Status input
            -> Bool -- ^ If access via privileged connection. Not queried.
            -> [String] -- ^ Parameter list input. The name, start date and duration.
            -> (BaseTools.Dictionary,[[String]],[[String]]) -- ^ Modified state, empty result, list of one added user.
_addHoliday state _ (name:starth:lengthh:[]) =
    let users = _extractPackagedUsers state
        uSetgs = _extractUsr users name
        start = _stringToDate starth
        length = _stringToPositiveInt lengthh
        hol = Holiday start length
    in case uSetgs of
         Just settings -> let changedSettings = settings { holidayList = _addHol (holidayList settings) hol }
                              updatedUsers = Map.insert name (packageUsr changedSettings) users
                          in 
                            (
                             Map.insert "user" [(BaseTools.CfDict updatedUsers)] state,
                             [],
                             [[name]])
         otherwise -> error $ "User " ++ name ++ " does not exist - add holiday failed"


_addHoliday _ _ _ = error "Invalid parameters for add holiday"



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
  print ("Config:" ++ show config)

  let privileged = map BaseTools.get (MayBe.fromMaybe [] $ Map.lookup "privileged" (BaseTools.get config)) :: [String]
  let portName = show (BaseTools.get . head $ (Map.!) (BaseTools.get config) "port" :: Int)

  -- Read in all users stored by the holiday system and their settings. Will fail on I/O or parse error.
  usrNames <- MControl.liftM usrNameListFromFileList (SysDir.getDirectoryContents workDir)
  usrData <- MControl.liftM
             Map.fromList $
                MControl.sequence (map (\x -> extractIO (x, readUsrFile x)) usrNames)
  -- This is sufficient, but cannot be transferred via the socket server. Only fixed data types, no arbitrary
  -- data types. Dynamic should be used in the socket server instead.
  -- We build a BaseTools.Dictionary from that now. Dynamic would be a better idea. SocketServer has to be changed for that.
  let packageableDataDict = Map.map packageUsr usrData

  let state = Map.fromList [("workdir", [BaseTools.CfString workDir]), ("user", [BaseTools.CfDict packageableDataDict])]

  -- Setup socket connection, map network functions. This may fail as well.
  let socket = SocketServer.connectionDefault  { SocketServer.privileged = privileged, SocketServer.portName = portName }
                                     --Missing: getu (get user holiday), delh, geta, delu, sync (?)
  let funcReg = foldl step Map.empty [("getl", (SocketServer.SyncLess _getAllUsers)),
                                      ("addu", (SocketServer.Syncing _addUser _saveUserIo)),
                                      ("addh", (SocketServer.Syncing _addHoliday _saveUserIo)),
                                      (".dbg", (SocketServer.Syncing _debug _debugIo))]
          where step m (n, f) = SocketServer.pushHandler m n f
  -- Start socket server, process requests. Errors on a single request have to be caught.
  SocketServer.serveSocketUntilShutdown funcReg state socket
  -- FIXME: Garbage received over the network must not crash the server.

  return ()
