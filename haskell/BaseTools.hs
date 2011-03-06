{-# LANGUAGE FlexibleInstances #-} -- This is to make GetCfg's getItem work with String result as well.
{-# LANGUAGE TypeSynonymInstances #-} -- Same topic. Both seem to be widely used.
-- | Author: Thorsten Rangwich
-- | See file LICENSE for details on using this code.
-- | This module contains some base tools like reading a configuration file.
module BaseTools
    (
     readConfigFile,
     readConfigList,
     emptyConfig,
     ConfigItem(..),
     Dictionary,
     UnpackCfg(..),
     DateInt,
     getDateInt,
     fromDateInt,
     dateFromDateInt,
     getDateIntFromDayOfYearInt,
     dateAdd,
     PositiveInt,
     fromPositiveInt,
     getPositiveInt,
     getItems,
     splitBy,
     splitBy',
     mergeWith
    )
where

import qualified Data.Map as Map
import qualified System.IO as FileIO
import qualified Data.String.Utils as StringUtils
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Control.Monad as MControl
import qualified Data.Time.Calendar as Date

-- | All items currently supported in config file: None, Int, Float, String and Dictionary String to ConfigItem list.
data ConfigItem = CfNone -- ^ Empty item.
                | CfInt Int -- ^ Int value.
                | CfFloat Float -- ^ Float value.
                | CfString String -- ^ String value.
                | CfDict (Map.Map String [ConfigItem]) -- ^ Dictionary, only ConfigItem lists as values, no single values.
                  deriving Show

type Dictionary = Map.Map String [ConfigItem]

-- | Type class to retrieve the various values in native format
class UnpackCfg a where
    get :: ConfigItem -> a
    get _ = error "Unexpected type for config item"
    getIndex :: Int -> [ConfigItem] -> a
    getIndex i l = get (l!!i)
    getList :: [ConfigItem] -> [a]
    getList = map get 

instance UnpackCfg Int where
    get (CfInt v) = v
    get _ = error "No integer value in config item"

instance UnpackCfg Float where
    get (CfFloat v) = v

instance UnpackCfg String where
    get (CfString v) = v
    get _ = error "There is no string value in config here"

instance UnpackCfg Dictionary where
    get (CfDict v) = v
    get _ = error "No section / dictionary here"

-- | Get CfgItem from String (as supposed to be in cfg file). FIXME: Error handling for invalid float or integer strings.
_getCfgFromString :: String -> ConfigItem
_getCfgFromString "" = CfNone
_getCfgFromString ('"':[]) = error "Parse error, single quote value"
_getCfgFromString ('"':xs) = if last xs == '"' then CfString (take (length xs - 1) xs)
                             else error "Parse error, string not terminated by quote"
_getCfgFromString a = if '.' `elem` a then CfFloat (read a)
                      else CfInt (read a)


-- | Extract the name of a section. First [ is omitted, last is checked and left out. This is a help stub.
_extractSectionName :: String -> String
_extractSectionName [] = error "Invalid section line, almost empty"
_extractSectionName s = if last s == ']' then take (length s - 1) s
                        else error "Invalid, unterminated section line (missing ])."

-- | Return empty list or current list of values for a given key in a section or top level dictionary.
-- | You can use it to retrieve a section list as well.
getItems :: String -> Dictionary -> [ConfigItem]
getItems key = Maybe.fromMaybe [] . Map.lookup key

-- | Add a config file line to the top level or to a section dictionary.
_addConfigValue :: String -- ^ Current line to parse (already stripped from leading and trailing whitespaces).
                -> Dictionary -- ^ Current section or top level dictionary.
                -> Dictionary -- ^ Returns updated section or top level dictionary.
_addConfigValue "" dic = dic
_addConfigValue ('#':_) dic = dic
_addConfigValue line dic = let (key, val) = case List.elemIndex '=' line of
                                              Just i -> (take i line, drop (i + 1) line)
                                              Nothing -> error "Non comment, non section line missing ="
                           in Map.insert key (getItems key dic ++ [_getCfgFromString val]) dic

-- | Read a section, return rest of list to parse and updated dictionary.
_readSection :: [String] -- ^ Rest of the list to be parsed.
             -> Dictionary -- ^ Current section dictionary.
             -> ([String], Dictionary) -- ^ Returns rest of lines to parse, section dictionary.
_readSection [] dic = ([], dic)
_readSection (x:xs) dic = let strippedLine = StringUtils.strip x
                          in case strippedLine of
                             "" -> _readSection xs dic
                             '#':rs -> _readSection xs dic
                             '[':rs -> (x:xs, dic) --new section, end parsing of this section.
                             _ -> _readSection xs (_addConfigValue strippedLine dic)

-- | Parse part of a config list. Receives the rest of a config list and an unfinished dictionary and completes it.
-- | Calling itself it adds top level keys or calls _readConfigSection to parse a whole section.
_readConfig :: [String] -- ^ Rest of lines still to parse 
            -> Dictionary -- ^ Current config dictionary
            -> Dictionary -- ^ Returns updated config dictionary
_readConfig [] dic = dic
_readConfig (x:xs) dic =
    let strippedLine = StringUtils.strip x
    in case strippedLine of
         "" -> _readConfig xs dic
         '#':_ -> _readConfig xs dic
         ('[':section) -> let (rs, sdic) = _readSection xs Map.empty
                              sectionName = _extractSectionName section
                          in _readConfig rs (Map.insert sectionName (getItems sectionName dic ++ [CfDict sdic]) dic)
         _ -> _readConfig xs (_addConfigValue strippedLine dic)

-- | Take a list of config file lines and parse them into cfg dict.
readConfigList :: [String] -> Dictionary
readConfigList a = _readConfig a Map.empty

-- | Read the whole config file. Splits file into line list and calls readConfigList.
readConfigFile :: String -> IO Dictionary
readConfigFile = MControl.liftM (readConfigList . lines) . FileIO.readFile
-- The above is a bit hard to understand. An equivalent definition is:
--
--   readConfigFile fileName = FileIO.readFile fileName >>= return . readConfigList . lines
--
-- And all this was derived by desugaring this do syntax:
--
--   readConfigFile fileName = do
--       theFile <- File.readFile fileName
--       let theLines = lines theFile
--       return (readConfigList theLines)

emptyConfig :: Dictionary
emptyConfig = Map.empty

-- | Split string by character. Do not merge mutliple separators in a row but instead
-- | generate empty strings.
splitBy :: Char -- ^ Character to split at
        -> String -- ^ String to split at character
        -> [String] -- ^ Result list of substrings.
splitBy _ [] = []
splitBy c s = w : splitBy c (drop 1 s')
              where (w, s') = break (== c) s


-- | Split string by character. Merges multiple separators. This was derived 1:1 from the
-- | ghc implementation of words.
splitBy' :: Char -- ^ Character to split at
         -> String -- ^ String to split
         -> [String] -- ^ Result list of substrings
splitBy' c s = case dropWhile (== c) s of
                                "" -> []
                                s' -> w : splitBy' c s''
                                      where (w, s'') =
                                             break (== c) s'

-- | Merge a list of strings to a single string, seperated by the given separator.
mergeWith :: Char -- ^ Character to insert between
          -> [String] -- ^ List of strings to merge
          -> String -- ^ Merged string
mergeWith _ [] = []
mergeWith _ (x:[]) = x
mergeWith s (x:xs) = x ++ s : mergeWith s xs

-- Stuff for date support. This is a very cheap implementation. Date is stored in Int as e.g. 20081231. So
-- interitance from Num can be used for Eq and Ord comparison. Compared to Day it throws an exception
-- when the date is constructed from an invalid date and is not adjusted to the next proper date.

-- | Data type to receive a valid date coded in an integer in human readable form, like 19751025 for 25.10.1975.
-- | Constructor is not published, only unsafe function to create it.
data DateInt = CreateDateInt Int -- ^ Create type from unchecked integer.
               deriving (Eq, Ord)
instance Show DateInt
    where show = show . fromDateInt

-- | There should be a function in Haskell already. Returns True, if the year is a leap year.
isLeapYear :: Int -- ^ Year to check
           -> Bool -- ^ True if year is leap year, otherwise False.
isLeapYear y = and [(y `mod` 4) == 0, or [(y `mod` 100) > 0, (y `mod` 400) == 0]]

-- | Static table containing the number of days within a month.
unadjustedMaxInMonth :: [Int]
unadjustedMaxInMonth = [31,28,31,30,31,30,31,31,30,31,30,31]

-- | Unsecure function to return the number of days of a month in a particular year.
maxInMonth :: Int -- ^ Month to check, MUST be between 1 and 12 - otherwise exception raised.
           -> Int -- ^ Year to check the month for.
           -> Int -- ^ Number of days in this month. Between 1 and 31.
maxInMonth m y = let unadjusted = unadjustedMaxInMonth !! (m - 1)
                 in if isLeapYear y then unadjusted + 1
                    else unadjusted

-- | Small helper stub to extract the year, month, day components
-- | from an integer used as a date. Internally used, not exported.
dateComponentsFromInt :: Int -- ^ Integer to extract components
                      -> (Int, Int, Int) -- ^ Year, month, day of month
dateComponentsFromInt di = let d = di `mod` 100
                               ym = di `div` 100
                           in let y = ym `div` 100
                                  m = ym `mod` 100
                              in (y, m, d)

-- | Function to initialise a date from an integer. This function raises an exception if the date is invalid.
-- | This function is exported.
getDateInt :: Int -- ^ Integer input
           -> DateInt -- ^ packaged DateInt
getDateInt di = let (y, m, d) = dateComponentsFromInt di
                in if and [(di >= 19000101), (di <= 20991231), (m >= 1), (m <= 12), (d >= 1), (d <= maxInMonth m y)]
                   then CreateDateInt di
                   else error ("Invalid integer to create date:" ++ show di)

-- | Unusual format: 12005 for 01.01.2005, 322005 for 01.02.2005. HolidayServer needs that.
getDateIntFromDayOfYearInt :: Int
                           -> DateInt
getDateIntFromDayOfYearInt d = let year = fromIntegral $ d `mod` 10000
                                   dInYear = fromIntegral $ d `div` 10000
                               in let (_, month, day) = Date.toGregorian . Date.addDays (dInYear-1) $
                                                        Date.fromGregorian year 1 1
                                  in getDateInt (fromIntegral year * 10000 + month * 100 + day)

-- | Create unusual format: 12005 from 01.01.2005
getDateOfYearIntFromDateInt :: DateInt
                            -> Int
getDateOfYearIntFromDateInt di =
    let
        d = dateFromDateInt di
    in
      let (j,m,t) = Date.toGregorian d
      in (fromIntegral (Date.diffDays d (Date.fromGregorian j 1 1))) * 10000 + (fromIntegral j)

-- | Function to extract Int in which the date is stored from a DateInt. This
-- | is needed only for internal usage. No need to export this.
fromDateInt :: DateInt -- ^ DateInt input
            -> Int -- ^ The constructed integer carrying the date information.
fromDateInt (CreateDateInt di) = di

-- | Function to create a real date from an integer packed date.
dateFromDateInt :: DateInt -- ^ DateInt input
                -> Date.Day -- ^ Haskell standard Day equivalent value.
dateFromDateInt di = let (y, m, d) = dateComponentsFromInt (fromDateInt di)
                     in Date.fromGregorian (fromIntegral y) m d

-- | Shift a date by a certain positive or negative amount of days. This method fails if the shift
-- | is large enough to get out of the allowed range.
dateAdd :: DateInt -- ^ DateInt to shift
        -> Int -- ^ Number of days to shift
        -> DateInt -- ^ The new, shifted DateInt. 
dateAdd date offset = let shifted = Date.addDays (fromIntegral offset) (dateFromDateInt date)
                      in let (year, month, day) = Date.toGregorian shifted
                         in getDateInt (day + month * 100 + fromIntegral year * 10000)

-- Some number support.

-- | Positive integer, constructor is not exported.
data PositiveInt = CreatePositiveInt Int
                   deriving (Eq, Ord)
instance Show PositiveInt
    where show = show . fromPositiveInt

--getPositiveInt . (read :: Int)

-- | Create a positive integer. Unsecure function, raises error if input is not strictly positive.
getPositiveInt :: Int -- ^ Input number, nust be a positive integer
               -> PositiveInt -- ^ Return value packaged in positive integer data type.
getPositiveInt i | i > 0 = CreatePositiveInt i
                 | otherwise = error ("Not a positive Integer:" ++ show i)

-- | Convert back into an integer
fromPositiveInt :: PositiveInt -- ^ PositiveInt input
                -> Int -- ^ Int output
fromPositiveInt (CreatePositiveInt i) = i

{- Test code for ghci
  let a = readConfigFile "test.cfg"
  let b = liftM (getItems "Instrument") a --retrieve sections instrument
  let c = (liftM (getIndex 0) b)::IO Dictionary -- retrieve 1st index of dictionaries as dictionary
  let d = liftM (getItems "test") c --retrieve value list for "test" key
  (liftM getList d)::IO Int -- Print out list of integers

 --short:
  (liftM (getList . (getItems "test") . (getIndex 0) . (getItems "Instrument")) a)::IO [Int]

-}  
