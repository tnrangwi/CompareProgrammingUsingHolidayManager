#!/usr/bin/runghc
-- Author: Thorsten Rangwich
-- See file LICENSE for details on using this code.
-- This is the script starting the holiday server
import qualified HolidayServer
import qualified System
import qualified System.Console.GetOpt as GetOpt -- This is the most complex way of getopts I have ever seen. Brrr!

-- | Function converting name and value of an option into a tuple to be used in an association list.
optionAssociation :: String -- ^ option name
                  -> String -- ^ option value
                  -> (String, String) -- ^ option name and value in association tuple to be used in lookup list
optionAssociation name value = (name, value)

-- | Valid command line options
options :: [GetOpt.OptDescr (String, String)]
options =
    [
     GetOpt.Option ['f'] ["config-file-name"]  (GetOpt.ReqArg (optionAssociation "f") "cfgfile")  "Config file name",
     GetOpt.Option ['c'] ["config-file-dir"]   (GetOpt.ReqArg (optionAssociation "c") "configdir") "Config file directory",
     GetOpt.Option ['w'] ["working-directory"] (GetOpt.ReqArg (optionAssociation "w") "workdir")   "Directory with usr files"
    ]

-- | Parse array of command line options. Return association list.
parseOptions :: [String] -- ^ List of command line parameters 
             -> IO ([(String, String)], [String]) -- ^ Association list with options, list of non option parameters
parseOptions args =
    case GetOpt.getOpt GetOpt.Permute options args of
      (o,n,[])   -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ GetOpt.usageInfo header options))
    where header = "Usage: holiday.hs [Option...]" -- FIXME: Use the real name of the program!

-- | Main function. Starts up the holiday server.
-- | Parse ./holiday.hs [-f <config_file>] [-w <workdir>] [-c <config_dir].
-- | See documentation of the called startHolidayServer for details.
main :: IO ()

main = do
  argv <- System.getArgs
  opts <- parseOptions argv
  print opts
  -- HolidayServer.startHolidayServer (lookup "f" options) (lookup "c" options) (lookup "w" options)