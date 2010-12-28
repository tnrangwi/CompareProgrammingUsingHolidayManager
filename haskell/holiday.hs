#!/usr/bin/runghc
-- Author: Thorsten Rangwich
-- See file LICENSE for details on using this code.
-- This is the script starting the holiday server
import qualified HolidayServer

-- | Main function. Starts up the holiday server
main :: IO ()
-- FIXME: Parse argument list!
main = do
  HolidayServer.startHolidayServer Nothing Nothing Nothing