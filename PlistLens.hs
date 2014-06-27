module Main where

import System.Process
import System.IO
import Control.Concurrent
import Data.List
import qualified Data.ByteString  as BS

data PlistCommand 
  = Exit
  | Save
  | Print Entry
  | Set Entry Value
  | Add Entry Type Value



data Entry = E
data Type = T
data Value = V

plistBuddy :: FilePath -> IO (String -> IO String)
plistBuddy fileName = do
    (Just hin, Just hout, _, _) <- 
	createProcess (proc "script" ["-q","/dev/null","/usr/libexec/PlistBuddy",fileName])
--	createProcess (proc "/usr/libexec/PlistBuddy" [fileName])    
--	createProcess (proc "/bin/echo" [fileName])
	    { std_in = CreatePipe 
	    , std_out = CreatePipe 
	    }

    hSetBuffering hin  NoBuffering
    hSetBinaryMode hin True
    hSetBuffering hout NoBuffering
    hSetBinaryMode hout True
  
    let prompt = "\nCommand: "

    let untilPrompt cs | reverse prompt `isPrefixOf` cs = return (reverse $ drop (length prompt) $ cs)
                       | otherwise = do
         eof <- hIsEOF hout
         if eof 
         then return (reverse cs)
         else do 
            c <- hGetChar hout
   	    untilPrompt (c : cs)

    -- Wait for first prompt 
    _ <- untilPrompt "\n"

    return $ \ input -> do
          hPutStrLn hin input  -- send command
          untilPrompt "\n"   -- wait for output

main = do
     buddy <- plistBuddy ("X.plist")
     txt <- buddy "Help"
     print txt
     txt <- buddy "Print"
     print txt
     txt <- buddy "Exit"
     print txt
     return ()
