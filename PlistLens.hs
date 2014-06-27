{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson as Aeson
import System.Process
import System.IO
import Control.Concurrent
import Data.List
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text as Text
import Data.Text(Text)

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
{-
     txt <- buddy "Print"
     print txt
 -}
     rows <- getRows buddy schema 0
     rows' <- sequence [ LBS.putStrLn $ Aeson.encode row
                       | row <- rows
                       ]

     txt <- buddy "Exit"

     return ()

data TYPE = STRING | NUMBER | BOOLEAN
        deriving Show
-- type 
data CONV = Mandatory | Optional | Key
        deriving Show

type SchemaColumn = (Text,[String],TYPE,CONV)
type Schema = [SchemaColumn]

schema :: Schema
schema = [("id",["Test","Arr","#","id"],STRING,Key)
         ,("val",["Test","Arr","#","numberize"],NUMBER,Mandatory)
         ,("boo",["Test","Arr","#","boo"],BOOLEAN,Mandatory)
         ]

getRows :: (String -> IO String) -> [SchemaColumn] -> Int -> IO [Aeson.Value]
getRows buddy schema n = do
   let f "#" = show n
       f o   = o
   row <- getRow buddy [ (iD,fmap f path,ty,conv) | (iD,path,ty,conv) <- schema ]
   print row
   if null row
   then return []
   else do rows <- getRows buddy schema (n+1)
           return (Aeson.object row : rows)

getRow :: (String -> IO String) -> [SchemaColumn] -> IO [(Text,Aeson.Value)]
getRow buddy schema = do
        res <- sequence [ getEntry buddy col | col <- schema ]
        return $ concat res

getEntry :: (String -> IO String) -> SchemaColumn -> IO [(Text,Aeson.Value)]
getEntry buddy (nm,path,ty,conv) = do
        res <- buddy $ "Print " ++ concat (intersperse "::" path)
        let txt = concat $ intersperse "\n" $ drop 2 $ lines $ filter (/= '\r') $ res
        print res
        print txt
        if "Print: Entry," `isPrefixOf` txt && "Does Not Exist" `isSuffixOf` txt
        then return []
        else return [(nm, case ty of
                        STRING -> Aeson.String (Text.pack txt)
                        NUMBER -> Aeson.Number (read txt)
                        BOOLEAN | "true" `isInfixOf` txt -> Aeson.Bool True
                        BOOLEAN | "false" `isInfixOf` txt -> Aeson.Bool False
                        BOOLEAN -> error $ "bad BOOLEAN: " ++ show txt
                     )]

--match STRING 

