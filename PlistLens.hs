{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.Aeson as Aeson
import Data.Aeson((.:), Value(..))
import System.Process
import System.Environment
import System.IO
import Control.Concurrent
import Data.List
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text as Text
import Data.Text(Text)
import Web.Scotty.CRUD.JSON
import Web.Scotty.CRUD.Types hiding (getRow)
--import qualified Data.Vector as V

main :: IO ()
main = getArgs >>= main2

main2 :: [String] -> IO ()
main2 [schemaFile,"get",plistFile] = do 
  schemaText <- LBS.readFile schemaFile
  case Aeson.eitherDecode schemaText of
    Left msg -> error $"can not read schema: " ++ msg
    Right js -> main_get js plistFile
  return ()
main2 _ = putStrLn "usage: plist-lens <schema.json> [put|get] <db.plist>"

------------------------------------------------------------------------------

main_get :: [SchemaColumn] -> FilePath -> IO ()
main_get ss plistFile = do 
  buddy <- plistBuddy plistFile
  txt <- buddy "Help"
  -- now, we are going to look for the rows
  res <- getRows buddy ss 0 
  print res
  buddy "Exit"
  return ()

------------------------------------------------------------------------------

------------------------------------------------------------------------------

data TYPE = STRING | NUMBER | BOOLEAN
        deriving Show
data CONV = RO | RW | Key
        deriving Show
data SchemaColumn = SchemaColumn Text [String] TYPE CONV
        deriving Show
type Schema = [SchemaColumn]

instance Aeson.FromJSON CONV where
  parseJSON (String "key") = return Key
  parseJSON (String "ro") = return RO
  parseJSON (String "rw") = return RW
  parseJSON _ = fail "do not understand conv (expecting key, ro, rw)"

instance Aeson.FromJSON TYPE where
  parseJSON (String "string") = return STRING
  parseJSON (String "number") = return NUMBER
  parseJSON (String "boolean") = return BOOLEAN
  parseJSON _ = fail "do not understand type"

instance Aeson.FromJSON SchemaColumn where
  parseJSON (Object o) = SchemaColumn
                <$> o .: "field"
                <*> o .: "path"
                <*> o .: "type"
                <*> o .: "conv"
  parseJSON _ = fail "not object"

------------------------------------------------------------------------------




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

getRows :: (String -> IO String) -> [SchemaColumn] -> Int -> IO [Aeson.Value]
getRows buddy schema n = do
   let f "#" = show n
       f o   = o
   row <- getRow buddy [ (SchemaColumn iD (fmap f path) ty conv) | (SchemaColumn iD path ty conv) <- schema ]
--   print row
   if null row
   then return []
   else do rows <- getRows buddy schema (n+1)
           return (Aeson.object row : rows)

getRow :: (String -> IO String) -> [SchemaColumn] -> IO [(Text,Aeson.Value)]
getRow buddy schema = do
        res <- sequence [ getEntry buddy col | col <- schema ]
        return $ concat res


getEntry :: (String -> IO String) -> SchemaColumn -> IO [(Text,Aeson.Value)]
getEntry buddy (SchemaColumn nm path ty conv) = do
        res <- buddy $ "Print " ++ concat (intersperse "::" path)
        let txt = concat $ intersperse "\n" $ drop 2 $ lines $ filter (/= '\r') $ res
--        print (res,txt)
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

