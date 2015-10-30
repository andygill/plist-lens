{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative
import qualified Data.Aeson as Aeson
import Data.Aeson((.:), (.:?), Value(..))
import System.Process
import System.Environment
import System.IO
import Control.Concurrent
import Data.List
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text as Text
import Data.Text(Text)
import Data.Char
import Web.Scotty.CRUD.JSON
import Web.Scotty.CRUD.Types hiding (getRow)
import Data.Monoid
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
--import qualified Data.Vector as V

main :: IO ()
main = getArgs >>= main2

main2 :: [String] -> IO ()
main2 [schemaFile,cmd,plistFile] = do 
  schemaText <- LBS.readFile schemaFile
  case Aeson.eitherDecode schemaText of
    Left msg -> error $"can not read schema: " ++ msg
    Right js -> case cmd of
                  "put" -> main_put js plistFile
                  "get" -> main_get js plistFile
                  _     -> main2 []
  return ()
main2 _ = putStrLn "usage: plist-lens <schema.json> [put|get] <db.plist>"

------------------------------------------------------------------------------

main_get :: Schema -> FilePath -> IO ()
main_get s@(Schema schema prefix) plistFile = do 
--  print s
  buddy <- plistBuddy plistFile
  -- now, we are going to look for the rows
  let loop n = do
        row <- getRow buddy [ col { path = fullPath prefix n $ path col } | col <- schema ]         
        if null row
        then return ()
        else do LBS.hPut stdout (Aeson.encode (Aeson.object row) <> "\n") 
                loop (n+1)
  loop 0          
  buddy "Exit"
  return ()

main_put :: Schema -> FilePath -> IO ()
main_put s@(Schema cols prefix) plistFile = do 
  buddy <- plistBuddy plistFile
  -- now, we are going to look for the row mapping
  let loop n = do
        print ("loop",n)
        opt_val <- getEntry buddy (head [ fullPath prefix n $ path col | col <- cols, conv col == Key ]) STRING
        case opt_val of
          Just (String v) -> do
                xs <- loop (n+1)
                return $ (v,n) : xs
          _ -> return []
  xs <- loop 0          
  let db = HashMap.fromList xs
  print db
  tab :: Table Row <- readTable stdin
  print tab
  foldM_ (\ db (row_id,row) -> do
  	     case HashMap.lookup row_id db of
               Just row_num -> do insertRow buddy s row_num row
	       	    	          return db
               Nothing      -> do let row_num = HashMap.size db
	       		       	  let s' = s { schema = [ if name c == "id"
				      	       	      	  then c { conv = RW }
							  else c
							| c <- schema s ] }
	       		          putStrLn $ "## new id : " ++ show (row_id, row_num)
	       		       	  insertRow buddy s' row_num (HashMap.insert "id" (Aeson.String row_id) row :: HashMap.HashMap Text Value)
	       		          return $ HashMap.insert row_id row_num db
         ) db (HashMap.toList tab)
  buddy "Save"
  buddy "Exit"
  return ()

------------------------------------------------------------------------------

fullPath :: [String] -> Int -> [String] -> [String]
fullPath prefix n rest = fmap (\ xs -> "'" ++ xs ++ "'") $ prefix ++ [show n] ++ rest 

------------------------------------------------------------------------------

data TYPE = STRING | INTEGER | BOOLEAN
        deriving (Show,Eq,Ord)

showT STRING  = "string"
showT INTEGER = "integer"
showT BOOLEAN = "bool"

data CONV = RO | RW | Key
        deriving (Show,Eq,Ord)
data SchemaColumn = SchemaColumn { name :: Text, path :: [String], ty :: TYPE, conv :: CONV, put :: Maybe String }
        deriving Show

data Schema = Schema { schema :: [SchemaColumn], prefix :: [String] }
        deriving Show
        
instance Aeson.FromJSON Schema where
  parseJSON (Object o) = Schema
                <$> o .: "schema"
                <*> o .: "prefix"
  parseJSON _ = fail "not object"

instance Aeson.FromJSON CONV where
  parseJSON (String "key") = return Key
  parseJSON (String "ro") = return RO
  parseJSON (String "rw") = return RW
  parseJSON _ = fail "do not understand conv (expecting key, ro, rw)"

instance Aeson.FromJSON TYPE where
  parseJSON (String "string") = return STRING
  parseJSON (String "integer") = return INTEGER
  parseJSON (String "bool") = return BOOLEAN
  parseJSON _ = fail "do not understand type"

instance Aeson.FromJSON SchemaColumn where
  parseJSON (Object o) = SchemaColumn
                <$> o .: "field"
                <*> o .: "path"
                <*> o .: "type"
                <*> o .: "conv"
                <*> o .:? "put"
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

-------


insertRow :: (String -> IO String) -> Schema -> Int -> Row -> IO ()
insertRow buddy s n row = do
        print (s,n,row)
        -- Now, go over each of the 
        sequence [ case HashMap.lookup (name col) row of
                     Nothing -> return () -- no update included
                     Just val -> do
                             b <- assignEntry buddy (fullPath (prefix s) n (path col)) val (ty col) (put col)
                             if b 
                             then return ()
                             else print $ "failed to write " ++ show (row,val)
                 | col <- schema s
                 , conv col == RW
                 ]
        return ()

--

getRow :: (String -> IO String) -> [SchemaColumn] -> IO [(Text,Aeson.Value)]
getRow buddy schema = do
        res <- sequence [ do opt_v <- getEntry buddy (path col) (ty col)
                             case opt_v of
                                    Nothing -> return []
                                    Just v -> return [(name col,v)]
                        | col <- schema ]
        return $ concat res

getEntry :: (String -> IO String) -> [String] -> TYPE -> IO (Maybe Aeson.Value)
getEntry buddy ps ty = do
        res <- buddy $ "Print " ++ concat (intersperse "::" ps)
        let txt = concat $ intersperse "\n" $ drop 2 $ lines $ filter (/= '\r') $ res
--        print (res,txt)
        if "Print: Entry," `isPrefixOf` txt && "Does Not Exist" `isSuffixOf` txt
        then return Nothing
        else return  $ Just $ case ty of
                        STRING -> Aeson.String (Text.pack txt)
                        INTEGER -> Aeson.Number (read txt)
                        BOOLEAN | "true" `isInfixOf` txt -> Aeson.Bool True
                        BOOLEAN | "false" `isInfixOf` txt -> Aeson.Bool False
                        BOOLEAN -> error $ "bad BOOLEAN: " ++ show txt

-- Returns True on success, False on failure
setEntry :: (String -> IO String) -> [String] -> Aeson.Value -> TYPE -> IO Bool
setEntry buddy ps val ty = do
        res <- buddy $ "Set " ++ concat (intersperse "::" ps) ++ " " ++ showV val
        let txt = concat $ intersperse "\n" $ drop 2 $ lines $ filter (/= '\r') $ res
        print (res,txt)
        return $ null txt 

-- Returns True on success, False on failure
addEntry :: (String -> IO String) -> [String] -> Aeson.Value -> TYPE -> IO Bool
addEntry buddy ps val ty = do
        print ps
        res <- buddy $ "Add " ++ concat (intersperse "::" ps) ++ " " ++ showT ty ++ " " ++ showV val
        let txt = concat $ intersperse "\n" $ drop 2 $ lines $ filter (/= '\r') $ res
        print (res,txt)
        return $ null txt 

-- Returns True on success, False on failure. Tries both set and add. Should work.
-- The final argument is a special flag for special updates.
assignEntry :: (String -> IO String) -> [String] -> Aeson.Value -> TYPE -> Maybe String -> IO Bool
assignEntry buddy ps val ty Nothing = do
        ok <- setEntry buddy ps val ty
        if ok 
        then return ok 
        else addEntry buddy ps val ty
assignEntry buddy ps val ty (Just "Tag") = do
        ok <- setEntry buddy ps val ty
        if ok   -- In this case, the dictionary was already there
        then return ok 
        else do -- The dict was not there, so makes sure we also have the tagName
                addEntry buddy ps val ty
                let tagName = let (c1:c2:cs) = last (init ps) in c1 : toUpper c2 : cs
                print ("##",tagName)
                addEntry buddy (init ps ++ ["Tag"]) (Aeson.String $ Text.pack tagName) STRING

showV :: Aeson.Value -> String
showV (Aeson.String txt) = show txt
showV (Aeson.Number x)   = show x
showV (Aeson.Bool True)  = "true"
showV (Aeson.Bool False) = "false"
