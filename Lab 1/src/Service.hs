module Service where

import Common
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Service = Service
  { uid :: Maybe Integer,
    name :: String,
    author_id :: Integer,
    version :: Integer,
    term_id :: Integer,
    rule_id :: Integer, 
    period_id :: Integer
  }

createService :: IO Service
createService = do
  putStrLn "Enter type name:"
  _name <- getLine
  putStrLn "Enter type author_id:"
  _author_id <- getLine
  putStrLn "Enter type version:"
  _version <- getLine
  putStrLn "Enter type term_id:"
  _term_id <- getLine
  putStrLn "Enter type rule_id:"
  _rule_id <- getLine
  putStrLn "Enter type period_id:"
  _period_id <- getLine
  return
    Service
      { uid = Nothing,
        name = _name,
        author_id = read _author_id :: Integer,
        version = read _version :: Integer,
        term_id = read _term_id :: Integer,
        rule_id = read _rule_id :: Integer,
        period_id = read _period_id :: Integer
      }

printService :: Service -> IO ()
printService _service = do
  putStrLn "Service:"
  putStr (" Id: " ++ show (uid _service))
  putStr (" Name: " ++ name _service)
  putStr (" Author Id: " ++ show (author_id _service))
  putStr (" Version: " ++ show (version _service))
  putStr (" Term Id: " ++ show (term_id _service))
  putStr (" Rule Id: " ++ show (rule_id _service))
  putStr (" Period Id: " ++ show (period_id _service))
  putStrLn ""

instance Common Service where
  convert [SqlInteger _id, SqlByteString _name, SqlInteger _author_id, SqlInteger _version, SqlInteger _term_id, SqlInteger _rule_id, SqlInteger _period_id] =
    Service
      { uid = Just _id,
        name = BS.unpack _name,
        author_id = _author_id,
        version = _version,
        term_id = _term_id,
        rule_id = _rule_id,
        period_id = _period_id
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from service"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from service where id = ?"

  deleteAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from service"

  deleteById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from service where id = ?"

  create conn entity = do
    _ <- run conn query [SqlString (name entity), SqlInteger (author_id entity), SqlInteger (version entity), SqlInteger (term_id entity), SqlInteger (rule_id entity), SqlInteger (period_id entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into service (name, author_id, version, term_id, rule_id, period_id) values (?, ?, ?, ?, ?, ?)"
      lastId = "select * from service order by id desc limit 1"
