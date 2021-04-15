module Period where
  
import Common
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Period = Period
  { uid :: Maybe Integer,
    datetime_start :: String,
    datetime_end :: String
  }

createPeriod :: IO Period
createPeriod = do
  putStrLn "Enter type datetime_start:"
  _datetime_start <- getLine
  putStrLn "Enter type datetime_end:"
  _datetime_end <- getLine
  return
    Period
      { uid = Nothing,
        datetime_start = _datetime_start,
        datetime_end = _datetime_end
      }

printPeriod :: Period -> IO ()
printPeriod _period = do
  putStrLn "Type:"
  putStr (" Id: " ++ show (uid _period))
  putStr (" Start: " ++ datetime_start _period)
  putStr (" End: " ++ datetime_end _period)
  putStrLn ""

instance Common Period where
  convert [SqlInteger _id, SqlByteString _datetime_start, SqlByteString _datetime_end] =
    Period
      { uid = Just _id,
        datetime_start = BS.unpack _datetime_start, 
        datetime_end = BS.unpack _datetime_end
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from periods"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from periods where id = ?"

  deleteAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from periods"

  deleteById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from periods where id = ?"

  create conn entity = do
    _ <- run conn query [SqlString (datetime_start entity), SqlString (datetime_end entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into periods (datetime_start, datetime_end) values (?, ?)"
      lastId = "select * from periods order by id desc limit 1"
