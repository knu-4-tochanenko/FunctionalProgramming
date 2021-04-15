module Rule where
  
import Common
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Rule = Rule
  { uid :: Maybe Integer,
    text :: String
  }

createRule :: IO Rule
createRule = do
  putStrLn "Enter type text:"
  _text <- getLine
  return
    Rule
      { uid = Nothing,
        text = _text
      }

printRule :: Rule -> IO ()
printRule _rule = do
  putStrLn "Type:"
  putStr (" Id: " ++ show (uid _rule))
  putStr (" Text: " ++ text _rule)
  putStrLn ""

instance Common Rule where
  convert [SqlInteger _id, SqlByteString _text] =
    Rule
      { uid = Just _id,
        text = BS.unpack _text
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from rules"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from rules where id = ?"

  deleteAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from rules"

  deleteById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from rules where id = ?"

  create conn entity = do
    _ <- run conn query [SqlString (text entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into rules (text) values (?)"
      lastId = "select * from rules order by id desc limit 1"
