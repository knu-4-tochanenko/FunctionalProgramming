module Term where

import Common
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Term = Term
  { uid :: Maybe Integer,
    text :: String
  }

createTerm :: IO Term
createTerm = do
  putStrLn "Enter term text:"
  _text <- getLine
  return
    Term
      { uid = Nothing,
        text = _text
      }

printTerm :: Term -> IO ()
printTerm _term = do
  putStrLn "Term:"
  putStrLn ("\tId: " ++ show (uid _term))
  putStrLn ("\tText: " ++ text _term)
  putStrLn ""

instance Common Term where
  convert [SqlInteger _id, SqlByteString _text] =
    Term
      { uid = Just _id,
        text = BS.unpack _text
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from terms"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from terms where id = ?"

  deleteAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from terms"

  deleteById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from terms where id = ?"

  create conn entity = do
    _ <- run conn query [SqlString (text entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into terms (text) values (?)"
      lastId = "select * from terms order by id desc limit 1"
