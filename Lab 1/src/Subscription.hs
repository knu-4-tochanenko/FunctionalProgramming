module Subscription where

import Common
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Subscription = Subscription
  { uid :: Maybe Integer,
    text :: String
  }

createSubscription :: IO Subscription
createSubscription = do
  putStrLn "Enter subscription text:"
  _text <- getLine
  return
    Subscription
      { uid = Nothing,
        text = _text
      }

printSubscription :: Subscription -> IO ()
printSubscription _subscription = do
  putStrLn "Subscription:"
  putStrLn ("\tId: " ++ show (uid _subscription))
  putStrLn ("\tText: " ++ text _subscription)
  putStrLn ""

instance Common Subscription where
  convert [SqlInteger _id, SqlByteString _text] =
    Subscription
      { uid = Just _id,
        text = BS.unpack _text
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from subscription"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from subscription where id = ?"

  deleteAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from subscription"

  deleteById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from subscription where id = ?"

  create conn entity = do
    _ <- run conn query [SqlString (text entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into subscription (text) values (?)"
      lastId = "select * from subscription order by id desc limit 1"
