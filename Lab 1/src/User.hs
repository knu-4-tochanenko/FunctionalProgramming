module User where

import Common
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data User = User
  { uid :: Maybe Integer,
    name :: String,
    surname :: String,
    email :: String
  }

createUser :: IO User
createUser = do
  putStrLn "Enter type name:"
  _name <- getLine
  putStrLn "Enter type surname:"
  _surname <- getLine
  putStrLn "Enter type email:"
  _email <- getLine
  return
    User
      { uid = Nothing,
        name = _name,
        surname = _surname,
        email = _email
      }

printUser :: User -> IO ()
printUser _user = do
  putStrLn "Subscription:"
  putStr (" Id: " ++ show (uid _user))
  putStr (" Name: " ++ name _user)
  putStr (" Surname: " ++ surname _user)
  putStr (" Email: " ++ email _user)
  putStrLn ""

instance Common User where
  convert [SqlInteger _id, SqlByteString _name, SqlByteString _surname, SqlByteString _email] =
    User
      { uid = Just _id,
        name = BS.unpack _name,
        surname = BS.unpack _surname,
        email = BS.unpack _email
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from users"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from users where id = ?"

  deleteAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from users"

  deleteById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from users where id = ?"

  create conn entity = do
    _ <- run conn query [SqlString (name entity), SqlString (surname entity), SqlString (email entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into users (name, surname, email) values (?, ?, ?)"
      lastId = "select * from users order by id desc limit 1"
