module Utils where
  
import Data.List
import Database.HDBC
import Database.HDBC.PostgreSQL  
  
import Common
import Period
import Rule
import Service
import Subscription
import Term
import User

--TODO: move to utils
tables :: [[Char]]
tables = ["periods", "rules", "service", "subscription", "terms", "users"]

runDB :: IO ()
runDB = do
  c <- connectPostgreSQL "host=localhost dbname=tochanenko_fp user=postgres password=root"
  putStrLn "\nChoose entity:"
  putStrLn (intercalate "\n" tables)
  putStrLn "else exit\n"
  _name <- getLine
  putStrLn ("you said: " ++ _name)
  if _name `elem` tables
    then do
      putStrLn "\nChoose action:\n1 - New\n2 - Delete\n3 - Delete by id\n4 - Get all\n5 - Get all by id\nexit"
      action <- getLine
      putStrLn ("you said: " ++ action)
      if action == "exit"
        then do
          putStrLn "Unknown command"
          disconnect c
          runDB
        else do
          case action of
            "1" -> do
              putStrLn ("New " ++ _name)
              case _name of
                "periods" -> do
                  period <- createPeriod
                  result <- create c period :: IO Period
                  printPeriod result
                "rules" -> do
                  author <- createRule
                  result <- create c author :: IO Rule
                  printRule result
                "service" -> do
                  service <- createService
                  result <- create c service :: IO Service
                  printService result
                "subscription" -> do
                  subscription <- createSubscription
                  result <- create c subscription :: IO Subscription
                  printSubscription result
                "terms" -> do
                  term <- createTerm
                  result <- create c term :: IO Term
                  printTerm result
                "users" -> do
                  user <- createUser
                  result <- create c user :: IO User
                  printUser result
              commit c
--            "d" -> do
--              putStrLn ("Delete " ++ _name)
--              putStrLn ("Get by id" ++ _name)
--              putStrLn "Enter id:"
--              _uid <- getLine
--              let _id = (read _uid :: Integer)
--              result <- removeAll c :: IO Bool
--              putStrLn "Deleted"
--            "di" -> do
--              putStrLn ("Delete by id " ++ _name)
            "4" -> do
              putStrLn ("Get all " ++ _name)
              case _name of
                "periods" -> do
                  result <- getAll c :: IO [Period]
                  mapM_ printPeriod result
                "rules" -> do
                  result <- getAll c :: IO [Rule]
                  mapM_ printRule result
                "service" -> do
                  result <- getAll c :: IO [Service]
                  mapM_ printService result
                "subscription" -> do
                  result <- getAll c :: IO [Subscription]
                  mapM_ printSubscription result
                "terms" -> do
                  result <- getAll c :: IO [Term]
                  mapM_ printTerm result
                "users" -> do
                  result <- getAll c :: IO [User]
                  mapM_ printUser result
            "5" -> do
              putStrLn ("Get by id" ++ _name)
              putStrLn "Enter id:"
              _uid <- getLine
              let _id = (read _uid :: Integer)
              case _name of
                "periods" -> do
                  result <- getById c _id :: IO (Maybe Period)
                  mapM_ printPeriod result
                "rules" -> do
                  result <- getById c _id :: IO (Maybe Rule)
                  mapM_ printRule result
                "service" -> do
                  result <- getById c _id :: IO (Maybe Service)
                  mapM_ printService result
                "subscription" -> do
                  result <- getById c _id :: IO (Maybe Subscription)
                  mapM_ printSubscription result
                "terms" -> do
                  result <- getById c _id :: IO (Maybe Term)
                  mapM_ printTerm result
                "users" -> do
                  result <- getById c _id :: IO (Maybe User)
                  mapM_ printUser result
            _ -> do
              putStrLn "Unknown command"
              disconnect c
              runDB
          disconnect c
          runDB
  else if _name /= "exit"
    then putStrLn "Unknown command"
    else putStrLn "Exit"
