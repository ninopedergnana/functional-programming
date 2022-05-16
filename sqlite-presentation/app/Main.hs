module Main where

import Control.Monad
import qualified Data.Text as T
import Database.SQLite.Simple

-- A query for creating a table in the database. Does nothing if the table exists already.
createQuery :: Query
createQuery = Query (T.pack "CREATE TABLE IF NOT EXISTS database (firstname TEXT, lastname TEXT, weight TEXT, height TEXT, phone TEXT);")

-- A query for adding a (name, phonenumber) pair into the database.
addQuery :: Query
addQuery = Query (T.pack "INSERT INTO database (firstname, lastname, weight, height, phone) VALUES (?, ?, ?, ?, ?);")

addToDatabase :: Connection -> String -> String -> String -> String -> String -> IO ()
addToDatabase db firstname lastname weight height phone = execute db addQuery (firstname, lastname, weight, height, phone)

-- A query for getting all numbers associated with a given name from the database.
getQuery :: Query
getQuery = Query (T.pack "SELECT * FROM database WHERE lastname = ?;")

-- A query for getting all numbers associated with a given name from the database.
getBMIQuery :: Query
getBMIQuery = Query (T.pack "SELECT weight, height FROM database WHERE lastname = ?;")

getPersonData :: Connection -> String -> IO [[String]]
getPersonData db name = query db getQuery [name]

getWeightAndHeight :: Connection -> String -> IO [[String]]
getWeightAndHeight db name = query db getBMIQuery [name]

openDatabase :: IO Connection
openDatabase = do
  db <- open "database.db"
  execute_ db createQuery
  return db

addMode :: Connection -> IO ()
addMode db = do
  putStrLn "First Name?"
  firstname <- getLine
  when (not (null firstname)) $ do
    putStrLn "Last Name?"
    lastname <- getLine
    putStrLn "Weight?"
    weight <- getLine
    putStrLn "Height?"
    height <- getLine
    putStrLn "Phone?"
    phone <- getLine
    addToDatabase db firstname lastname weight height phone

queryMode :: Connection -> IO ()
queryMode db = do
  putStrLn "Name?"
  name <- getLine
  when (not (null name)) $ do
    personData <- getPersonData db name
    putStrLn (show $ "Person data:")
    mapM_ print personData


bmiMode :: Connection -> IO ()
bmiMode db = do
  putStrLn "Name?"
  name <- getLine
  when (not (null name)) $ do
    personData <- getWeightAndHeight db name
    putStrLn (show $ "Person data:")
    let firstlist = head personData
    let result = bmiCalc (head firstlist) (last firstlist)
    print result


bmiCalc :: String -> String -> Float
bmiCalc heightCm weightKg = do
    let w = read weightKg :: Float
    let h = read heightCm :: Float
    return (w/(h/100)^2)



main :: IO ()
main = do
  db <- openDatabase
  putStrLn "(a)dd or (q)uery? (b)mi"
  choice <- getLine
  case choice of "a" -> addMode db
                 "q" -> queryMode db
                 "b" -> bmiMode db
                 _ -> return ()