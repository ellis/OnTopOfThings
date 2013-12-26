{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Map as M
--import           Database.Persist
import           Database.Esqueleto
import           Database.Persist.Sqlite (runSqlite, runMigration)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Change
  date Int
  uuid String
  property String
  action Int
  value String Maybe
  deriving Show
Item
  uuid String
  UuidKey uuid
  type String
  title String
  stage String Maybe
  deriving Show
Parent
  child String
  parent String
  deriving Show
Order
  before ItemId
  after ItemId
Flag
  uuid ItemId
  name String
  deriving Show
Property
  uuid ItemId
  name String
  value String
  deriving Show
|]

data Stuff = Stuff
  { dbItems :: M.Map String Item
  , dbParents :: M.Map String Parent
  } deriving Show

main :: IO ()
main = runSqlite "otot2.db" $ do
  runMigration migrateAll
  -- addTestRecords

  -- changes <- selectList ([] :: [Filter Change]) []
  -- liftIO $ print (changes :: [Entity Change])

  changes <- select $ from $ \change -> do return change
  liftIO $ print (changes :: [Entity Change])

  liftIO $ mapM_ (putStrLn . show . changeUuid . entityVal) changes

  let changes' = map entityVal changes
  liftIO $ putStrLn $ show changes'

--  let l1 = map (\x -> (changeUuid x, [x])) changes'
--  liftIO $ putStrLn $ show l1
--
--  let m1 = M.fromAscList l1
--  liftIO $ putStrLn $ show m1
--
  let db = Stuff M.empty M.empty
  let db1 = foldl dbUpdate db changes'
--  let db1 = dbUpdate (Change 1 "a" "type" 1 (Just "list")) db
  liftIO $ putStrLn $ show db1

--addTestRecords :: IO ()
--addTestRecords = do
--  insert $ Change 1 "a" "type" 1 (Just "list")
--  insert $ Change 1 "a" "title" 1 (Just "admin")
--  insert $ Change 2 "b" "type" 1 (Just "task")
--  insert $ Change 2 "b" "title" 1 (Just "improve myrepos scripts")
--  insert $ Change 2 "b" "stage" 1 (Just "inbox")
--  insert $ Change 2 "b" "parent" 1 (Just "a")

dbUpdate :: Stuff -> Change -> Stuff
dbUpdate db x =
  case x of
    Change _ _ _ 1 Nothing -> db -- error
    Change _ uuid "type" 1 (Just value) -> dbItemMod (\item -> item { itemType = value }) uuid
    Change _ uuid "title" 1 (Just value) -> dbItemMod (\item -> item { itemTitle = value }) uuid
    Change _ uuid "stage" 1 (Just value) -> dbItemMod (\item -> item { itemStage = Just value }) uuid
    Change _ uuid "parent" 1 (Just value) -> dbParentMod value uuid
    _ -> db -- error
  where
    dbItemMod :: (Item -> Item) -> String -> Stuff
    dbItemMod fn uuid = db' where
      items = dbItems db
      item = M.findWithDefault (Item uuid "" "" Nothing) uuid items
      item' = fn item
      items' = M.insert uuid item' items
      db' = db { dbItems = items' }
    dbParentMod :: String -> String -> Stuff
    dbParentMod value uuid = db' where
      xs = dbParents db
      x' = Parent uuid value
      xs' = M.insert uuid x' xs
      db' = db { dbParents = xs' }

--construct :: [Entity Change] -> Map String (Map String (Maybe String))
--construct l = map2 where
  --map1 = map (\x -> 
