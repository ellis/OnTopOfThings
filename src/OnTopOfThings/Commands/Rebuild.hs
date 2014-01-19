{-
Copyright (C) 2013,2014  Ellis Whitehead

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>
-}

{-# LANGUAGE OverloadedStrings #-}

module OnTopOfThings.Commands.Rebuild
( modeInfo_rebuild
, mode_rebuild
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Debug.Trace
import Database.Persist.Sqlite
import System.Console.CmdArgs.Explicit
import System.IO
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Args
import Command
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Json
import OnTopOfThings.Data.PatchDatabase
import OnTopOfThings.Data.Types
import qualified Database as DB

modeInfo_rebuild :: ModeInfo
modeInfo_rebuild = (mode_rebuild, ModeRunIO optsRun_rebuild)

mode_rebuild = Mode
  { modeGroupModes = mempty
  , modeNames = ["rebuild"]
  , modeValue = options_empty "rebuild"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Rebuild the Sqlite database."
  , modeHelpSuffix = []
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup
    [ flagHelpSimple updHelp
    ]
  }

--modeInfo_l :: [ModeInfo]
--modeInfo_l =
--  [ modeInfo_add
--  , modeInfo_close
--  , modeInfo_delete
--  , modeInfo_mod
--  ]
--modeInfo :: M.Map String ModeInfo
--modeInfo = M.fromList $ map (\x@(mode, _) -> (head (modeNames mode), x)) modeInfo_l

optsRun_rebuild :: Options -> IO (Validation ())
optsRun_rebuild opts = do
  events_ <- loadFiles
  case events_ of
    Left msgs -> return (Left msgs)
    Right events' -> do
      --mapM_ (putStrLn . show) records
      -- TODO: Instead of sorting in memory, it'd be better to write the commands to the DB, then process them in sorted order
      let events = sortBy (\a b -> compare (eventTime a) (eventTime b)) events'
      runSqlite "repl.db" $ do
        -- Create tables if necessary
        DB.databaseInit
        -- Delete existing rows
        deleteWhere ([] :: [Filter Event])
        deleteWhere ([] :: [Filter Item])
        deleteWhere ([] :: [Filter Property])
        -- Add the command records and process them
        mapM insert events
        result <- mapM processEvent events
        let result' = concatEithersN result
        return $ fmap (const ()) result'

processEvent :: Event -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
--processEvent event | trace ("event: "++(show event)) False = undefined
processEvent event = case eventType event of
  "createItem" -> do
    case eventToItems event of
      Left msgs -> return (Left (("error in event: "++(show event)):msgs))
      Right items -> do
        mapM_
          (\(ItemForJson item properties) -> do
            insert item
            mapM_
              (\(name, values) -> do
                mapM_ (\value -> do
                  let property = Property "item" (itemUuid item) name value
                  insert property
                  ) values
              ) (M.toList properties)
          ) items
        return (Right ())
  "patch1" -> do
    let file_ = eventToPatchFile1 event
    case file_ of
      Left msgs -> return (Left (("error in event: "++(show event)):msgs))
      Right file -> patchFile1 file
  s -> return (Left ["unrecognized event type: "++s])

--optsRun_rebuild' opts = do
--  x <- loadCommandRecords
--  case x of
--    Left msgs -> return (Left msgs)
--    Right records' -> do
--      --mapM_ (putStrLn . show) records
--      -- TODO: Instead of sorting in memory, it'd be better to write the commands to the DB, then process them in sorted order
--      let records = sortBy (\a b -> compare (Command.commandTime a) (Command.commandTime b)) records'
--      runSqlite "otot.db" $ do
--        -- Create tables if necessary
--        DB.databaseInit
--        -- Delete existing rows
--        deleteWhere ([] :: [Filter Command])
--        deleteWhere ([] :: [Filter Event])
--        deleteWhere ([] :: [Filter Item])
--        deleteWhere ([] :: [Filter Property])
--        -- Add the command records and process them
--        result <- mapM processRecord records
--        let result' = concatEithersN result
--        return $ fmap (const ()) result'

-- processRecord :: CommandRecord -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
-- --processRecord record | if (Command.commandCmd record) /= "add" then trace ("cmd: "++(T.unpack $ Command.commandCmd record)) False else False = undefined
-- processRecord record = do
--   let cmd = T.unpack $ Command.commandCmd record
--   case M.lookup cmd modeInfo of
--     Nothing -> return (Left ["unknown command `"++cmd++"`"])
--     Just (mode, run) -> do
--       let args = fmap T.unpack (Command.commandArgs record)
--       --if cmd /= "add" then liftIO $ print (modeNames mode) else return ()
--       case process mode args of
--         Left msg -> return (Left [msg])
--         Right opts -> do
--           case run of
--             ModeRunDB optsProcess1 optsProcess2 optsRunDB -> do
--               --if cmd /= "add" then liftIO $ print record else return ()
--               --if cmd /= "add" then liftIO $ print opts else return ()
--               handleOptions record opts mode optsProcess1 optsProcess2 optsRunDB
--             _ -> return (Left ["Command `"++cmd++"` is not valid while rebuilding"])
-- 
-- --handleOptions :: CommandRecord -> Mode Options -> 
-- handleOptions record opts mode optsProcess1 optsProcess2 optsRun = do
--   let time = Command.commandTime record
--   -- Validate options and add parameters required for the new command record
--   opts_ <- optsProcess1 opts
--   case opts_ of
--     Left msgs -> return (Left msgs)
--     Right opts' -> do
--       -- Convert Options to CommandRecord
--       let record = optsToCommandRecord time "default" opts'
--       --liftIO $ toStdErr record
--       -- TODO: Save CommandRecord to temporary file
--       -- TODO: CommandRecord read in from file
--       -- TODO: Verify that CommandRecords are equal
--       -- Convert CommandRecord to Command
--       let command = DB.recordToCommand record
--       --liftIO $ print (Command.commandCmd record)
--       -- Command saved to DB
--       insert command
--       -- TODO: Command loaded from DB
--       -- TODO: Verify that Commands are equal
--       -- TODO: Command converted to a CommandRecord
--       -- TODO: Verify that CommandRecords are equal
--       -- TODO: CommandRecord converted to Options
--       -- TODO: Verify that Options are equal
--       -- Options are validated and processed for modification of DB 'item' and 'property' tables
--       opts__ <- optsProcess2 opts'
--       case opts__ of
--         Left msgs -> return (Left msgs)
--         Right opts'' -> do
--           -- Update items and properties
--           x_ <- optsRun record opts''
--           case x_ of
--             Left msgs -> return (Left (msgs ++ [show record]))
--             Right _ -> return (Right ())

