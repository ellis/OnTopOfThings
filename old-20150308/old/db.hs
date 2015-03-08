-- For documentation on HDBC, see http://book.realworldhaskell.org/read/using-databases.html

import Data.Maybe (catMaybes)
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

data Ticket = Ticket
  { ticketUuid :: String
  , ticketTitle :: String
  } deriving Show
--  tkt_mtime String
--  tkt_ctime String
--  -- Add as many fields as required below this line
--  tickettype String sql=type
--  status String Maybe
--  subsystem String Maybe
--  priority String Maybe
--  severity String Maybe
--  foundin String Maybe
--  private_contact String Maybe
--  resolution String Maybe
--  title String
--  comment String Maybe
--  -- Added by ellis
--  parent String Maybe
--  after String Maybe
--  estimate Int Maybe

main :: IO ()
main = handleSqlError $ do
  conn <- connectSqlite3 "test.fossil"
  l <- quickQuery' conn "SELECT tkt_uuid,tkt_ctime,title from ticket" []
  let l' = catMaybes $ map toTicket l
  mapM_ (\t -> putStrLn $ show t) l'

toTicket :: [SqlValue] -> Maybe Ticket
toTicket row =
  case row of
    [uuid, title, _] -> Just $ Ticket (fromSql uuid) (fromSql title)
    _ -> Nothing
