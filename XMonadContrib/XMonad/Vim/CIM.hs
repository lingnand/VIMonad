-- provides a simple chinese pinyin input database
module XMonad.Vim.CIM
    ( dbOpen
    , wordList
    , dbClose
    , CIMDb) where

import Database.Berkeley.Db
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text.ICU.Convert as C
import Data.List
import Data.Char
import qualified Data.Text as T
import XMonad.Vim.Constants

pack :: String -> B.ByteString
pack = B.pack . map (fromIntegral . ord)

data CIMDb = CIMDb Db DbEnv C.Converter

dbOpen :: IO CIMDb
dbOpen = do
    -- get the converter
    converter <- C.open "gbk" Nothing
    dbenv <- dbEnv_create []
    dbEnv_open [DB_CREATE, DB_INIT_MPOOL] 0 dbenv =<< getXMonadDir 
    db <- db_create [] dbenv
    db_set_pagesize db 2048
    db_open [DB_RDONLY] DB_BTREE 0 db Nothing "gbk.bsddb" Nothing
    return $ CIMDb db dbenv converter

wordList :: CIMDb -> String -> IO [String]
wordList (CIMDb db _ converter) w =  do
    mwl <- db_get [] db Nothing (pack w)
    case mwl of
        Just wl -> return $ words $ T.unpack $ C.toUnicode converter wl
        _ -> return []

dbClose :: CIMDb -> IO ()
dbClose (CIMDb db db_env _) = do
    db_close [] db
    dbEnv_close [] db_env
