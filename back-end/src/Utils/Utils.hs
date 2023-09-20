module Utils.Utils where
import qualified Data.Text as T
import Text.Read (readMaybe)


data LgSeverity = LgCritical | LgError | LgInfo | LgMessage | LgConnection | LgDebug

logger :: LgSeverity -> String -> IO ()
logger _ = print

tshow :: Show a => a -> T.Text
tshow = T.pack . show

tReadMaybe :: Read a => T.Text -> Maybe a 
tReadMaybe = readMaybe . T.unpack