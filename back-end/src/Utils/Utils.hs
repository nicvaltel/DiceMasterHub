module Utils.Utils where
import qualified Data.Text as T


data LgSeverity = LgCritical | LgError | LgInfo | LgMessage | LgConnection

logger :: LgSeverity -> String -> IO ()
logger _ = print

tshow :: Show a => a -> T.Text
tshow = T.pack . show