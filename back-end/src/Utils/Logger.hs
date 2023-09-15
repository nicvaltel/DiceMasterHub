module Utils.Logger where


data LgSeverity = LgCritical | LgInfo | LgMessage | LgConnection

logger :: LgSeverity -> String -> IO ()
logger _ = print