module Users.User where
    
import Data.Text (Text)
import Data.IntMap


data User = User {
    userId :: UserId,
    userName :: Username
}

type UserId = Int

type Username = Text

-- type UserFrom = User

-- type UserTo = User

class UserRepo db where
    findUserById :: UserId -> db (Maybe User)



