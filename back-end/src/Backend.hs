module Backend where


import Data.Text (Text)


data InputMessage = MoveMessage MoveData | ActionMessage ActionData | TextMessage Text

data MoveData

data ActionData

data User

data GameRoom

processInputMessage :: User -> GameRoom -> InputMessage -> IO ()
processInputMessage user gameRoom inputMsg = case inputMsg of
    MoveMessage moveData -> processMoveMessage user gameRoom moveData
    ActionMessage actionData -> processActionMessage user gameRoom actionData
    TextMessage text -> processTextMessage user gameRoom text


processTextMessage :: User -> GameRoom -> Text -> IO ()
processTextMessage = undefined -- TODO implement

processActionMessage :: User -> GameRoom -> ActionData -> IO ()
processActionMessage = undefined -- TODO implement

processMoveMessage :: User -> GameRoom -> MoveData -> IO ()
processMoveMessage user gameRoom moveData = do
    if (checkMoveIsValid gameRoom moveData)
        then do
            let newGameRoom = updateGameRoomState gameRoom moveData
            if (checkEndGameConditiion newGameRoom)
                then finishGameInRoom newGameRoom
                else sendMakeNextMoveToOtherPlayer user newGameRoom
        else sendRemakeMoveAnsver user gameRoom moveData

sendMakeNextMoveToOtherPlayer :: User -> GameRoom -> IO ()
sendMakeNextMoveToOtherPlayer = undefined

finishGameInRoom :: GameRoom -> IO ()
finishGameInRoom = undefined

checkEndGameConditiion :: GameRoom -> Bool
checkEndGameConditiion = undefined

updateGameRoomState :: GameRoom -> MoveData -> GameRoom
updateGameRoomState = undefined


sendRemakeMoveAnsver :: User -> GameRoom -> MoveData -> IO ()
sendRemakeMoveAnsver = undefined -- TODO implement



checkMoveIsValid :: GameRoom -> MoveData -> Bool
checkMoveIsValid = undefined -- TODO implement
    
