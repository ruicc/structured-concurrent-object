module Main where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (join)
import Control.Concurrent.Object
import Control.Concurrent.STM
import qualified Data.Map as Map


type GroupId = Int
type MemberId = Int
type MemberName = String
type GState = Map.Map MemberId MemberName

data GMessage
    = AddMember MemberId MemberName
    | RemoveMember MemberId
    | GetMember MemberId
    | GetAllMembers

data GReply
    = AddMemberR Bool
    | RemoveMemberR Bool
    | GetMemberR (Maybe MemberName)
    | GetAllMembersR [MemberId]
    deriving (Show)

newtype Group = Group { unGroup :: Object GMessage GReply }

instance ObjectLike IO Group where
    type Message Group = GMessage
    type Reply Group = GReply

    (Group obj) ! msg = obj ! msg
    (Group obj) !? msg = obj !? msg
    kill (Group obj) = kill obj


newGroup :: GroupId -> IO Group
newGroup gid = Group <$> new Class
    { classInitializer = return Map.empty
    , classFinalizer = (\_st -> putStrLn "Cleanup")
    , classCallbackModule = CallbackModule $ \self@Self{..} msg -> case msg of
            AddMember mid name -> do
                atomically $ modifyTVar' selfState (Map.insert mid name)
                return (AddMemberR True, self)
            RemoveMember mid -> do
                atomically $ modifyTVar' selfState (Map.delete mid)
                return (RemoveMemberR True, self)
            GetMember mid -> do
                mems <- atomically $ readTVar selfState
                return (GetMemberR (Map.lookup mid mems), self)
            GetAllMembers -> do
                mids :: [MemberId]
                    <- (map fst . Map.toList) <$> (atomically $ readTVar selfState)
                return (GetAllMembersR mids, self)
    }

main :: IO ()
main = do
    let gid = 42

    gr :: Group <- newGroup gid

    gr ! AddMember 1 "Alice"
    gr ! AddMember 2 "Bob"
    gr ! AddMember 3 "Charlie"
    gr ! AddMember 4 "Edward"
    gr ! RemoveMember 2
    gr ! RemoveMember 3

    (join $ gr !? GetMember 1) >>= print
    (join $ gr !? GetMember 2) >>= print

    mids <- join $ gr !? GetAllMembers

    print mids
    kill gr
