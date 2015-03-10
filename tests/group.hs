module Main where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (join)
import Control.Concurrent.Object
import Control.Concurrent.STM
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- | Group

newtype Group = Group { unGroup :: Object GMessage GReply }

type GroupId = Int
type MemberId = Int
type MemberName = String

data GState = GState
    { groupId :: GroupId
    , groupMembers :: Map.Map MemberId MemberName
    }

data GMessage
    = AddMember MemberId MemberName
    | RemoveMember MemberId
    | GetGroupId
    | GetMember MemberId
    | GetAllMembers

data GReply
    = AddMemberR Bool
    | RemoveMemberR Bool
    | GetGroupIdR GroupId
    | GetMemberR (Maybe MemberName)
    | GetAllMembersR [MemberId]
    deriving (Show)

instance ObjectLike IO Group where
    type OMessage Group = GMessage
    type OReply Group = GReply
    type OClass Group = Class GMessage GReply

    new cl = Group <$> new cl
    (Group obj) ! msg = obj ! msg
    (Group obj) !? msg = obj !? msg
    kill (Group obj) = kill obj

--------------------------------------------------------------------------------

newGroup :: GroupId -> IO Group
newGroup gid = new Class
    { classInitializer = return $ GState gid Map.empty
    , classFinalizer = (\_st -> putStrLn "Cleanup")
    , classCallbackModule = CallbackModule $ \self@Self{..} msg -> case msg of
            AddMember mid name -> do
                atomically $ modifyTVar' selfState
                        (\ gst -> gst { groupMembers = Map.insert mid name (groupMembers gst) })
                return (AddMemberR True, self)
            RemoveMember mid -> do
                atomically $ modifyTVar' selfState
                        (\ gst -> gst { groupMembers = Map.delete mid (groupMembers gst) })
                return (RemoveMemberR True, self)
            GetGroupId -> do
                gst <- atomically $ readTVar selfState
                return (GetGroupIdR $ groupId gst, self)
            GetMember mid -> do
                gst <- atomically $ readTVar selfState
                return (GetMemberR $ Map.lookup mid (groupMembers gst), self)
            GetAllMembers -> do
                mids :: [MemberId]
                    <- (map fst . Map.toList . groupMembers) <$> (atomically $ readTVar selfState)
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

    (join $ gr !? GetGroupId) >>= print
    (join $ gr !? GetMember 1) >>= print
    (join $ gr !? GetMember 2) >>= print
    (join $ gr !? GetAllMembers) >>= print

    kill gr
