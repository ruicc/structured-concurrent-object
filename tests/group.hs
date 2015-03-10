module Main where

import           Prelude hiding (lookup)
import           Control.Applicative ((<$>))
import           Control.Monad (join)
import           Control.Concurrent.Object
import           Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.Map as Map

import           Control.Monad.Trans.Cont (ContT)
import           Control.Concurrent.Structured (liftIO)
import qualified Control.Concurrent.Structured as CS

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

instance ObjectLike (ContT r IO) Group where
    type OMessage Group = GMessage
    type OReply Group = GReply
    type OClass Group = Class GMessage GReply

    new cl = liftIO $ Group <$> new cl
    (Group obj) ! msg = liftIO $ obj ! msg
    (Group obj) !? msg = liftIO $ (liftIO <$> obj !? msg)
    kill (Group obj) = liftIO $ kill obj

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

main1 :: IO ()
main1 = do
    let
        gid :: Int
        gid = 42

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

main2 :: IO ()
main2 = CS.runConcurrent $ do
    let
        gid :: Int
        gid = 42

        put :: String -> CS.Concurrent ()
        put = liftIO . putStrLn

    gr :: Group <- liftIO $ newGroup gid

    gr ! AddMember 1 "Alice"
    gr ! AddMember 2 "Bob"
    gr ! AddMember 3 "Charlie"
    gr ! AddMember 4 "Edward"
    gr ! RemoveMember 2
    gr ! RemoveMember 3


    (join $ gr !? GetGroupId) >>= liftIO . print
    (join $ gr !? GetMember 1) >>= liftIO . print
    (join $ gr !? GetMember 2) >>= liftIO . print
    (join $ gr !? GetAllMembers) >>= liftIO . print

    kill gr

main :: IO ()
main = main1 >> main2
