module Control.Concurrent.Object
    ( Class(..), Object, Self(..), CallbackModule(..)
    , ObjectLike(..)
    ) where

import Prelude hiding (init, mod)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Object.Internal


runCallbackModuleIO
    :: Self IO msg reply state
    -> msg
    -> IO (reply, Self IO msg reply state)
runCallbackModuleIO self msg = (unCM $ selfModule self) self msg

newObjectIO
    :: Class IO msg reply state
    -> IO (Object msg reply)
newObjectIO Class{..} = do
    ch <- newTChanIO
    tid <- forkIO $ bracket classInitializer classFinalizer $ \ (st :: state) -> do
        let
            loop self = do
                (msg, mmv)
                    <- atomically $ readTChan ch
                (reply, self')
                    <- runCallbackModuleIO self msg
                case mmv of
                    Just mv -> putMVar mv reply
                    Nothing -> return ()
                loop $ self'

        tid <- myThreadId
        tst <- newTVarIO st
        loop $ Self tid ch classCallbackModule tst

    return $ Object tid ch


instance ObjectLike IO (Object msg reply) where
    type OMessage (Object msg reply) = msg
    type OReply (Object msg reply) = reply
    type OClass (Object msg reply) = Class IO msg reply

    new = newObjectIO

    obj ! msg = atomically $ writeTChan (objChan obj) (msg, Nothing)

    obj !? msg = do
        mv <- newEmptyMVar
        atomically $ writeTChan (objChan obj) (msg, Just mv)
        -- TODO: timeout
        return $ readMVar mv

    kill obj = killThread $ objThreadId obj


instance ObjectLike IO (Self IO msg reply state) where
    type OMessage (Self IO msg reply state) = msg
    type OReply (Self IO msg reply state) = reply
    type OClass (Self IO msg reply state) = Class IO msg reply

    -- | Self should not be made by itself.
    new = error "Self cannot be made"

    self ! msg = do
        _ <- runCallbackModuleIO self msg
        return ()

    self !? msg = do
        (reply, _self') <- runCallbackModuleIO self msg
        mv <- newMVar reply
        return $ readMVar mv

    kill self = killThread $ selfThreadId self
