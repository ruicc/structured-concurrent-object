module Control.Concurrent.Object
    ( Class(..), Object, Self(..), CallbackModule(..)
    , ObjectLike(..)
    ) where

import Prelude hiding (init, mod)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Object.Internal


instance ObjectLike IO (Object msg reply) where
    type OMessage (Object msg reply) = msg
    type OReply (Object msg reply) = reply
    type OClass (Object msg reply) = Class msg reply

    new = newObject

    obj ! msg = atomically $ writeTChan (objChan obj) (msg, Nothing)

    obj !? msg = do
        mv <- newEmptyMVar
        atomically $ writeTChan (objChan obj) (msg, Just mv)
        -- TODO: timeout
        return $ readMVar mv

    kill obj = killThread $ objThreadId obj


instance ObjectLike IO (Self msg reply state) where
    type OMessage (Self msg reply state) = msg
    type OReply (Self msg reply state) = reply
    type OClass (Self msg reply state) = Class msg reply

    -- | Self should not be made by itself.
    new = error "Self cannot be made"

    self ! msg = do
        _ <- runCallbackModule self msg
        return ()

    self !? msg = do
        (reply, _self') <- runCallbackModule self msg
        mv <- newMVar reply
        return $ readMVar mv

    kill self = killThread $ selfThreadId self
