module Control.Concurrent.Object.Internal
    ( Class(..), Object(..), Self(..), CallbackModule(..)
    , ObjectLike(..)
    ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

data Class m msg reply state
    = Class
    { classInitializer :: m state
    , classFinalizer :: state -> m ()
    , classCallbackModule :: CallbackModule m msg reply state
    }

data Object msg reply
    = Object
    { objThreadId :: ThreadId
    , objChan :: TChan (msg, Maybe (MVar reply))
    }

data Self m msg reply state
    = Self
    { selfThreadId :: ThreadId
    , selfChan :: TChan (msg, Maybe (MVar reply)) -- ^ Necesarry? Message sent in action would be evaluated in time, not via Channel.
    , selfModule :: CallbackModule m msg reply state -- ^ Not TVar. CallbackModule must not be changed during action.
    , selfState :: TVar state
    }

newtype CallbackModule m msg reply state
    = CallbackModule { unCM :: Self m msg reply state -> msg -> m (reply, Self m msg reply state) }

class ObjectLike m obj where
    type OMessage obj :: *
    type OReply obj :: *
    type OClass obj :: * -> *

    -- | Make Object.
    new :: OClass obj state -> m obj

    -- | Kill Object.
    kill :: obj -> m ()

    -- Asynchronous sending
    (!) :: obj -> OMessage obj -> m ()

    -- Synchronous sending
    (!?) :: obj -> OMessage obj -> m (m (OReply obj))
