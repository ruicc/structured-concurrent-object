module Control.Concurrent.Object.Internal
    ( Class(..), Object(..), Self(..), CallbackModule(..)
    , ObjectLike(..)
    , runCallbackModule
    , newObject
    ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

data Class msg reply state
    = Class
    { classInitializer :: IO state
    , classFinalizer :: state -> IO ()
    , classCallbackModule :: CallbackModule msg reply state
    }

data Object msg reply
    = Object
    { objThreadId :: ThreadId
    , objChan :: TChan (msg, Maybe (MVar reply))
    }

data Self msg reply state
    = Self
    { selfThreadId :: ThreadId
    , selfChan :: TChan (msg, Maybe (MVar reply)) -- ^ Necesarry? Message sent in action would be evaluated in time, not via Channel.
    , selfModule :: CallbackModule msg reply state -- ^ Not TVar. CallbackModule must not be changed during action.
    , selfState :: TVar state
    }

newtype CallbackModule msg reply state
    = CallbackModule { unCM :: Self msg reply state -> msg -> IO (reply, Self msg reply state) }

class ObjectLike m obj where
    type OMessage obj :: *
    type OReply obj :: *
    type OClass obj :: * -> *

    -- | Make Object.
    new :: OClass obj state -> m obj

    -- | Kill Object.
    kill :: obj -> m ()

    -- Asynchronous sending a message.
    (!) :: obj -> OMessage obj -> m ()

    -- Synchronous sending a message.
    (!?) :: obj -> OMessage obj -> m (m (OReply obj))


runCallbackModule
    :: Self msg reply state
    -> msg
    -> IO (reply, Self msg reply state)
runCallbackModule self msg = (unCM $ selfModule self) self msg

newObject :: Class msg reply state -> IO (Object msg reply)
newObject Class{..} = do
    ch <- newTChanIO
    tid <- forkIO $ bracket classInitializer classFinalizer $ \ (st :: state) -> do
        let
            loop self = do
                (msg, mmv)
                    <- atomically $ readTChan ch
                (reply, self')
                    <- runCallbackModule self msg
                case mmv of
                    Just mv -> putMVar mv reply
                    Nothing -> return ()
                loop $ self'

        tid <- myThreadId
        tst <- newTVarIO st
        loop $ Self tid ch classCallbackModule tst

    return $ Object tid ch
