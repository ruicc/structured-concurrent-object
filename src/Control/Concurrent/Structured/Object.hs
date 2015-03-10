module Control.Concurrent.Structured.Object
    ( module Control.Concurrent.Object
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Object
import Control.Concurrent.Object.Internal
import Control.Concurrent.Structured
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Cont



instance ObjectLike (ContT r IO) (Object msg reply) where
    type OMessage (Object msg reply) = msg
    type OReply (Object msg reply) = reply
    type OClass (Object msg reply) = Class msg reply

    new = liftIO . newObject

    obj ! msg = liftIO $ obj ! msg

    obj !? msg = liftIO (liftIO <$> obj !? msg)

    kill obj = liftIO $ kill obj

--instance ObjectLike (CIO r) (Self msg reply state) where
--    type OMessage (Self msg reply state) = msg
--    type OReply (Self msg reply state) = reply
--    type OClass (Self msg reply state) = Class msg reply
--
--    -- | Self should not be made by itself.
--    new = error "Self cannot be made"
--
--    self ! msg = do
--        _ <- runCallbackModuleCIO self msg
--        return ()
--
--    self !? msg = do
--        (reply, _self') <- runCallbackModuleCIO self msg
--        mv <- newMVar reply
--        return $ readMVar mv
--
--    kill self = killThread $ selfThreadId self
