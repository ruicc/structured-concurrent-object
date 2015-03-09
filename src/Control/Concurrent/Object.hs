module Control.Concurrent.Object
    ( Class(..), Object, Self(..), CallbackModule(..)
    , new
    , ObjectLike(..)
    ) where

import Prelude hiding (init, mod)
--import Control.Concurrent.Structured
--import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

-- | TODO:
--      Group生成
--          DONE
--      AddMemberに対して処理
--          DONE
--      エラーハンドリング
--          bracketはあるけど..
--          親に通知が必要か(Async?)
--      Group消滅時の処理
--          DONE
--   Options:
--      有限状態マシン gen_fsm
--          DONE(?)
--      メッセージハンドラのadd/remove (gen_event)
--          とりあえずClassで。
--      メッセージハンドラの動的差し替え
--          ロジックのレコード化
--      状態のタイムアウト (openは30000msecのみ)
--          非同期イベント用フォークする？
--              しない、内部イベントは外部イベントとは別途処理
--          This参照的なことはどうする？
--              Selfをactionに渡した

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

runCallbackModuleIO
    :: Self IO msg reply state
    -> msg
    -> IO (reply, Self IO msg reply state)
runCallbackModuleIO self msg = (unCM $ selfModule self) self msg

newObject
    :: Class IO msg reply state
    -> IO (Object msg reply)
newObject Class{..} = do
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


class ObjectLike m obj where
    type Message obj :: *
    type Reply obj :: *
    type Klass obj :: * -> *

    -- | Make Object.
    new :: Klass obj state -> m obj

    -- | Kill Object.
    kill :: obj -> m ()

    -- Asynchronous sending
    (!) :: obj -> Message obj -> m ()

    -- Synchronous sending
    (!?) :: obj -> Message obj -> m (m (Reply obj))

instance ObjectLike IO (Object msg reply) where
    type Message (Object msg reply) = msg
    type Reply (Object msg reply) = reply
    type Klass (Object msg reply) = Class IO msg reply

    new = newObject

    obj ! msg = atomically $ writeTChan (objChan obj) (msg, Nothing)

    obj !? msg = do
        mv <- newEmptyMVar
        atomically $ writeTChan (objChan obj) (msg, Just mv)
        -- TODO: timeout
        return $ readMVar mv

    kill obj = killThread $ objThreadId obj

instance ObjectLike IO (Self IO msg reply state) where
    type Message (Self IO msg reply state) = msg
    type Reply (Self IO msg reply state) = reply

    -- | Self should not be made by itself.
    new = undefined

    self ! msg = do
        _ <- runCallbackModuleIO self msg
        return ()

    self !? msg = do
        (reply, _self') <- runCallbackModuleIO self msg
        mv <- newMVar reply
        return $ readMVar mv

    kill self = killThread $ selfThreadId self
