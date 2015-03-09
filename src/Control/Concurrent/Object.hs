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

runCallbackModule
    :: Self msg reply state
    -> msg
    -> IO (reply, Self msg reply state)
runCallbackModule self msg = (unCM $ selfModule self) self msg

-- | Make Object from Class.
new
    :: Class msg reply state
    -> IO (Object msg reply)
new Class{..} = do
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


class ObjectLike obj where
    type Message obj :: *
    type Reply obj :: *

    -- | Kill Object.
    kill :: obj -> IO ()

    -- Asynchronous sending
    (!) :: obj -> Message obj -> IO ()

    -- Synchronous sending
    (!?) :: obj -> Message obj -> IO (IO (Reply obj))

instance ObjectLike (Object msg reply) where
    type Message (Object msg reply) = msg
    type Reply (Object msg reply) = reply

    obj ! msg = atomically $ writeTChan (objChan obj) (msg, Nothing)

    obj !? msg = do
        mv <- newEmptyMVar
        atomically $ writeTChan (objChan obj) (msg, Just mv)
        -- TODO: timeout
        return $ readMVar mv

    kill obj = killThread $ objThreadId obj

instance ObjectLike (Self msg reply state) where
    type Message (Self msg reply state) = msg
    type Reply (Self msg reply state) = reply

    self ! msg = do
        _ <- runCallbackModule self msg
        return ()

    self !? msg = do
        (reply, _self') <- runCallbackModule self msg
        mv <- newMVar reply
        return $ readMVar mv

    kill self = killThread $ selfThreadId self
