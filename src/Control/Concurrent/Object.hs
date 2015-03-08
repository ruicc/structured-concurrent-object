module Control.Concurrent.Object where

import Prelude hiding (init)
--import Control.Concurrent.Structured
import Control.Monad
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
--      Group消滅時の処理
--          DONE
--   Options:
--      有限状態マシン gen_fsm
--          DONE(?)
--      メッセージハンドラのadd/remove (gen_event)
--          Classで。
--      メッセージハンドラの動的差し替え
--          ロジックのレコード化
--      状態のタイムアウト (openは30000msecのみ)
--          非同期イベント用フォークする？ This参照？

data Class msg reply = Class
    { onInit
    , onDeleted
    , onEvent
    }


data Object msg reply = Object
    { objThreadId :: ThreadId
    , objChan :: TChan (msg, Maybe (MVar reply))
    }


new :: IO state -> (state -> IO b) -> (Object msg reply -> msg -> state -> IO (reply, state)) -> IO (Object msg reply)
new init final action = do
    ch <- newTChanIO
    -- これだとイベントの差し替えができない
    tid <- forkIO $ bracket init final $ \ (s :: state) -> do
        let
            loop obj state = do
                (msg, mmv) <- atomically $ readTChan ch
                (reply, state')
                    <- action
                            obj -- アクション内で自身にメッセージ投げたい時に必要
                            msg
                            state
                case mmv of
                    Just mv -> putMVar mv reply
                    Nothing -> return ()
                loop obj state'

        tid <- myThreadId
        loop (Object tid ch) s
    return $ Object tid ch

-- Asynchronous send
(!) :: Object msg reply -> msg -> IO ()
obj ! msg = atomically $ writeTChan (objChan obj) (msg, Nothing)

-- Synchronous send/receive
(!?) :: Object msg reply -> msg -> IO (IO reply)
obj !? msg = do
    mv <- newEmptyMVar
    atomically $ writeTChan (objChan obj) (msg, Just mv)
    return $ takeMVar mv

kill :: Object msg reply -> IO ()
kill obj = killThread $ objThreadId obj


--------------------------------------------------------------------------------

data Msg1 = Inc | Get

test :: IO ()
test = do
    obj :: Object Msg1 Int
        <- new
                (return 42)
                (\_ -> putStrLn "cleanup")
                (\msg st -> case msg of
                    Inc -> putStrLn "Inc" >> return (st + 1, st + 1)
                    Get -> return (st, st))
    print =<< join (obj !? Get)
    obj ! Inc
    obj ! Inc
    obj ! Inc
    print =<< join (obj !? Get)
    kill obj
