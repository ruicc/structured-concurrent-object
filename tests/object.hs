module Main where

import Control.Monad (join)
import Control.Concurrent.Object
import Control.Concurrent.STM


data Msg
    = Inc
    | Get
    | Print String

main :: IO ()
main = do
    let
        klass = Class
            { classInitializer = (return (42 :: Int))
            , classFinalizer = (\_ -> putStrLn "cleanup")
            , classCallbackModule = CallbackModule $ \self@Self{..} msg -> case msg of
                    Inc -> do
                        atomically $ modifyTVar' selfState (+1)
                        mint <- join (self !? Get) -- Send sync message
                        self ! (Print $ "Inc: " ++ show mint) -- Send async message
                        return $ (mint, self)
                    Get -> do
                        st <- atomically $ readTVar selfState
                        return (Just st, self)
                    Print str -> do
                        putStrLn str
                        return $ (Nothing, self)
            }

    obj :: Object Msg (Maybe Int)
        <- new klass

    get1 <- (obj !? Get)
    get1 >>= print
    obj ! Inc
    obj ! Inc
    obj ! Inc
    get2 <- (obj !? Get)
    get2 >>= print
    kill obj
