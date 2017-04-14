import Control.Concurrent
import Control.Monad

n = 1000000

main = do
    left  <- newEmptyMVar
    right <- foldM make left [0..n-1]
    putMVar right 0    -- bang!
    x <- takeMVar left -- wait for completion
    print x
 where
    make l n = do
       r <- newEmptyMVar
       forkIO (thread n l r)
       return r

thread :: Int -> MVar Int -> MVar Int -> IO ()
thread _ l r = do
   v <- takeMVar r
   putMVar l $! v+1
