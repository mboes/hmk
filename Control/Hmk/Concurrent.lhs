
2007 Sep 15

The author disclaims copyright to this source. In place of a legal
notice, here is a blessing:

   May you do good and not evil.
   May you find forgiveness for yourself and forgive others.
   May you share freely, never taking more than you give.

(after the sqlite source code)

--

> module Control.Hmk.Concurrent where
>
> import {-# SOURCE #-} Control.Hmk
> import Control.Concurrent
> import Control.Monad.State
> import Control.Applicative
> import qualified Data.Map as Map

Map the dependency graph to a tree of concurrent threads, where each thread
encapsulates a recipe to execute after having waited for the completion of the
threads of each prerequesite. It is up to the runtime to schedule the
execution of the threads as it best sees fit.

However, interleaving the execution of potentially many thousands threads
simultaneously, which are usually all I/O bound, is a bad idea. So we do
enforce some rate limiting of the number of threads that can run
simultaneously by making all threads wait on the same quantity semaphore. The
quantity semaphore can be thought of as making available a fixed amount of
slots. So long as slots remain, threads may take a slot and run. But if slots
run out, the remaining threads must wait until existing threads have finished
their job.

Because one cannot wait for a thread to finish directly, we signal job
completion by means of MVar's. A finished thread writes some inessential value
to its MVar, which unblocks threads waiting on that MVar.

Note that there are two distinct monadic levels here. One level is the IO
monad to which a state transformer is applied. This level constructs and
combines monadic values that form the process tree. This process tree is then
pulled out of the state transformed IO monad and executed.

> data Done = Done
>
> processTree :: (Ord a, Show a) => Int -> DepGraph IO a -> IO ()
> processTree slots gr = do
>   sem <- newQSem slots
>   join $ evalStateT (sequence_ <$> mapM (aux sem) gr) Map.empty
>     where aux sem (Node x ps rule) = do
>             ws <- mapM (wait sem) ps
>             signal <- liftIO $ newEmptyMVar
>             modify (Map.insert x signal)
>             return $ do
>                 waitQSem sem
>                 sequence ws
>                 result <- maybe (return TaskSuccess) ($ prereqs rule) (recipe rule)
>                 case result of
>                   TaskSuccess -> do
>                     signalQSem sem
>                     putMVar signal Done
>                   TaskFailure -> error $ "Recipe for " ++ show x ++ " failed."
>           wait sem n@(Node x _ _) = do
>             seen <- get
>             case Map.lookup x seen of
>               Just signal -> return $ do
>                 readMVar signal
>                 return ()
>               Nothing -> do
>                 work <- aux sem n
>                 signal <- (Map.! x) <$> get
>                 return $ do
>                   tid <- forkIO work
>                   readMVar signal
>                   killThread tid
