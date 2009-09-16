
2009 Apr 19

The author disclaims copyright to this source. In place of a legal
notice, here is a blessing:

   May you do good and not evil.
   May you find forgiveness for yourself and forgive others.
   May you share freely, never taking more than you give.

(after the sqlite source code)

--

Construct rules with recipes and other actions in the IO monad.
Staleness of targets determined according to stat(2) system call, or
using user-defined comparison functions in the IO monad.

This module should be imported qualified.

> module Control.Hmk.IO where
> import Control.Hmk hiding (isStale)
> import Control.Monad (foldM)
> import System.FilePath
> import System.Posix.Files
> import System.Exit

Staleness check.

> isStale :: Cmp IO FilePath
> isStale x y = do
>   xe <- fileExist x
>   ye <- fileExist x
>   if not (xe && ye) then
>       return True else
>       do xstat <- getFileStatus x
>          ystat <- getFileStatus y
>          let xtime = modificationTime xstat
>              ytime = modificationTime ystat
>          return (xtime < ytime)

Project exit code to task result.

> testExitCode :: ExitCode -> IO Result
> testExitCode ExitSuccess = return TaskSuccess
> testExitCode (ExitFailure _) = return TaskFailure

Perform each system action, aborting if an action returns
non-zero exit code.

> abortOnError :: [IO Result] -> IO Result
> abortOnError = foldM f TaskSuccess where
>     f TaskSuccess k = k
>     f TaskFailure _ = error "Command exited with non-zero status."
