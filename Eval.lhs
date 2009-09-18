Copyright (C) 2009 Mathieu Boespflug <mboes@tweag.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

> module Eval (eval) where
>
> import Parse
> import Control.Hmk
> import qualified Control.Hmk.IO as IO
>
> import Data.Sequence (Seq)
> import qualified Data.Sequence as Seq
> import qualified Data.Traversable as Seq
> import qualified Data.Foldable as Seq
> import Control.Applicative
> import Control.Monad.State
> import Data.List (intercalate)
>
> import System.IO
> import System.Process
> import System.Posix.Env
> import System.Posix.Process (getProcessID)


Variable references are substituted for their values, using the environment.

The environment is a variable store that can be updated in place. It is very
much not a persistent abstraction, so we implement it with an impure
structure. For simplicity that structure is the system environment, since
we'll need to communicate the environment to child processes using the system
environment anyways.

A list of tokens can be spliced, appended to, etc, to form a new list of
tokens. Freezing means turning a list of tokens into a string, which cannot be
manipulated any further. This string representation is used to export values
to the outside world, such as the system environment.

> freeze :: Seq String -> String
> freeze = intercalate " " . Seq.toList

Evaluation substitutes values for all variable references, using the system
environment as a variable store. Assignments are executed first, then PRule's
are evaluated to Rule's, the data structure for rules used by Control.Hmk. The
return value is a sequence of functions mapping stems to rules. This is
because stems are synthesized as a by-product of meta-rule instantiaton, but
this instantiation is performed post evaluation.

> type Stem = String
>
> eval :: Mkfile                -- ^ Result of the parser.
>      -> IO (Seq (Stem -> Rule IO FilePath))
> eval (rules, assigns) = do
>   Seq.mapM_ evalAssignment assigns
>   Seq.msum <$> Seq.mapM evalRule rules
>     where evalRule (PRule pt pps pr pcmp) = do
>                          ts <- Seq.mapM evalToken pt
>                          ps <- Seq.mapM evalToken pps
>                          -- xxx support user supplied compare.
>                          let f t stem = let r = fmap (evalRecipe ts t ps stem) pr
>                                         in Rule t (Seq.toList ps) r IO.isStale
>                          return $ fmap f ts
>           evalAssignment (Assign attr var value) = do
>                          -- xxx take into account attributes.
>                          lits <- Seq.mapM evalToken value
>                          setEnv var (freeze lits) True
>           evalToken (Ref toks) = do
>                          var <- Seq.mapM evalToken toks
>                          maybe "" id <$> getEnv (freeze var)
>           evalToken (Lit x) = return x
>           evalRecipe alltarget target prereq stem text newprereq = do
>             pid <- show <$> getProcessID
>             setEnv "alltarget" (freeze alltarget) True
>             setEnv "newprereq" (intercalate " " newprereq) True
>             setEnv "newmember" "" True -- xxx aggregates not supported.
>             setEnv "nproc" (show 0) True
>             setEnv "pid" pid True
>             setEnv "prereq" (freeze prereq) True
>             setEnv "stem" stem True
>             setEnv "target" target True
>             (Just inh, _, _, ph) <-
>                         createProcess (proc "/bin/sh" ["-e"]) { std_in = CreatePipe }
>             hSetBinaryMode inh False
>             hPutStr inh text
>             waitForProcess ph >>= IO.testExitCode
