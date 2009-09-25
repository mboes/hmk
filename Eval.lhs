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
> import qualified Data.Map as Map
> import qualified Data.Foldable as Map
> import qualified Data.Set as Set
> import Control.Applicative
> import Control.Monad.State
> import Data.List (intercalate)
> import Data.Maybe (isNothing)
>
> import System.IO
> import System.Directory
> import System.Exit
> import System.Process
> import System.Posix.Env
> import System.Posix.Process (getProcessID)

Default shell to pass recipes to. The shell used should understand the "-e"
switch, which means stop executing on first non-zero exit status.

> defaultShell = "/bin/sh"

> instance Monad m => Applicative (StateT s m) where
>     pure = return
>     (<*>) = ap

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
> addVariable attr var val = do
>   modify (Map.insert var (val :: Seq String))
>   case attr of
>     Export -> liftIO $ setEnv var (freeze val) True
>     Local -> return ()
>
> lookupVariable var = Map.findWithDefault (Seq.empty) var <$> get
>
> evalToken (Lit x) = return (Seq.singleton x)
> evalToken (Coll toks) = Seq.singleton <$> Seq.concat <$>
>                         Seq.mapM ((f <$>) . evalToken) toks
>     where f ls | Seq.length ls == 1 = Seq.index ls 0
>                | otherwise = error "Cannot collate lists."
> evalToken (Ref tok) = do
>                var <- evalToken tok
>                lookupVariable (freeze var)
>
> eval :: Mkfile -> IO (Seq (Stem -> Rule IO FilePath))
> eval mkfile = evalStateT (init >> eval' mkfile) Map.empty
>     where init = addVariable Export "MKSHELL" (Seq.singleton defaultShell)
>
> eval' (Mkrule ts flags ps r cont) = do
>   tsv <- Seq.msum <$> Seq.mapM evalToken ts
>   flagsv <- evalFlags flags
>   psv <- Seq.msum <$> Seq.mapM evalToken ps
>   -- xxx support user supplied compare.
>   shell <- (`Seq.index` 0) <$> lookupVariable "MKSHELL"
>   let f t stem = let rv = if Set.member Flag_N flagsv && isNothing r
>                           then Just $ evalRecipe tsv t flagsv psv stem shell ("touch " ++ t)
>                           else fmap (evalRecipe tsv t flagsv psv stem shell) r
>                      cmp = Set.fold (\x cmp -> case x of
>                                                    Flag_P cmp -> evalCompare cmp
>                                                    _ -> cmp) IO.isStale flagsv
>                  in let cmp = if Set.member Flag_V flagsv
>                               then (\_ _ -> return True :: IO Bool)
>                               else cmp
>                     in Rule t (Seq.toList psv) rv IO.isStale
>   (Seq.><) <$> pure (fmap f tsv) <*> eval' cont
> eval' (Mkassign attr var val cont) = do
>   -- xxx take into account attributes.
>   lits <- Seq.msum <$> Seq.mapM evalToken val
>   addVariable attr var lits
>   eval' cont
> eval' (Mkinsert file cont) = do
>   filev <- evalToken file
>   unless (Seq.length filev == 1)
>              (error "Insertion must evaluate to a unique filename.")
>   let fp = Seq.index filev 1
>   (Seq.><) <$> (eval' =<< parse fp <$> liftIO (readFile fp)) <*> eval' cont
> eval' (Mkinpipe file cont) = do
>   filev <- evalToken file
>   let fp = Seq.index filev 1
>   (_, Just outh, _, ph) <- liftIO $ createProcess (proc fp []) { std_out = CreatePipe }
>   result <- liftIO $ hGetContents outh
>   liftIO $ waitForProcess ph
>   (Seq.><) <$> eval' (parse "<pipe>" result) <*> eval' cont
> eval' Mkeof = return Seq.empty

A recipe is executed by supplying the recipe as standard input to the shell.
(Note that unlike make, hmk feeds the entire recipe to the shell rather than
running each line of the recipe separately.)

> evalRecipe alltarget target flags prereq stem shell text newprereq = do
>   pid <- show <$> getProcessID
>   setEnv "alltarget" (freeze alltarget) True
>   setEnv "newprereq" (intercalate " " newprereq) True
>   setEnv "newmember" "" True -- xxx aggregates not supported.
>   setEnv "nproc" (show 0) True
>   setEnv "pid" pid True
>   setEnv "prereq" (freeze prereq) True
>   setEnv "stem" stem True
>   setEnv "target" target True
>   if not (Set.member Flag_Q flags) then putStr text else return ()
>   (Just inh, _, _, ph) <- createProcess (proc shell ["-e"])
>                           { std_in = CreatePipe }
>   hSetBinaryMode inh False
>   hPutStr inh text
>   code <- waitForProcess ph >>= IO.testExitCode
>   let final = if Set.member Flag_E flags
>               then return TaskSuccess else return code :: IO Result
>   let final = if Set.member Flag_D flags
>               then case code of
>                        TaskFailure -> removeFile target >> final
>                        TaskSuccess -> final
>               else final
>   final

Parsing flags has to be done at evaluation time because we allow variable
references in place of flag characters, for instance to programmatically turn
on or off verbosity of rules, etc. The content of the flags field of a rule is
necessarily either a collation or a literal.

> evalFlags Nothing = return Set.empty
> evalFlags (Just tok) = do
>   v <- evalToken tok
>   when (Seq.length v /= 1) (error "No spaces allowed in flags field.")
>   return $ interp (Seq.index v 1)
>     where interp "" = Set.empty
>           interp ('D':xs) = Set.insert Flag_D (interp xs)
>           interp ('E':xs) = Set.insert Flag_E (interp xs)
>           interp ('N':xs) = Set.insert Flag_N (interp xs)
>           interp ('n':xs) = Set.insert Flag_n (interp xs)
>           interp ('P':xs) = Set.singleton (Flag_P xs)
>           interp ('Q':xs) = Set.insert Flag_Q (interp xs)
>           interp ('R':xs) = Set.insert Flag_R (interp xs)
>           interp ('U':xs) = Set.insert Flag_U (interp xs)
>           interp ('V':xs) = Set.insert Flag_V (interp xs)

A user supplied comparison command is wrapped into an action in the IO monad.

> evalCompare cmp x y = do
>   code <- rawSystem cmp [x, y]
>   case code of
>       ExitSuccess -> return False
>       ExitFailure _ -> return True


Version of eval where stems are instantiated to the empty string.

> evalNoMeta :: Mkfile -> IO (Seq (Rule IO FilePath))
> evalNoMeta mkfile = fmap ($ "") <$> eval mkfile
