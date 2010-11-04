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

> {-# LANGUAGE CPP #-}
> module Eval (Target(..), Stem(..), eval, evalNoMeta, Eval.isStale, substituteStem) where
>
> import Parse
> import Control.Hmk
> import qualified Control.Hmk.IO as IO
>
> import Data.Sequence (Seq)
> import qualified Data.Sequence as Seq
> import qualified Data.Foldable as Seq
> import qualified Data.Traversable as Seq
> import Data.Map (Map)
> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import Control.Applicative
> import Control.Monad.State
> import Control.Monad.Writer
> import Data.List (intercalate)
> import Data.Maybe (isNothing)
> import Data.Char (isDigit)
>
> import System.IO
> import System.Directory
> import System.Exit
> import System.Process
> import System.Posix.Env
> import System.Posix.Process (getProcessID)

Default shell to pass recipes to. The shell used should understand the -e and
-x switches, which means stop executing on first non-zero exit status and
trace execution, respectively.

> defaultShell = "/bin/sh"

#if ! MIN_VERSION_mtl(2,0,0)
> instance Monad m => Applicative (StateT s m) where
>     pure = return
>     (<*>) = ap
>
> instance (Monoid w, Monad m) => Applicative (WriterT w m) where
>     pure = return
>     (<*>) = ap
#endif

Targets are either regular files, virtual or patterns. After instantiation of
meta-rules the patterns disappear, leaving only files and virtual targets. But
it cannot be known until after evaluation of all rules whether a target is
virtual or not. However, thanks to lazy evaluation, we can lookup wether a
target is virtual or not now, if we promise to build it later.

> data Target = File { name :: FilePath }
>             | Virtual { name :: String }
>             | Pattern { name :: String }
>             | REPattern { name :: String }
>               deriving Show

> newtype RevAppend a = RevAppend a
>
> instance Monoid a => Monoid (RevAppend a) where
>     mempty = RevAppend mempty
>     mappend (RevAppend x) (RevAppend y) = RevAppend (mappend y x)

Only the name of targets matters when comparing them, so we make equality into
an equivalence relation rather than the default structural equality.

> instance Eq Target where
>     x == y = name x == name y
>
> instance Ord Target where
>     compare x y = compare (name x) (name y)

Variable references are substituted for their values, using the environment.

A list of tokens can be spliced, appended to, etc, to form a new list of
tokens. Freezing means turning a list of tokens into a string, which cannot be
manipulated any further. This string representation is used to export values
to the outside world, such as the system environment.

> freeze :: Seq String -> String
> freeze = intercalate " " . Seq.toList

The environment is a variable store that can be updated in place. It is very
much not a persistent abstraction. We implement it using an internal map
rather than using the system environment directly because using the system
environment would require retokenizing values when we look them up. The
content of this internal map is immediately reflected into the system
environment, however, because assignments that are exported need to be
available to subprocesses forked during sourcing and piping.

Evaluation substitutes values for all variable references, using the system
environment as a variable store. Assignments are executed first, then PRule's
are evaluated to Rule's, the data structure for rules used by Control.Hmk. The
return value is a sequence of functions mapping stems to rules. This is
because stems are synthesized as a by-product of meta-rule instantiaton, but
this instantiation is performed post evaluation.

> data Stem = Stem String | RESubMatches [String] | NoStem
>
> addVariable attr var val = do
>   modify (fmap (Map.insert var (val :: Seq String)))
>   case attr of
>     Export -> liftIO $ setEnv var (freeze val) True
>     Local -> return ()

The value of a variable is looked up by decreasing order of precedence in the
following places:

 - assignments on the command line,
 - assignments in the mkfile,
 - the environment.

> lookupVariable var = do
>   maybe Seq.empty id . msum <$>
>         sequence [ Map.lookup var . fst <$> get
>                  , Map.lookup var . snd <$> get
>                  , fmap Seq.singleton <$> liftIO (getEnv var)
>                  , return (Just Seq.empty) ]
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
> eval :: Map String (Seq String) -> Mkfile -> IO (Seq (Stem -> Rule IO Target))
> eval cmdline mkfile = evalStateT (init >> go) (cmdline, Map.empty)
>     where init = addVariable Export "MKSHELL" (Seq.singleton defaultShell)
>           go = mdo (rules, RevAppend virtuals) <- runWriterT (eval' virtuals mkfile)
>                    return rules
>
> eval' virtuals (Mkrule ts flags ps r cont) = do
>   flagsv <- evalFlags flags
>   let tag t | t `elem` virtuals        = Virtual t
>             | Set.member Flag_R flagsv = REPattern t
>             | '%':_ <- t               = Pattern t
>             | otherwise                = File t
>   tsv <- Seq.msum <$> Seq.mapM evalToken ts
>   psv <- fmap tag <$> Seq.msum <$> Seq.mapM evalToken ps
>   shell <- (`Seq.index` 0) <$> lookupVariable "MKSHELL"
>   let f tv stem = let t = if Set.member Flag_V flagsv
>                           then Virtual tv
>                           else tag tv
>                       rv = if Set.member Flag_N flagsv && isNothing r
>                            then Just $ evalRecipe tsv t flagsv psv stem shell ("touch " ++ name t)
>                            else fmap (evalRecipe tsv t flagsv psv stem shell) r
>                       cmp = Set.fold (\x cmp -> case x of
>                                                   Flag_P cmp -> evalCompare cmp
>                                                   _ -> cmp) Eval.isStale flagsv
>                   in if Set.member Flag_V flagsv
>                      then Rule t (Seq.toList psv) rv (\_ _ -> return True)
>                      else Rule t (Seq.toList psv) rv cmp
>   when (Set.member Flag_V flagsv) (Seq.mapM_ (tell . RevAppend . return) tsv)
>   (Seq.><) <$> pure (fmap f tsv) <*> eval' virtuals cont
> eval' virtuals (Mkassign attr var val cont) = do
>   -- xxx take into account attributes.
>   lits <- Seq.msum <$> Seq.mapM evalToken val
>   addVariable attr var lits
>   eval' virtuals cont
> eval' virtuals (Mkinsert file cont) = do
>   filev <- evalToken file
>   unless (Seq.length filev == 1)
>              (error "Insertion must evaluate to a unique filename.")
>   let fp = Seq.index filev 0
>   (Seq.><) <$> (eval' virtuals =<< parse fp <$> liftIO (readFile fp)) <*> eval' virtuals cont
> eval' virtuals (Mkinpipe command cont) = do
>   commandv <- Seq.msum <$> Seq.mapM evalToken command
>   when (Seq.length commandv == 0)
>              (error "Command evaluated to empty string.")
>   let (cmd:args) = Seq.toList commandv
>   (_, Just outh, _, ph) <- liftIO $ createProcess (proc cmd args) { std_out = CreatePipe }
>   result <- liftIO $ hGetContents outh
>   code <- liftIO $ waitForProcess ph
>   case code of
>     ExitSuccess ->
>         (Seq.><) <$> eval' virtuals (parse "<pipe>" result) <*> eval' virtuals cont
>     ExitFailure n ->
>         error $ "Sub-process " ++ cmd ++ " exited with error status " ++ show n ++ "."
> eval' virtuals Mkeof = return Seq.empty

A recipe is executed by supplying the recipe as standard input to the shell.
(Note that unlike make, hmk feeds the entire recipe to the shell rather than
running each line of the recipe separately.)

> substituteStem (Stem stem) (Pattern ('%':suffix)) = File (stem ++ suffix)
> substituteStem (RESubMatches matches) (REPattern pat) = File (subst pat)
>     where subst "" = ""
>           subst ('\\':n:xs) | isDigit n = matches !! (read [n] - 1) ++ subst xs
>           subst (x:xs) = x : subst xs
> substituteStem _ x = x
>
> evalRecipe alltarget target flags prereq stem shell text newprereq = do
>   pid <- show <$> getProcessID
>   setEnv "alltarget" (freeze alltarget) True
>   setEnv "newprereq" (intercalate " " (map (name . substituteStem stem) newprereq)) True
>   setEnv "newmember" "" True -- xxx aggregates not supported.
>   setEnv "nproc" (show 0) True
>   setEnv "pid" pid True
>   setEnv "prereq" (freeze (fmap (name . substituteStem stem) prereq)) True
>   case stem of
>     Stem stem -> setEnv "stem" stem True
>     RESubMatches stems -> do
>       let nstems = zip [1..] stems
>       mapM_ (\(n, stem) -> setEnv ("stem" ++ show n) stem True) nstems
>     NoStem -> return ()
>   setEnv "target" (name $ substituteStem stem $ target) True
>   let p = if Set.member Flag_Q flags
>           then proc shell ["-e"]
>           else proc shell ["-ex"]
>   (Just inh, _, _, ph) <- createProcess p { std_in = CreatePipe }
>   hSetBinaryMode inh False
>   hPutStrLn inh text
>   hClose inh
>   code <- waitForProcess ph >>= IO.testExitCode
>   let final = if Set.member Flag_E flags
>               then return TaskSuccess else return code :: IO Result
>   let final' = if Set.member Flag_D flags
>               then case code of
>                        TaskFailure -> removeFile (name target) >> final
>                        TaskSuccess -> final
>               else final
>   final'

Parsing flags has to be done at evaluation time because we allow variable
references in place of flag characters, for instance to programmatically turn
on or off verbosity of rules, etc. The content of the flags field of a rule is
necessarily either a collation or a literal.

> evalFlags toks = do
>   v <- Seq.msum <$> Seq.mapM evalToken toks
>   return $ interp (freeze v)
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

> evalCompare cmp (File x) (File y) = do
>   code <- rawSystem cmp [x, y]
>   case code of
>       ExitSuccess -> return False
>       ExitFailure _ -> return True

The default comparison action.

> isStale (File x) (File y) = IO.isStale x y
> isStale (File x) (Virtual y) = return True
> isStale _ _ = error "impossible."

Version of eval where stems are instantiated to the empty string.

> evalNoMeta :: Map String (Seq String) -> Mkfile -> IO (Seq (Rule IO Target))
> evalNoMeta cmdline mkfile = fmap ($ NoStem) <$> eval cmdline mkfile
