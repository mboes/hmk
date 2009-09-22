
2007 Sep 15

The author disclaims copyright to this source. In place of a legal
notice, here is a blessing:

   May you do good and not evil.
   May you find forgiveness for yourself and forgive others.
   May you share freely, never taking more than you give.

(after the sqlite source code)

--

A Haskell implementation of the plan9 mk program. This can be used as
a standalone program or as a library, for convenience.

> module Control.Hmk ( module Control.Hmk.Analyze
>                    , mk
>                    , Cmp, Rule(..), Task
>                    , Schedule, Result(..) ) where
>
> import Control.Hmk.Analyze
> import Control.Applicative
> import Control.Monad.State
> import Control.Monad.Reader
> import Data.List (find)
> import qualified Data.Set as Set

Hmk manages dependencies between entities. These dependencies are
specified by means of rules, establishing a dependency between the
target and prerequesites. Rules also package a means of comparing
targets and prerequisites to determine whether the target is out of
date.

> type Cmp m a = a -> a -> m Bool
> data Rule m a = Rule { target :: a
>                      , prereqs :: [a]
>                      , recipe :: Maybe ([a] -> Task m)
>                      , isStale :: Cmp m a }
>
> instance Show a => Show (Rule m a) where
>     show rule = "Rule " ++ show (target rule) ++ " " ++ show (prereqs rule)

The rules induce a dependency graph. It is from this dependency graph
that we will compute a schedule, ie a list of tasks.

> type DepGraph m a = [Tree m a]
> data Tree m a = Node a (DepGraph m a) (Rule m a)
>                 deriving Show
>
> data Result = TaskSuccess | TaskFailure
>               deriving (Eq, Show)
> type Task m = m Result
> type Schedule m = [Task m]

Here's how we construct one, with the given set of targets as the
roots of the DAG. The dependency graph is represented as a forest.
This is the natural representation in a functional language. We could
of course use adjacency lists or matrices, but that would only
complicate the code for essentially no gain, and perhaps even a
performance hit.

One could consider the forest constructed hither as the reification of
control induced by the graph structure. One could argue that a tree is
wasteful but it is always possible to build it with sharing of
subtrees, even if the sharing cannot be observed. But we dispense with
this sophistication, trusting instead that the garbage collector will
work hard enough that only the current path down the tree is in memory
while traversing it.

Invariant 1: The targets in each rule should not appear as targets in
any other rules.

Invariant 2: The induced graph must be acyclic.

Invariant 3: every prerequisite should be the target of some rule.

> depgraph :: Ord a => [Rule m a] -> [a] -> DepGraph m a
> depgraph rules targets = runReader (mapM aux targets) Set.empty where
>     aux x = do
>       visited <- ask
>       if x `Set.member` visited then
>          error "Cycle detected." else
>           case find (\r -> target r == x) rules of
>             Just rule -> mdo
>               let n = Node x ps rule
>               ps <- local (Set.insert x) $ mapM aux (prereqs rule)
>               return n
>             Nothing -> error "Invariant 3 violated."

From the mk(1) manual:

"A target is considered up to date if it has no prerequisites or if
all its prerequisites are up to date and it is newer than all its
prerequisites. Once the recipe for a target has executed, the target
is considered up to date."

So let's remove all those targets that are up to date. We detect
targets that do not exist by comparing them with themselves with the
isStale function of the rule.

> prune :: (Applicative m, Monad m) => DepGraph m a -> m (DepGraph m a)
> prune = foldM aux [] where
>     aux gr (Node x ps rule) = do
>       ps' <- prune ps
>       if null ps' then
>          do ood <- or <$> mapM (isStale rule x) (x : prereqs rule)
>             if ood then
>                return $ Node x ps' rule : gr else
>                return gr
>          else return $ Node x ps' rule : gr

Given a dependency graph take the longest path to each out of date
dependency and execute recipes in reverse order. Schedule recipes for
execution at most once. This can be done with a simple topological
sort because at this stage the graph now contains exactly those nodes
that need to be built.

> schedule :: Ord a => DepGraph m a -> Schedule m
> schedule gr = reverse $ evalState (foldM aux [] gr) Set.empty where
>     aux result (Node x ps rule) = do
>       visited <- get
>       if x `Set.member` visited then
>          return result else
>          do put (Set.insert x visited)
>             tasks <- foldM aux result ps
>             return $ maybe tasks ((:tasks) . ($ prereqs rule)) (recipe rule)

Let's piece everything together.

> mk :: (Ord a, Applicative m, Monad m) => [Rule m a] -> [a] -> m (Schedule m)
> mk rules targets = schedule <$> (prune $ depgraph rules targets)

** Tests **

> rl x ps = Rule x ps (Just (\_ -> putStrLn (show x) >> return TaskSuccess))
>           (\_ _ -> return True)
> t1 = [rl 1 [2,3], rl 2 [4], rl 3 [4], rl 4 []]
> t2 = [rl 1 [2,3], rl 2 [4], rl 3 [4], rl 4 [6,5], rl 5 [], rl 6 [5]]
