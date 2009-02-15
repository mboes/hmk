
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

> module Control.Hmk ( mk
>                    , Cmp, Rule(..), Task
>                    , Schedule, Result(..)) where
>
> import qualified Data.Graph.Inductive as G
> import Control.Monad
> import Data.List (nub, zipWith3)
> import Data.Maybe (fromJust)
> import qualified Data.Map as Map


Hmk manages dependencies between entities. These dependencies are
specified by means of rules, establishing a dependency between the
target and prerequesites. Rules also package a means of comparing
targets and prerequisites to determine whether the target is out of
date.

> type Cmp m a = a -> a -> m Bool
> data Rule m a = Rule { target :: a
>                      , prereqs :: [a]
>                      , recipe :: Task m
>                      , isOOD :: Cmp m a }

The rules induce a dependency graph. We label each edge with its
destination for convenience.

> type DepGraph m a = G.Gr (Label m a) a
> type Context m a = G.Context (Label m a) a

where labels are represented as follows. We use tags to mark out of
date nodes. Currently we tag each node with the list of out of date
prerequesites.

> data Label m a = Label { lbl_rule :: Rule m a
>                        , lbl_tag :: [a] }
>
> instance Show a => Show (Label m a) where
>     show (Label r _) = show (target r)

The dependency graph is used to determine a set of tasks to accomplish
and the order in which these tasks should be done.

> type Task m = m Result
> type Schedule m = [Task m]
> data Result = TaskSuccess | TaskFailure

We can now define a few utility functions:

> mkDepGraph :: Ord a => [Rule m a] -> DepGraph m a
> mkDepGraph rs =
>     let es = nub $ concatMap edges rs
>         vs = nub $ concatMap vertices rs
>     in G.gmap mkLabel (G.run_ G.empty (G.insMapNodesM vs >> G.insMapEdgesM es))
>     where edges r@(Rule{target=t,prereqs=ps}) = zipWith3 (,,) (repeat t) ps ps
>           vertices (Rule{target=t,prereqs=ps}) = t : ps
>           labels = Map.fromList $
>                    map (\r -> (target r, Label r undefined)) rs
>           mkLabel (preds, n, t, sucs) = (preds, n, labels Map.! t, sucs)

Return the list of dependencies wrt whom target is out of date.

> outOfDate :: Monad m => Cmp m a -> a -> [a] -> m [a]
> outOfDate cmp target deps = filterM (cmp target) deps
>
> tagContext :: Monad m => Context m a -> m (Context m a)
> tagContext c@(preds, n, l@(Label rule _), sucs) = do
>     let deps = map fst sucs
>     ood <- outOfDate (isOOD rule) (target rule) (target rule:deps)
>     return (preds, n, l{lbl_tag=ood}, sucs)

Simplify the graph by dismissing all nodes that are not reachable in
the dependency graph starting from the given targets. Reduce it
further by pruning those nodes that are up to date wrt their
dependencies.

> shrink :: Ord a => [a] -> DepGraph m a -> DepGraph m a
> shrink ts g = G.delNodes (G.nodes (foldr f g ns)) g
>     where ns = map (fst . G.mkNode_ m) ts
>           m = G.fromGraph (G.nmap (target . lbl_rule) g)
>           f n g = case G.match n g of
>                     (Just c, g') -> foldr f g' (G.suc' c)
>                     (Nothing, g') -> g'
>
> prune :: DepGraph m a -> DepGraph m a
> prune g = G.delNodes (G.ufold f [] g) g
>     where f c xs = case (lbl_tag . G.lab') c of
>                      [] -> G.node' c : xs
>                      _ -> xs

Given a dependency graph take the longest path to each out of date
dependency and execute recipes in reverse order. Schedule recipes for
execution at most once. This can be done with a simple topological
sort because at this stage the graph now contains exactly those nodes
that need to be built.

> schedule :: DepGraph m a -> Schedule m
> schedule = map (recipe . lbl_rule) . G.topsort' . G.grev

Now to make a set of targets given a set of rules, we build a
dependency graph for each target. We take the transitive closure of
each graph and prune away those nodes that are up to date. From this a
schedule of tasks can be derived.

> mk :: (Monad m, Ord a) => [Rule m a] -> [a] -> m (Schedule m)
> mk rs ts =
>     return rs
>     >>= (**) mkDepGraph
>     >>= (**) trc'
>     >>= guard G.isSimple errCyclic
>     >>= (**) (shrink ts)
>     >>= gmapM tagContext
>     >>= (**) prune
>     >>= guard (not . isAmbiguous) errAmb
>     >>= (**) schedule
>   where (**) = (.) return
>         guard p s x = if p x then return x else fail s
>         errCyclic = "There are cycles in the dependency graph."
>         errAmb = "There is more than one way to build the targets."
>         isAmbiguous _ = False -- xxx

> gmapM :: (G.DynGraph gr, Monad m) =>
>          (G.Context a b -> m (G.Context c d)) -> gr a b -> m (gr c d)
> gmapM f = G.ufold (\c g -> return (G.&) `ap` (f c) `ap` g) (return G.empty)

Wrapper function to remove loops that were induced by transitive
closure calculation and reestablish the invariant that edge labels
name the target they point to.

> trc' :: DepGraph m a -> DepGraph m a
> trc' g = G.gmap f $ G.efilter (not . isLoop) $ G.trc g
>     where isLoop (a, b, _) | a == b = True
>                            | otherwise = False
>           f (preds, n, l, sucs) = (map p preds, n, l, map s sucs)
>               where s (_, x) = let l' = target $ lbl_rule $ fromJust $ G.lab g x in (l', x)
>                     p (_, x) = let l' = target $ lbl_rule l in (l', x)
