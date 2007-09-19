
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

> module Hmk where
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
Tdestination for convenience.

> type DepGraph m a = G.Gr (Label m a) a
> type Context m a = G.Context (Label m a) a

where labels are represented as follows. We use tags to mark out of
date nodes.

> data Label m a = Label { lTarget :: a
>                      , lRecipe :: Task m
>                      , lOOD :: Cmp m a
>                      , lTag :: Tag }
> data Tag = Make | Ignore
>
> instance Show a => Show (Label m a) where
>     show (Label t _ _ _) = show t

The dependency graph is used to determine a set of tasks to accomplish
and the order in which these tasks should be done.

> type Task m = m ()
> type Schedule m = [Task m]

We can now define a few utility functions:

> mkDepGraph :: Ord a => [Rule m a] -> DepGraph m a
> mkDepGraph rs =
>     let es = nub $ concatMap edges rs
>         vs = nub $ concatMap vertices rs
>     in G.gmap mkLabel (G.run_ G.empty (G.insMapNodesM vs >> G.insMapEdgesM es))
>     where edges r@(Rule{target=t,prereqs=ps}) = zipWith3 (,,) (repeat t) ps ps
>           vertices (Rule{target=t,prereqs=ps}) = t : ps
>           labels = Map.fromList $
>                    map (\(Rule t _ r cmp) -> (t, Label t r cmp undefined)) rs
>           mkLabel (preds, n, t, sucs) = (preds, n, labels Map.! t, sucs)

A mapping between targets and node identifiers in the graph.

> buildNodeMap :: Ord a => DepGraph m a -> Map.Map a G.Node
> buildNodeMap =
>     Map.fromList . inverse . map (\(n,l) -> (n, lTarget l)) . G.labNodes
>     where inverse = map (\(a,b) -> (b,a))
>
> t2n :: Ord a => Map.Map a G.Node -> a -> G.Node
> t2n m t = m Map.! t

Return the list of dependencies wrt whom target is out of date.

> outOfDate :: Monad m => Cmp m a -> a -> [a] -> m [a]
> outOfDate cmp target deps = filterM (cmp target) deps
>
> tagContext :: Monad m => Context m a -> m (Context m a)
> tagContext c@(preds, n, l@(Label target _ cmp _), sucs) = do
>     let deps = map fst sucs
>     ood <- outOfDate cmp target (target:deps)
>     let l' = if null ood then l{lTag=Ignore} else l{lTag=Make}
>     return (preds, n, l', sucs)

Simplify the graph by dismissing all nodes that are not reachable in
the dependency graph starting from the given targets. Reduce it
further by pruning those nodes that are up to date wrt their
dependencies.

> shrink :: Ord a => [a] -> DepGraph m a -> DepGraph m a
> shrink ts g = G.delNodes (G.nodes (foldr f g ns)) g
>     where ns = map (m Map.!) ts
>           m = buildNodeMap g
>           f n g = case G.match n g of
>                     (Just c, g') -> foldr f g' (G.suc' c)
>                     (Nothing, g') -> g'
>
> prune :: DepGraph m a -> DepGraph m a
> prune g = G.delNodes (G.ufold f [] g) g
>     where f c xs = case (lTag . G.lab') c of
>                      Ignore -> G.node' c : xs
>                      Make -> xs

Given a dependency graph take the longest path to each out of date
dependency and execute recipes in reverse order. Schedule recipes for
execution at most once. This can be done with a simple topological sort because
at this stage the graph now contains exactly those nodes that need to be built.

> schedule :: DepGraph m a -> Schedule m
> schedule = map lRecipe . G.topsort' . G.grev

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
>         errAmb = "There is more than one way to build the targets."
>         errCyclic = "There are cycles in the dependency graph."
>         isAmbiguous _ = False

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
>               where s (_, x) = let l' = lTarget (fromJust (G.lab g x)) in (l', x)
>                     p (_, x) = let l' = lTarget l in (l', x)
