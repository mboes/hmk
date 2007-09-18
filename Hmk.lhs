
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
> import Data.List (nub, maximumBy)
> import Data.Maybe (fromJust)
> import qualified Data.Map as Map


Hmk manages dependencies between entities. These dependencies are
specified by means of rules, establishing a dependency between the
target and prerequesesites. Rules also package a means of comparing
targets and prerequisites to determine whether the target is out of
date.

> type Cmp m a = a -> a -> m Bool
> data Rule m a = Rule { target :: a
>                      , prereqs :: [a]
>                      , recipe :: Task m
>                      , compareDate :: Cmp m a }

The rules induce a dependency graph:

> type DepGraph a = G.Gr a ()

The dependency graph is used to determine a set of tasks to accomplish
and the order in which these tasks should be done.

> type Task m = m ()
> type Schedule m = [Task m]

We can now define a few utility functions:

> mkDepGraph :: Ord a => [Rule m a] -> DepGraph a
> mkDepGraph rs =
>     let es = nub $ concatMap edges rs
>         vs = nub $ concatMap vertices rs
>     in G.run_ G.empty (G.insMapNodesM vs >> insMapUEdgesM es)
>     where edges (Rule{target=t,prereqs=ps}) = map ((,) t) ps
>           vertices (Rule{target=t,prereqs=ps}) = t : ps
>           insMapUEdgesM = G.insMapEdgesM . map (\(a,b) -> (a,b,()))

A mapping between targets and node identifiers in the graph.

> buildNodeMap :: Ord a => DepGraph a -> Map.Map a G.Node
> buildNodeMap = Map.fromList . inverse . G.labNodes
>     where inverse = map (\(a,b) -> (b,a))
>
> t2n :: Ord a => Map.Map a G.Node -> a -> G.Node
> t2n m t = m Map.! t

Return the list of out of date dependencies.

> outOfDate :: Monad m => Cmp m a -> a -> [a] -> m [a]
> outOfDate cmp target deps = filterM (cmp target) deps

Simplify the graph by pruning away all nodes that are not reachable in
the dependency graph starting from the given targets.

> prune :: Ord a => [a] -> DepGraph a -> DepGraph a
> prune ts g = G.buildGr $ G.dfsWith id vs g
>     where vs = map (t2n (buildNodeMap g)) ts

Given a dependency graph take the longest path to each out of date
dependency and execute recipes in reverse order. Schedule recipes for
execution at most once.

> schedule :: Ord a => Map.Map a (Task m) -> DepGraph a -> [(a, [a])] -> Schedule m
> schedule m g xs = map (m Map.!) . G.topsort' . G.grev $ g

Now to make a set of targets given a set of rules, we build a
dependency graph for each target. We take the transitive closure of
each graph and prune away those nodes that are up to date. From this a
schedule of tasks can be derived.

> cmp _ _ = return True

> mk :: (Monad m, Ord a) => [Rule m a] -> [a] -> m (Schedule m)
> mk rs ts = do
>     let taskMap = Map.fromList . map (\Rule{target=t,recipe=r} -> (t,r)) $ rs
>     g <- guard unambiguous errAmb
> --         . liftM (prune ts)
>          . guard G.isSimple errCyclic
>          . return . G.efilter (not . isLoop) . G.trc . mkDepGraph $ rs
>     let dss = map (map (fromJust . G.lab g) . G.suc g) . map (t2n (buildNodeMap g)) $ ts
>     xs <- liftM (zip ts) $ zipWithM (outOfDate cmp) ts dss
>     return (schedule taskMap g xs)
>   where guard p s mx = liftM p mx >>= \cond -> if cond then mx else fail s
>         errAmb = "There is more than one way to build the targets."
>         errCyclic = "There are cycles in the dependency graph."
>         unambiguous _ = True
>         isLoop (a, b, _) | a == b = True
>                          | otherwise = False

Below is an implementation of longest path for a DAG.

> elpdfs :: G.Graph gr => G.Node -> G.Node -> gr a b -> (Int, G.Path)
> elpdfs source sink _ | source == sink = (1, [sink])
> elpdfs source sink g | (Just c, g') <- G.match source g =
>     if null (G.suc' c) then (0, []) else
>         case maximumBy (\ x y -> fst x `compare` fst y)
>                  (map (\v -> elpdfs v sink g') (G.suc' c)) of
>           (n, vs) | length vs > 0 -> (n+1, source : vs)
>           _ -> (0, [])
> elpdfs _ _ _ | otherwise = error "No such source in graph."
>
> elp :: G.Graph gr => gr a b -> G.Node -> G.Node -> G.Path
> elp g source sink = snd $ elpdfs source sink g
>
> relp g source sink = reverse $ elp g source sink
