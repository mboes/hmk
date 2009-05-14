
2009 Apr 15

The author disclaims copyright to this source. In place of a legal
notice, here is a blessing:

   May you do good and not evil.
   May you find forgiveness for yourself and forgive others.
   May you share freely, never taking more than you give.

(after the sqlite source code)

--

Rule coalescing, rule completion and ambiguity checking.

> module Control.Hmk.Analyze (coalesce, complete, process) where
> import {-# SOURCE #-} Control.Hmk
> import Data.List (sortBy)
> import Data.Maybe (isNothing)
> import qualified Data.Set as Set

From the mk(1) man page:

"A later rule may modify or override an existing rule under the
following conditions:

 - If the targets of the rules exactly match and one rule contains
only a prerequisite clause and no recipe, the clause is added to the
prerequisites of the other rule. If either or both targets are
virtual, the recipe is always executed.

 - If the targets of the rules match exactly and the prerequisites do
not match and both rules contain recipes, mk reports an ``ambiguous
recipe'' error.

 - If the target and prerequisites of both rules match exactly, the
second rule overrides the first."

> coalesce :: Ord a => [Rule m a] -> [Rule m a]
> coalesce = aux . sortBy cmp
>     where aux (r:rs@(r':_))
>               | target r == target r', isNothing (recipe r) =
>                   r'{prereqs = prereqs r' ++ prereqs r} : aux rs
>               | target r == target r', prereqs r == prereqs r' =
>                   aux rs
>               | target r == target r' =
>                   error "Ambiguous rules."
>               | otherwise = r : aux rs
>           aux rs = rs
>           cmp x y = case compare (target x) (target y) of
>                       EQ -> maybe LT (const GT) (recipe x)
>                       x -> x

For simplicity, we assume as invariant that for all prerequisites p in
every rule there exists a rule whose target is p. 'complete' adds new
rules if necessary to achieve this invariant.

> complete :: (Ord a, Show a) => Cmp m a -> [Rule m a] -> [Rule m a]
> complete cmp rules = let targets = Set.fromList (map target rules)
>                          prereqss = Set.unions $
>                                     map (Set.fromList . prereqs) rules
>                          mkrule p rules | Set.member p targets = rules
>                                         | otherwise =
>                                             Rule p [] (err p) cmp : rules
>                      in Set.fold mkrule [] prereqss ++ rules
>     where err p = error $ "Don't know how to build " ++ show p ++ "."

Putting everything together:

> process :: (Ord a, Show a) => Cmp m a -> [Rule m a] -> [Rule m a]
> process cmp = complete cmp . coalesce
