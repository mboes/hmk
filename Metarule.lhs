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

> module Metarule (instantiate, instantiateRecurse) where
>
> import Control.Hmk
>
> import Data.Sequence (Seq)
> import qualified Data.Sequence as Seq
> import qualified Data.Foldable as Seq
> import qualified Data.Set as Set
> import Control.Applicative
> import Control.Monad.State
>
> import Text.Regex.PCRE.Light.Char8

Remove any uninstantiated meta-rule.

> cleanup :: Seq (Rule a FilePath) -> Seq (Rule a FilePath)
> cleanup = Seq.foldr f Seq.empty where
>     f r rs = case target r of
>                '%':_ -> rs
>                _ -> r Seq.<| rs

Instantiation of meta-rules. 'instantiate' is a helper function for
'instantiateRecurse', which instantiates meta-rules based on current targets
and then recursively instantiates meta-rules with the prerequesites of the
matching rules.

> type Stem = String
>
> seqCatMaybes :: Seq (Maybe a) -> Seq a
> seqCatMaybes = Seq.foldr (\x xs -> maybe xs (Seq.<| xs) x) Seq.empty
>
> instantiate :: Seq FilePath   -- ^ Targets.
>             -> Seq (Stem -> Rule a FilePath)
>             -> Seq (Rule a FilePath)
> instantiate targets closures = join $ fmap f closures where
>     f clo = let schema = target (clo undefined)
>                 stems = collectMatches schema targets
>             in fmap (\stem -> expand stem (clo stem)) stems
>     collectMatches ('%':suffix) ts =
>         let re = compile ("(.*)" ++ suffix ++ "$") [anchored, dollar_endonly]
>         -- The prefix is in the captured sub-pattern at index 1.
>         in seqCatMaybes (fmap (\t -> fmap (!! 1) (match re t [])) ts)
>     collectMatches s ts = Seq.empty
>     -- Substitute the stem for the percent characters in targets and
>     -- prerequesites.
>     expand stem r@Rule{target,prereqs} = r { target = subst stem target
>                                            , prereqs = map (subst stem) prereqs }
>     subst stem ('%':suffix) = stem ++ suffix
>     subst stem x = x
>
> instantiateRecurse :: Seq FilePath
>                    -> Seq (Stem -> Rule a FilePath)
>                    -> Seq (Rule a FilePath)
> instantiateRecurse targets closures =
>     let new = evalState (go targets) Set.empty
>     in cleanup (fmap ($ "") closures) Seq.>< new
>     where go targets | Seq.null targets = return Seq.empty
>                      | otherwise = do
>             seen <- get
>             let rules = instantiate targets closures
>                 ts = (Set.\\ seen) $ Set.unions $ Seq.toList $
>                      fmap (Set.fromList . prereqs) rules
>             put (seen `Set.union` ts)
>             (rules Seq.><) <$> go (Seq.fromList (Set.toList ts))
