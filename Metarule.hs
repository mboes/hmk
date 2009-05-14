-- Copyright (C) 2009 Mathieu Boespflug <mboes@tweag.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Metarule where

import Hmk (Rule(..))
import System.FilePath
import System.Directory
import Data.List (break, partition, isPrefixOf, isSuffixOf)
import Control.Monad (liftM)


type FilePattern = String

-- | Recursively enumerate all files and directories in given directory.
find :: FilePath -> IO [FilePath]
find d = do
  ls <- liftM (map (d</>) . filter (\x -> x /= "." && x /= "..")) $
        getDirectoryContents d
  (dirs, files) <- partitionM doesDirectoryExist ls
  rest <- mapM find dirs
  return $ dirs ++ files ++ concat rest
    where partitionM _ [] = return ([],[])
          partitionM f (x:xs) = do
            cond <- f x
            (ys, zs) <- partitionM f xs
            if cond then return (x:ys,zs) else return (ys, x:zs)

instantiateStem :: FilePattern -> IO [String]
instantiateStem s = do
  let (x, _:y) = break (=='%') s
  cd <- getCurrentDirectory
  ls <- makeRelative cd $ find cd
  let xs = filter (\z -> x `isPrefixOf` z && y `isSuffixOf` z) ls
      nx = length x
      ny = length y
  return $ map (\x -> take (length x - ny) $ drop nx x) xs
