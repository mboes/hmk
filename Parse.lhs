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

> module Parse ( PRule(..), Assignment(..), Token(..), Mkfile
>               , parse ) where

Parse mkfile's to a set of rules. The following quoted comments in this source
file are all excerpts from the man page for plan9's mk command, so are
copyright Lucent Technologies.

> import Text.Parsec hiding (parse, token)
> import Data.List (intercalate)
> import Data.Sequence (Seq)
> import qualified Data.Sequence as Seq
> import Control.Applicative hiding ((<|>), many)
> import System.FilePath (FilePath)


"A mkfile consists of assignments (described under `Environment') and rules. A
rule contains targets and a tail. A target is a literal string and is normally
a file name. The tail contains zero or more prerequisites and an optional
recipe, which is an rc script. Each line of the recipe must begin with white
space."

An mkfile is carved out into lines, each of which is separated into tokens,
which are either a literal or a reference. Reference names may themselves
contain references and literals, so a reference name is a sequence of tokens.

> data Token = Lit String
>            | Ref (Seq Token)
>              deriving Show
>
> data PRule = PRule (Seq Token) (Seq Token) (Maybe String) (Maybe String)
>              deriving Show
>
> data AssignAttr = Export | Local
>                   deriving Show
> data Assignment = Assign AssignAttr String (Seq Token)
>                   deriving Show
>
> type Mkfile = (Seq PRule, Seq Assignment)

Parsing produces unevaluated rules, represented by the PRule type. Evaluation
takes values of this type as input to produce values of type Rule used in
Control.Hmk. The parser also accumulates assignments by side-effect, that will
be executed during evaluation.

> parse :: FilePath -> String -> Mkfile
> parse fp input =
>     case runParser ((,) <$> p_toplevel <*> getState) Seq.empty fp input of
>         Left e -> error (show e)
>         Right x -> x

Contrary to Plan9's mk, all ':' characters in prerequesites as well as in
targets must be escaped. This is to simplify the implementation slightly.

> token = (Ref . Seq.fromList <$> reference)
>         <|> substitution
>         <|> (Lit <$> literal)
>
> reference = do
>   char '$'
>   -- $name or ${name} or ${name:A%B=C%D}
>   name <|> bname
>     -- banned characters from variable references according to rc(1) manual.
>     where name = (:[]) . Lit <$> many1 (noneOf " \t\n#;&|^$=`'{}()<>:")
>           bname = between (char '{') (char '}') (many1 token)
>
> substitution = between (char '{') (char '}') $ do
>                      a <- quotableTill "%"; char '%'
>                      b <- quotableTill "="; char '='
>                      c <- quotableTill "%"; char '%'
>                      d <- quotableTill "}"
>                      return (error "Unimplemented.") -- xxx
>
> literal = quotableTill " \t\n:"


"Special characters may be quoted using single quotes '' as in rc(1)."

When parsing strings, we must allow for escaping of special characters using
quotes. 'quotableTill' munches characters until a character in the terminal
set is reached, dealing with quotes as appropriate. In a quoted string a quote
is written as a pair ''.

> quotableTill terminals = quoted <|> unquoted where
>     end = return []
>     unquoted = ((:) <$> noneOf terminals <*> (quoted <|> unquoted <|> end))
>     quoted = do
>       char '\''
>       xs <- manyTill ((string "''" >> return '\'') <|> anyChar)
>                      (try (char '\'' >> notFollowedBy (char '\'')))
>       (++) <$> pure xs <*> (quoted <|> unquoted <|> end)

Munch all whitespace on a line.

> whitespace = skipMany (oneOf " \t")
> indentation = skipMany1 (oneOf " \t")

> p_toplevel = do
>   many newline
>   many p_assignment
>   ((Seq.<|) <$> p_rule <*> p_toplevel) <|> (eof >> return Seq.empty)

" Assignments and rules are distinguished by the first unquoted occurrence of
: (rule) or = (assignment)."

The assignment parser is a side-effecting parser that accumulates assignments
into the parser state. It returns unit.

> p_assignment = try $ do
>   var <- quotableTill " \t="
>   char '='
>   value <- sepBy token whitespace
>   newline
>   assigns <- getState
>   putState $ assigns Seq.|> Assign Export var (Seq.fromList value)
>
> p_rule = do
>   targets <- Seq.fromList <$> many1 token
>   whitespace
>   char ':'
>   whitespace
>   prereqs <- Seq.fromList <$> sepBy token whitespace
>   newline
>   recipe <- p_recipe
>   return $ PRule targets prereqs recipe Nothing
>
> p_recipe = do
>   lines <- collect
>   if null lines then return Nothing else return $ Just $ intercalate "\n" lines
>     where collect = (do indentation; (:) <$> (many (noneOf "\n") <* newline) <*> collect)
>                     <|> return []
