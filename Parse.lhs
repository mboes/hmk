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

> module Parse ( Token(..), Mkfile(..), AssignAttr(..), parse ) where

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
Tokens can be composed, as in 'a${b}c', a token made of 2 literals and 1
reference.

> data Token = Lit String
>            | Coll (Seq Token)
>            | Ref Token
>              deriving Show
>
> data AssignAttr = Export | Local
>                   deriving Show
>
> data Mkfile = Mkrule (Seq Token) (Maybe Char) (Seq Token) (Maybe String) (Maybe String) Mkfile
>             | Mkassign AssignAttr String (Seq Token) Mkfile
>             | Mkinsert Token Mkfile -- lines beginning with '<'
>             | Mkinpipe Token Mkfile -- lines beginning with '<|'
>             | Mkeof

Parsing produces unevaluated rules, represented by the PRule type. Evaluation
takes values of this type as input to produce values of type Rule used in
Control.Hmk. The parser also accumulates assignments by side-effect, that will
be executed during evaluation.

> parse :: FilePath -> String -> Mkfile
> parse fp input =
>     case runParser p_toplevel () fp input of
>         Left e -> error (show e)
>         Right x -> x

Contrary to Plan9's mk, all ':' characters in prerequesites as well as in
targets must be escaped. This is to simplify the implementation slightly.

> token = reference
>         <|> substitution
>         <|> collation
>
> reference = Ref <$> do
>   char '$'
>   -- $name or ${name} or ${name:A%B=C%D}
>   name <|> bname
>     -- banned characters from variable references according to rc(1) manual.
>     where name = Lit <$> many1 (noneOf " \t\n#;&|^$=`'{}()<>:")
>           bname = between (char '{') (char '}') collation
>
> substitution = between (char '{') (char '}') $ do
>                      a <- quotableTill "%"; char '%'
>                      b <- quotableTill "="; char '='
>                      c <- quotableTill "%"; char '%'
>                      d <- quotableTill "}"
>                      return (error "Unimplemented.") -- xxx

A literal or a collation of tokens.

> collation = do
>   x <- many1 (literal <|> reference)
>   case x of
>     [l] -> return l
>     toks -> return $ Coll (Seq.fromList toks)
>
> literal = Lit <$> quotableTill " \t\n:$"

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
>   p_assignment <|> p_rule <|> p_insert <|> p_inpipe <|> (Mkeof <$ eof)

" Assignments and rules are distinguished by the first unquoted occurrence of
: (rule) or = (assignment)."

> p_assignment = try $ do
>   var <- quotableTill " \t="
>   char '='
>   attr <- option Export p_assignment_attr
>   value <- sepBy token whitespace
>   newline
>   Mkassign attr var (Seq.fromList value) <$> p_toplevel
>
> p_assignment_attr = try $ do
>   c <- anyChar
>   char '='
>   case c of
>     'U' -> return Local
>     _ -> error "Unknown attribute."
>
> p_rule = do
>   targets <- Seq.fromList <$> many1 token
>   whitespace
>   char ':'
>   flag <- option Nothing p_rule_flag
>   whitespace
>   prereqs <- Seq.fromList <$> sepBy token whitespace
>   newline
>   recipe <- p_recipe
>   Mkrule targets flag prereqs recipe Nothing <$> p_toplevel
>
> p_rule_flag = try $ do
>   c <- anyChar
>   cmd <- if c == 'P' then quotableTill ":" else return ""
>   char ':'
>   return $ Just c
>
> p_recipe = do
>   lines <- collect
>   if null lines then return Nothing else return $ Just $ intercalate "\n" lines
>     where collect = (do indentation; (:) <$> (many (noneOf "\n") <* newline) <*> collect)
>                     <|> return []
>
> p_insert = char '*' >> return undefined
>
> p_inpipe = char '*' >> return undefined
