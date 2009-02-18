> module Parse (preprocess, parse, postprocess) where

Parse mkfile's to a set of rules. Meta-rules are expanded and variable
references are substituted for their values, using the environment. The
following quoted comments in this source file are all excerpts from the man
page for plan9's mk command, so are copyright Lucent Technologies.

The environment is a variable store that can be updated in place. It is very
much not a persistent abstraction, so we implement it with an impure
structure. For simplicity that structure is the system environment, since
we'll need to communicate the environment to child processes using the system
environment anyways.

> import Control.Hmk
>
> import Text.Parsec hiding (parse)
> import qualified Text.ParserCombinators.Parsec as P
> import Data.ByteString.Lazy.Char8 (ByteString)
> import qualified Data.ByteString.Lazy.Char8 as B
> import Data.List (intercalate)
> import Control.Monad
> import Control.Monad.Trans
>
> import System.FilePath (FilePath)
> import System.Posix.Env
> import System.Cmd (system)
> import System.Exit

Write parser as a transformation over the IO monad because we manipulate the
system environment directly during assignments / variable expansions.

> type P a = ParsecT ByteString () IO a

"A mkfile consists of assignments (described under `Environment') and rules. A
rule contains targets and a tail. A target is a literal string and is normally
a file name. The tail contains zero or more prerequisites and an optional
recipe, which is an rc script. Each line of the recipe must begin with white
space."

But before parsing rules and assignments we preprocess the input to unfold all
folded lines. That is,  recognizing "\\\n" as a blank.

> preprocess :: ByteString -> ByteString
> preprocess = id -- xxx

Now to parsing.

> parse :: FilePath -> ByteString -> IO [Rule IO FilePath]
> parse name str = do
>   let p = sepBy (many p_assignment >> p_rule) (many1 newline)
>   result <- runParserT p () name str
>   case result of
>       Left err -> error (show err)
>       Right rules -> return rules

During parsing we might need to expand variables first. If we encounter a
variable, we expand it, prepend the expansion to the parse stream, then
continue parsing from there. 'expand' attempts parsing a variable reference.
On success, the reference is expanded, returning nothing, otherwise it fails.
Hence 'expand' is only interesting for its side-effect on the parse stream.

> expand :: P ()
> expand = do
>   char '$'
>   optional expand
>   -- $name or ${name:A%B=C%D} or ${name:A&B=C&D}
>   name <|> braces
>     -- banned characters from variable references from rc(1) manual.
>     where name = do
>             var <- many1 (noneOf " \t\n#;&|^$=`'{}()<>")
>             mbval <- liftIO $ getEnv var
>             case mbval of
>               Just val -> do stream <- getInput
>                              liftIO $ putStrLn "expand"
>                              liftIO $ B.putStrLn (B.append (B.pack val) stream)
>                              setInput (B.append (B.pack val) stream)
>               Nothing -> return ()
>           braces = between (char '{') (char '}') $ do
>                      a <- quotableTill (char '%'); char '%'
>                      b <- quotableTill (char '='); char '='
>                      c <- quotableTill (char '%'); char '%'
>                      d <- quotableTill (char '}')
>                      return (error "Unimplemented.") -- xxx

"Special characters may be quoted using single quotes '' as in rc(1)."

When parsing strings, we must allow for escaping of special characters using
quotes. 'quotableTill' munches characters until a character in the terminal
set is reached, dealing with quotes as appropriate. In a quoted string a quote
is written as a pair ''.

> quotableTill :: P Char -> P String
> quotableTill terminals = any where
>     any = do stream <- getInput
>              liftIO $ putStrLn "quotableTill"
>              liftIO $ B.putStrLn stream
>              (expand >> any) <|> quoted <|> unquoted
>     end = (lookAhead terminals >> return []) <|> (eof >> return [])
>     unquoted = return (:) `ap` anyChar `ap` (end <|> any)
>     quoted = do
>       char '\''
>       xs <- manyTill ((string "''" >> return '\'') <|> anyChar)
>                      (try (char '\'' >> notFollowedBy (char '\'')))
>       return (xs ++) `ap` (end <|> any)

Targets and prerequesites are usually filenames. Let's call them entities.

> p_entity :: P FilePath
> p_entity = do
>   many (oneOf " \t")
>   s <- quotableTill (oneOf " \t\n:")
>   many (oneOf " \t")
>   return s

" Assignments and rules are distinguished by the first unquoted occurrence of
: (rule) or = (assignment)."

Assignments are parsed solely their side-effects, hence the unit return type.
The right-hand side of an assignment is dumped verbatim into the environment.
Parsing of the quotes and so forth will be done at expansion time.

> p_assignment :: P ()
> p_assignment = try $ do
>   var <- p_entity
>   char '='
>   value <- manyTill anyChar newline
>   liftIO $ setEnv var value True
>
> p_rule :: P (Rule IO FilePath)
> p_rule = do
>   target <- p_entity
>   char ':'
>   prereqs <- many p_entity
>   newline
>   recipe <- p_recipe
>   return $ Rule target prereqs (\_ -> recipe) defaultCmp
>
> defaultCmp _ _ = return True -- xxx
>
> p_recipe :: P (Task IO)
> p_recipe = do
>   cs <- many $ do
>              many1 (oneOf " \t")
>              cmd <- manyTill anyChar newline
>              newline
>              return cmd
>   return $ wrap $ system $ intercalate "\n" cs
>     where wrap m = m >>= \c -> case c of
>                                  ExitSuccess -> return TaskSuccess
>                                  ExitFailure _ -> return TaskFailure

"In mkfile's a later rule may modify or override an existing rule under
certain conditions. So we postprocess the list of rules to coalesce rules for
matching targets. The plan9 mk man page says:

– If the targets of the rules exactly match and one rule contains only a
prerequisite clause and no recipe, the clause is added to the prerequisites of
the other rule. If either or both targets are virtual, the recipe is always
executed.

– If the targets of the rules match exactly and the prerequisites do not match
and both rules contain recipes, mk reports an ``ambiguous recipe'' error.

– If the target and prerequisites of both rules match exactly, the second rule
overrides the first."

> postprocess :: [Rule IO FilePath] -> [Rule IO FilePath]
> postprocess = id -- xxx
