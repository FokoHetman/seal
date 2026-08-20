{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}
module Subcommands where

import Control.Applicative
import Control.Lens
import Data.List (stripPrefix, isPrefixOf, intercalate)
import Data.Data (Typeable)
import Data.Kind (Type)
import Data.Default (Default(def))
import System.Exit (exitFailure)
import Debug.Trace (traceIO)
import Globals (globalName)
data CliParseError = Undefined | NoMatch String String | MissingValue String

instance Show CliParseError where
  show Undefined = "undefined behavior"
  show (NoMatch expected found) = "expected `" <> expected <> "` found `" <> found <> "`"
  show (MissingValue v) = "missing value: " <> v


raiseError :: forall a . String -> CliParseError -> IO a
raiseError preludium err = do
  traceIO $ "[" <> preludium <> "] " <> show err
  exitFailure

newtype CliParser a = CliParser {runCliParser :: [String] -> Either CliParseError ([String], a)}

instance Monad CliParser where
  (CliParser p1) >>= f = CliParser $ \input -> do
    (input', a) <- p1 input
    runCliParser (f a) input'
    

instance Functor CliParser where
  fmap f (CliParser p) = CliParser $ \input -> do
    (input', x) <- p input
    pure (input', f x)

instance Applicative CliParser where
  pure x = CliParser $ \input -> pure (input, x)
  (CliParser p1) <*> (CliParser p2) =
    CliParser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Right (input'', f a)

instance Alternative (Either CliParseError) where
  empty = Left Undefined
  (Right r1) <|> _ = pure r1
  _ <|> (Right r2) = pure r2
  _ <|> _ = empty

instance Alternative CliParser where
  empty = CliParser $ \_ -> Left Undefined
  (CliParser p1) <|> (CliParser p2) = CliParser $ \input -> p1 input <|> p2 input

class CliParse a where
  cliParser :: CliParser a


data FlagBuilder' a t = MultipleValues String (Lens' a [t]) (String -> t)
                      | SingleValue String (Lens' a (Maybe t)) (String -> t)
                      | ExistentialValue (Lens' a Bool)


flagToParser :: ([String], String, FlagBuilder a) -> CliParser (a -> a)
flagToParser (names', _, FlagBuilder a) = case a of
  MultipleValues _ getter f -> parseMultiFlag names' getter f
  SingleValue _ getter f -> alternatives $ variations names' (`parseArgValueFlag` (set getter . Just . f))
  ExistentialValue getter -> alternatives $ variations names' (`parseArgFlag` (set getter))

data FlagBuilder a where
  FlagBuilder :: FlagBuilder' a t -> FlagBuilder a

data ArgBuilder' a t = ArgBuilder' (Lens' a (Maybe t)) (String -> Maybe t)

data ArgBuilder a where
  ArgBuilder :: ArgBuilder' a t -> ArgBuilder a

argToParser :: (String, String, ArgBuilder a) -> CliParser (a -> a)
argToParser (name, _, ArgBuilder (ArgBuilder' getter f)) = set getter . f <$> takeP name


type Flag a = ([String], String, FlagBuilder a)

showFlagSmall :: Flag a -> String
showFlagSmall (n:_, _, (FlagBuilder a)) = case a of
  MultipleValues s _ _ -> "[ " <> n <> " " <> s <> " ]"
  SingleValue s _ _ -> n <> " " <> s
  ExistentialValue _ -> n
showFlagSmall _ = undefined

showFlagBig :: Flag a -> (String, String)
showFlagBig (names', description, (FlagBuilder a)) = (,description) $ case a of
  MultipleValues s _ _  -> intercalate ", " (fmap (\n -> "[ " <> n <> " " <> s <> " ]") names')
  SingleValue s _ _     -> intercalate ", " (fmap (\n -> n <> " " <> s) names')
  ExistentialValue _    -> intercalate ", " names'

class (CliParse a, Default a, Typeable a) => Subcommand' a where
  names :: [String]
  flags :: [Flag a]
  args  :: [(String, String, ArgBuilder a)]
  run   :: a -> IO ()
  help  :: String

data Subcommand where
  Subcommand :: Subcommand' a => a -> Subcommand

data SubcommandW where
  SubcommandW :: Subcommand' a => SubcommandW

cliParserW :: SubcommandW -> CliParser Subcommand
cliParserW (SubcommandW @a) = Subcommand <$> cliParser @a

subcommandHelp :: forall a. Subcommand' a => String
subcommandHelp = concat
    [ globalName, " {", intercalate "," (names @a), "} ", intercalate " " $ fmap (\(x,_,_) -> x) $ args @a
    , "\n\nargs:\n"
    , displayArgs @a
    , "\nflags:\n"
    , displayFlags @a
    ]

subcommandParser :: forall a. Subcommand' a => CliParser a
subcommandParser = (foldr (<|>) empty $ fmap argP $ names @a)
            *> (($ def) <$> foldr (.) id <$> many (foldr (<|>) empty $ (fmap argToParser $ args @a)<>(fmap flagToParser $ flags @a)))

displayArgs :: forall a. Subcommand' a => String
displayArgs = intercalate "\n" res
  where
    darg = fmap (\(x,y,_) -> (x,y)) $ args @a
    maxlen = 2 + maximum (fmap (length . fst) $ darg)
    res = fmap (\(a,b) -> "\t" <> a <> [' ' | _<-[1..maxlen - length a]] <> b) darg

displayFlags :: forall a. Subcommand' a => String
displayFlags = intercalate "\n" res
  where
    dflag = fmap showFlagBig $ flags @a
    maxlen = 2 + maximum (fmap (length . fst) $ dflag)
    res = fmap (\(a,b) -> "\t" <> a <> [' ' | _<-[1..maxlen - length a]] <> b) dflag

takeP :: String -> CliParser String
takeP name = CliParser $ \case
  (('-':xs):ys) -> Left $ NoMatch name $ '-':xs
  (x:xs) -> Right (xs,x)
  _ -> Left $ MissingValue name

argP :: String -> CliParser String
argP s = CliParser $ \case
  (x:xs) | x==s -> Right (xs,x)
  (x:_) -> Left $ NoMatch s x
  _ -> Left $ NoMatch s ""


alternatives :: [CliParser (a -> a)] -> CliParser (a -> a)
alternatives [] = undefined
alternatives [x] = x
alternatives (x:xs) = x <|> alternatives xs

variations :: [a] -> (a -> b) -> [b]
variations [] _ = []
variations (x:xs) f = f x: variations xs f


parseArgFlag :: String -> (Bool -> a -> a) -> CliParser (a -> a)
parseArgFlag flagname f = CliParser $ \case
    (x:xs) | x==flagname -> Right (xs, f True)
    (x:_) -> Left $ NoMatch flagname x
    _ -> Left $ NoMatch flagname ""

parseArgValueFlag :: String -> (String -> a -> a) -> CliParser (a -> a)
parseArgValueFlag flagname f = CliParser $ \case
    (x:y:xs) | x==flagname -> Right (xs, f y)
    (x:_) | x == flagname -> Left (MissingValue x)
    (x:xs) | isPrefixOf flagname x -> Right (xs, f y) 
      where y = case stripPrefix flagname x of
                Just ('=':ys) -> ys
                Just ys -> ys
                Nothing -> error "was"
    (x:_) -> Left $ NoMatch flagname x
    _ -> Left $ NoMatch flagname ""

parseMultiFlag variants seg f = alternatives . variations variants $ \flag -> parseArgValueFlag flag (\x -> seg <>~ [f x])
