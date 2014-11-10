{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe #-}
-- | Here's a simple example
--
-- @
-- module Main (main) where
--
-- import Control.Monad (unless)
-- import Env
--
-- data Hello = Hello { name :: String, quiet :: Bool }
--
-- hello :: IO Hello
-- hello = 'Env.parse' ('header' \"envparse example\") $
--   Hello \<$\> 'var' ('str' <=< 'nonempty') \"NAME\"  ('help' \"Target for the greeting\")
--         \<*\> 'switch'                 \"QUIET\" ('help' \"Whether to actually print the greeting\")
--
-- main :: IO ()
-- main = do
--   Hello { name, quiet } <- hello
--   unless quiet $
--     putStrLn (\"Hello, \" ++ name ++ \"!\")
-- @
module Env
  ( parse
  , Parser
  , Mod
  , Info
  , header
  , desc
  , footer
  , var
  , Var
  , Reader
  , str
  , nonempty
  , auto
  , def
  , helpDef
  , flag
  , switch
  , Flag
  , HasHelp
  , help
  -- * Re-exports
  -- $re-exports
  , (<$>), Applicative(..), optional
  , (<=<), (>=>)
  , (<>), mempty, mconcat
  , asum
  -- * Testing
  -- $testing
  , fromEnv
  ) where

import           Control.Applicative
import           Control.Monad ((>=>), (<=<))
import           Data.Foldable (asum)
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid (Monoid(..), (<>))
import           Data.String (IsString(..))
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure)
import qualified System.IO as IO

import           Env.Free

-- $re-exports
-- External functions that may be useful to the consumer of the library

-- $testing
-- Utilities to test—without dabbling in IO—that your parsers do
-- what you want them to do


-- | Parse the environment
--
-- Prints the help text and exits with @EXIT_FAILURE@ if it encounters a parse error
--
-- @
-- >>> parse ('header' \"env-parse 0.1.0\") ('var' 'str' \"USER\" ('def' \"nobody\"))
-- @
parse :: Mod Info a -> Parser a -> IO a
parse i p = either die return . note (helpDoc i p) . fromEnv p =<< getEnvironment

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

die :: String -> IO a
die m = do IO.hPutStrLn IO.stderr m; exitFailure


-- | An environment parser
newtype Parser a = Parser { unParser :: Alt VarF a }
    deriving (Functor)

instance Applicative Parser where
  pure = Parser . pure
  Parser f <*> Parser x = Parser (f <*> x)

instance Alternative Parser where
  empty = Parser empty
  Parser f <|> Parser x = Parser (f <|> x)

data VarF a = VarF
  { varfName    :: String
  , varfReader  :: String -> Maybe a
  , varfHelp    :: Maybe String
  , varfDef     :: Maybe a
  , varfHelpDef :: Maybe String
  } deriving (Functor)

-- | An environment variable's value parser. Use @(<=<)@ and @(>=>)@ to combine these
type Reader a = String -> Maybe a

-- | Parse a particular variable from the environment
--
-- @
-- >>> var 'str' \"EDITOR\" ('def' \"vim\" <> 'helpDef' show)
-- @
var :: Reader a -> String -> Mod Var a -> Parser a
var r n (Mod f) = Parser . liftAlt $ VarF
  { varfName    = n
  , varfReader  = r
  , varfHelp    = varHelp
  , varfDef     = varDef
  , varfHelpDef = varHelpDef <*> varDef
  }
 where
  Var { varHelp, varDef, varHelpDef } = f defaultVar

-- | A flag that takes the active value if the environment variable
-- is set and non-empty and the default value otherwise
--
-- /Note:/ this parser never fails.
flag
  :: a -- ^ default value
  -> a -- ^ active value
  -> String -> Mod Flag a -> Parser a
flag f t n (Mod g) = Parser . liftAlt $ VarF
  { varfName    = n
  , varfReader  = Just . maybe f (const t) . nonempty
  , varfHelp    = flagHelp
  , varfDef     = Just f
  , varfHelpDef = Nothing
  }
 where
  Flag { flagHelp } = g defaultFlag

-- | A simple boolean 'flag'
--
-- /Note:/ same caveats apply
switch :: String -> Mod Flag Bool -> Parser Bool
switch = flag False True

-- | The trivial reader
str :: IsString s => Reader s
str = Just . fromString

-- | The reader that accepts only non-empty strings
nonempty :: Reader String
nonempty [] = Nothing
nonempty xs = Just xs

-- | The reader that uses the 'Read' instance of the type
auto :: Read a => Reader a
auto = \s -> case reads s of [(v, "")] -> Just v; _ -> Nothing
{-# ANN auto "HLint: ignore Redundant lambda" #-}


-- | This represents a modification of the properties of a particular 'Parser'.
-- Combine them using the 'Monoid' instance.
newtype Mod t a = Mod (t a -> t a)

instance Monoid (Mod t a) where
  mempty = Mod id
  mappend (Mod f) (Mod g) = Mod (g . f)


-- | Parser's metadata
data Info a = Info
  { infoHeader :: Maybe String
  , infoDesc   :: Maybe String
  , infoFooter :: Maybe String
  }

defaultInfo :: Info a
defaultInfo = Info
  { infoHeader = Nothing
  , infoDesc   = Nothing
  , infoFooter = Nothing
  }

-- | A help text header (it usually includes an application name and version)
header :: String -> Mod Info a
header h = Mod (\i -> i { infoHeader = Just h })

-- | A short program description
desc :: String -> Mod Info a
desc h = Mod (\i -> i { infoDesc = Just h })

-- | A help text footer (it usually includes examples)
footer :: String -> Mod Info a
footer h = Mod (\i -> i { infoFooter = Just h })


-- | Environment variable metadata
data Var a = Var
  { varHelp    :: Maybe String
  , varHelpDef :: Maybe (a -> String)
  , varDef     :: Maybe a
  }

defaultVar :: Var a
defaultVar = Var
  { varHelp    = Nothing
  , varDef     = Nothing
  , varHelpDef = Nothing
  }

-- | The default value of the variable
--
-- /Note:/ specifying the default value means the parser won't ever fail.
def :: a -> Mod Var a
def d = Mod (\v -> v { varDef = Just d })

-- | Flag metadata
data Flag a = Flag
  { flagHelp    :: Maybe String
  }

defaultFlag :: Flag a
defaultFlag = Flag { flagHelp = Nothing }

-- | Show the default value of the variable in the help text
helpDef :: (a -> String) -> Mod Var a
helpDef d = Mod (\v -> v { varHelpDef = Just d })


-- | A class of things that can have a help message attached to them
class HasHelp t where
  setHelp :: String -> t a -> t a

instance HasHelp Var where
  setHelp h v = v { varHelp = Just h }

instance HasHelp Flag where
  setHelp h v = v { flagHelp = Just h }

-- | Attach help text to the variable
help :: HasHelp t => String -> Mod t a
help = Mod . setHelp


helpDoc :: Mod Info a -> Parser a -> String
helpDoc (Mod f) p = intercalate "\n\n" . catMaybes $
  [ infoHeader
  , fmap (intercalate "\n" . splitEvery 50) infoDesc
  , Just "Available environment variables:"
  , Just (intercalate "\n" (helpParserDoc p))
  , fmap (intercalate "\n" . splitEvery 50) infoFooter
  ]
 where
  Info { infoHeader, infoDesc, infoFooter } = f defaultInfo

helpParserDoc :: Parser a -> [String]
helpParserDoc = concat . Map.elems . Map.fromList . foldAlt (\v -> [(varfName v, helpVarfDoc v)]) . unParser

helpVarfDoc :: VarF a -> [String]
helpVarfDoc VarF { varfName, varfHelp, varfHelpDef } =
  case varfHelp of
    Nothing -> [indent 2 varfName]
    Just h
      | k > 15    -> indent 2 varfName : map (indent 25 . stripl) (splitEvery 30 t)
      | otherwise ->
          case zipWith indent (23 - k : repeat 25) (map stripl (splitEvery 30 t)) of
            (x : xs) -> (indent 2 varfName ++ x) : xs
            []       -> [indent 2 varfName]
     where k = length varfName
           t = maybe h (\s -> h ++ " (default: " ++ s ++")") varfHelpDef

stripl :: String -> String
stripl (' ' : xs) = stripl xs
stripl xs = xs

splitEvery :: Int -> String -> [String]
splitEvery _ "" = []
splitEvery n xs = case splitAt n xs of ~(ys, zs) -> ys : splitEvery n zs

indent :: Int -> String -> String
indent n s = replicate n ' ' <> s


-- | Parse a static environment
fromEnv :: Parser a -> [(String, String)] -> Maybe a
fromEnv (Parser p) xs = runAlt go p
 where
  go v = (varfReader v =<< lookup (varfName v) xs) <|> varfDef v
