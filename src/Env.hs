{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe #-}
-- | Boo
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
  , auto
  , def
  , helpDef
  , help
  -- * Re-exports
  -- $re-exports
  , (<$>), (<*>), (<*), (*>)
  , (<>), mempty, mconcat
  , asum
  -- * Testing
  -- $testing
  , fromEnv
  ) where

import           Control.Applicative
import           Data.Foldable (asum)
import           Data.String (IsString(..))
import           Data.Monoid (Monoid(..), (<>))
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure)
import qualified System.IO as IO

import           Env.Free
import           Env.Mon

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
newtype Parser a = Parser (Alt VarF a)

instance Functor Parser where
  fmap f (Parser x) = Parser (fmap f x)

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
  }

instance Functor VarF where
  fmap f v = v { varfReader = fmap f . varfReader v, varfDef = fmap f (varfDef v) }

-- | A String parser
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

-- | The trivial reader
str :: IsString s => Reader s
str = Just . fromString

-- | The reader that uses the 'Read' instance of the type
--
-- The name is somewhat weird, but that's an optparse-applicative's fault.
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

-- | Attach a help text to the variable
help :: String -> Mod Var a
help h = Mod (\v -> v { varHelp = Just h })

-- | The default value of the variable
--
-- /Note:/ specifying the default value means the parser won't ever fail.
def :: a -> Mod Var a
def d = Mod (\v -> v { varDef = Just d })

-- | Show the default value of the variable in the help text
helpDef :: (a -> String) -> Mod Var a
helpDef d = Mod (\v -> v { varHelpDef = Just d })


helpDoc :: Mod Info a -> Parser a -> String
helpDoc (Mod f) (Parser p) = unlines $ concat
  [ line infoHeader
  , line infoDesc
  , line (Just "Available environment variables:")
  , unMon (runAlt (Mon . helpVarfDoc) p)
  , line infoFooter
  ]
 where
  Info { infoHeader, infoDesc, infoFooter } = f defaultInfo

helpVarfDoc :: VarF a -> [String]
helpVarfDoc VarF { varfName, varfHelp, varfHelpDef } =
  case varfHelp of
    Nothing -> [varfName]
    Just h
      | k > 17    -> varfName : map (indent 25) (splitEvery 30 t)
      | otherwise ->
          case zipWith indent (25 - k : repeat 30) (splitEvery 30 t) of
            (x : xs) -> (varfName ++ x) : xs
            []       -> [varfName]
     where k = length varfName
           t = maybe h (\s -> h ++ " (default: " ++ s ++")") varfHelpDef

splitEvery :: Int -> String -> [String]
splitEvery _ "" = []
splitEvery n xs = case splitAt n xs of ~(ys, zs) -> ys : splitEvery n zs

indent :: Int -> String -> String
indent n s = replicate n ' ' <> s

line :: Maybe String -> [String]
line = maybe [] (\h -> [h, ""])


-- | Parse a static environment
fromEnv :: Parser a -> [(String, String)] -> Maybe a
fromEnv (Parser p) xs = runAlt go p
 where
  go v = (varfReader v =<< lookup (varfName v) xs) <|> varfDef v
