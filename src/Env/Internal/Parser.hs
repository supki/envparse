{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Env.Internal.Parser
  ( Parser(..)
  , VarF(..)
  , parsePure
  , traverseSensitiveVar
  , Mod(..)
  , prefixed
  , var
  , Var(..)
  , defaultVar
  , Reader
  , eitherReader
  , str
  , char
  , nonempty
  , splitOn
  , auto
  , def
  , helpDef
  , showDef
  , flag
  , switch
  , Flag
  , HasHelp
  , help
  , sensitive
  ) where

import           Control.Applicative
import           Control.Arrow (left)
import           Control.Monad ((<=<))
import           Data.Foldable (traverse_)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup (Semigroup(..))
#endif
import           Data.String (IsString(..))

import           Env.Internal.Free
import qualified Env.Internal.Error as Error
import           Env.Internal.Val


-- | Try to parse a pure environment
parsePure :: Error.AsUnset e => Parser e a -> [(String, String)] -> Either [(String, e)] a
parsePure (Parser p) (Map.fromList -> env) =
  toEither (runAlt (fromEither . left pure . go) p)
 where
  go v@VarF {..} =
    case lookupVar v env of
      Left lookupErr ->
        maybe (Left lookupErr) pure varfDef
      Right val ->
        readVar v val

traverseSensitiveVar :: Applicative m => Parser e a -> (String -> m b) -> m ()
traverseSensitiveVar Parser {unParser} f =
  traverse_ f sensitiveVars
 where
  sensitiveVars =
    foldAlt (\VarF {varfSensitive, varfName} -> if varfSensitive then Set.singleton varfName else Set.empty) unParser

readVar :: VarF e a -> String -> Either (String, e) a
readVar VarF {..} =
  addName varfName . varfReader

lookupVar :: Error.AsUnset e => VarF e a -> Map String String -> Either (String, e) String
lookupVar VarF {..} =
  addName varfName . maybe (Left Error.unset) Right . Map.lookup varfName

addName :: String -> Either e a -> Either (String, e) a
addName name =
  left ((,) name)

-- | An environment parser
newtype Parser e a = Parser { unParser :: Alt (VarF e) a }
    deriving (Functor)

instance Applicative (Parser e) where
  pure =
    Parser . pure
  Parser f <*> Parser x =
    Parser (f <*> x)

instance Alternative (Parser e) where
  empty =
    Parser empty
  Parser f <|> Parser x =
    Parser (f <|> x)

-- | The string to prepend to the name of every declared environment variable
prefixed :: String -> Parser e a -> Parser e a
prefixed pre =
  Parser . hoistAlt (\v -> v {varfName=pre ++ varfName v}) . unParser

-- | Mark the enclosed variables as sensitive to remove them from the environment
-- once they've been parsed successfully.
sensitive :: Parser e a -> Parser e a
sensitive =
  Parser . hoistAlt (\v -> v {varfSensitive = True}) . unParser


data VarF e a = VarF
  { varfName      :: String
  , varfReader    :: Reader e a
  , varfHelp      :: Maybe String
  , varfDef       :: Maybe a
  , varfHelpDef   :: Maybe String
  , varfSensitive :: Bool
  } deriving (Functor)

liftVarF :: VarF e a -> Parser e a
liftVarF =
  Parser . liftAlt

-- | An environment variable's value parser. Use @(<=<)@ and @(>=>)@ to combine these
type Reader e a = String -> Either e a

-- | Create a 'Reader' from a simple parser function
eitherReader :: Error.AsUnread e => (String -> Either String a) -> Reader e a
eitherReader f s = left (Error.unread . suffix) $ f s
 where
  suffix x = x <> ": " <> show s

-- | Parse a particular variable from the environment
--
-- @
-- >>> var 'str' \"EDITOR\" ('def' \"vim\" <> 'helpDef' show)
-- @
var :: Error.AsUnset e => Reader e a -> String -> Mod Var a -> Parser e a
var r n (Mod f) =
  liftVarF $ VarF
    { varfName = n
    , varfReader = r
    , varfHelp = varHelp
    , varfDef = varDef
    , varfHelpDef = varHelpDef <*> varDef
    , varfSensitive = varSensitive
    }
 where
  Var {varHelp, varDef, varHelpDef, varSensitive} = f defaultVar

-- | A flag that takes the active value if the environment variable
-- is set and non-empty and the default value otherwise
--
-- /Note:/ this parser never fails.
flag
  :: a -- ^ default value
  -> a -- ^ active value
  -> String -> Mod Flag a -> Parser e a
flag f t n (Mod g) =
  liftVarF $ VarF
    { varfName = n
    , varfReader = \val ->
        pure $ case (nonempty :: Reader Error.Error String) val of
          Left  _ -> f
          Right _ -> t
    , varfHelp = flagHelp
    , varfDef = Just f
    , varfHelpDef = Nothing
    , varfSensitive = flagSensitive
    }
 where
  Flag {flagHelp, flagSensitive} = g defaultFlag

-- | A simple boolean 'flag'
--
-- /Note:/ this parser never fails.
switch :: String -> Mod Flag Bool -> Parser e Bool
switch =
  flag False True

-- | The trivial reader
str :: IsString s => Reader e s
str =
  Right . fromString

-- | The reader that accepts only non-empty strings
nonempty :: (Error.AsEmpty e, IsString s) => Reader e s
nonempty =
  fmap fromString . go where go [] = Left Error.empty; go xs = Right xs

-- | The reader that uses the 'Read' instance of the type
auto :: (Error.AsUnread e, Read a) => Reader e a
auto s =
  case reads s of [(v, "")] -> Right v; _ -> Left (Error.unread (show s))

-- | The single character string reader
char :: Error.AsUnread e => Reader e Char
char s =
  case s of [c] -> Right c; _ -> Left (Error.unread "must be a one-character string")

-- | The reader that splits a string into a list of strings consuming the separator.
splitOn :: Char -> Reader e [String]
splitOn sep = Right . go
 where
  go [] = []
  go xs = go' xs

  go' xs =
    case break (== sep) xs of
      (ys, []) ->
        ys : []
      (ys, _ : zs) ->
        ys : go' zs


-- | This represents a modification of the properties of a particular 'Parser'.
-- Combine them using the 'Monoid' instance.
newtype Mod t a = Mod (t a -> t a)

#if MIN_VERSION_base(4,9,0)
instance Semigroup (Mod t a) where
  Mod f <> Mod g = Mod (g . f)
#endif

instance Monoid (Mod t a) where
  mempty = Mod id
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
  mappend = (<>)
#else
  mappend (Mod f) (Mod g) = Mod (g . f)
#endif

-- | Environment variable metadata
data Var a = Var
  { varHelp      :: Maybe String
  , varHelpDef   :: Maybe (a -> String)
  , varDef       :: Maybe a
  , varSensitive :: Bool
  }

defaultVar :: Var a
defaultVar = Var
  { varHelp = Nothing
  , varDef = Nothing
  , varHelpDef = Nothing
  , varSensitive = defaultSensitive
  }

defaultSensitive :: Bool
defaultSensitive = False

-- | The default value of the variable
--
-- /Note:/ specifying it means the parser won't ever fail.
def :: a -> Mod Var a
def d =
  Mod (\v -> v {varDef=Just d})

-- | Flag metadata
data Flag a = Flag
  { flagHelp      :: Maybe String
  , flagSensitive :: Bool
  }

defaultFlag :: Flag a
defaultFlag = Flag
  { flagHelp = Nothing
  , flagSensitive = defaultSensitive
  }

-- | Show the default value of the variable in help.
helpDef :: (a -> String) -> Mod Var a
helpDef d =
  Mod (\v -> v {varHelpDef=Just d})

-- | Use the 'Show' instance to show the default value of the variable in help.
showDef :: Show a => Mod Var a
showDef =
  helpDef show


-- | A class of things that can have a help message attached to them
class HasHelp t where
  setHelp :: String -> t a -> t a

instance HasHelp Var where
  setHelp h v = v {varHelp=Just h}

instance HasHelp Flag where
  setHelp h v = v {flagHelp=Just h}

-- | Attach help text to the variable
help :: HasHelp t => String -> Mod t a
help =
  Mod . setHelp
