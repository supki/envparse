{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Env.Internal.Parser
  ( Parser(..)
  , VarF(..)
  , parsePure
  , eachVar
  , Mod(..)
  , prefixed
  , var
  , Var(..)
  , defaultVar
  , Reader
  , str
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
  ) where

import           Control.Applicative
import           Control.Arrow (left)
import           Control.Monad ((<=<))
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
import           Data.String (IsString(..))

import           Env.Internal.Free
import qualified Env.Internal.Error as Error
import           Env.Internal.Val


-- | Try to parse a pure environment
parsePure :: Parser e a -> [(String, String)] -> Either [(String, e)] a
parsePure (Parser p) (Map.fromList -> env) =
  toEither (runAlt go p)
 where
  go v = maybe id (\d x -> x <|> pure d) (varfDef v) (fromEither (readVar v env))

eachVar :: Applicative m => Parser e a -> (String -> m b) -> m ()
eachVar Parser {unParser} =
  for_ (foldAlt (Set.singleton . varfName) unParser)

readVar :: VarF e a -> Map String String -> Either [(String, e)] a
readVar VarF {varfName, varfReader} =
  left (pure . (\err -> (varfName, err))) . varfReader varfName


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


data VarF e a = VarF
  { varfName    :: String
  , varfReader  :: String -> Map String String -> Either e a
  , varfHelp    :: Maybe String
  , varfDef     :: Maybe a
  , varfHelpDef :: Maybe String
  } deriving (Functor)

liftVarF :: VarF e a -> Parser e a
liftVarF =
  Parser . liftAlt

-- | An environment variable's value parser. Use @(<=<)@ and @(>=>)@ to combine these
type Reader e a = String -> Either e a

lookupVar :: Error.AsUnset e => String -> Map String String -> Either e String
lookupVar name =
  maybe (Left Error.unset) Right . Map.lookup name

-- | Parse a particular variable from the environment
--
-- @
-- >>> var 'str' \"EDITOR\" ('def' \"vim\" <> 'helpDef' show)
-- @
var :: Error.AsUnset e => Reader e a -> String -> Mod Var a -> Parser e a
var r n (Mod f) =
  liftVarF $ VarF
    { varfName    = n
    , varfReader  = \name -> r <=< lookupVar name
    , varfHelp    = varHelp
    , varfDef     = varDef
    , varfHelpDef = varHelpDef <*> varDef
    }
 where
  Var {varHelp, varDef, varHelpDef} = f defaultVar

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
    { varfName    = n
    , varfReader  = \name env ->
        pure $ case (nonempty :: Reader Error.Error String) =<< lookupVar name env of
          Left  _ -> f
          Right _ -> t
    , varfHelp    = flagHelp
    , varfDef     = Just f
    , varfHelpDef = Nothing
    }
 where
  Flag {flagHelp} = g defaultFlag

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

instance Monoid (Mod t a) where
  mempty = Mod id
  mappend (Mod f) (Mod g) = Mod (g . f)

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
-- /Note:/ specifying it means the parser won't ever fail.
def :: a -> Mod Var a
def d =
  Mod (\v -> v {varDef=Just d})

-- | Flag metadata
data Flag a = Flag
  { flagHelp   :: Maybe String
  }

defaultFlag :: Flag a
defaultFlag =
  Flag {flagHelp=Nothing}

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
