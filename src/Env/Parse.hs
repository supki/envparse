{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Env.Parse
  ( Parser(..)
  , VarF(..)
  , parsePure
  , Mod(..)
  , prefixed
  , var
  , Var(..)
  , defaultVar
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
  ) where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as Map
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
import           Data.String (IsString(..))

import           Env.Free
import qualified Env.Error as Error
import           Env.Val


-- | Try to parse a pure environment
parsePure :: Error.AsUnset e => Parser e b -> [(String, String)] -> Either [(String, e)] b
parsePure (Parser p) (Map.fromList -> env) =
  toEither (runAlt go p)
 where
  go v = maybe id (\d x -> x <|> pure d) (varfDef v) (fromEither (readVar v =<< lookupVar v env))

lookupVar :: Error.AsUnset e => VarF e a -> Map String String -> Either [(String, e)] String
lookupVar VarF {varfName} =
  note [(varfName, Error.unset)] . Map.lookup varfName

readVar :: VarF e a -> String -> Either [(String, e)] a
readVar VarF {varfName, varfReader} =
  mapLeft (pure . (\err -> (varfName, err))) . varfReader

note :: a -> Maybe b -> Either a b
note a =
  maybe (Left a) Right

mapLeft :: (a -> b) -> Either a t -> Either b t
mapLeft f =
  either (Left . f) Right


-- | An environment parser
newtype Parser e a = Parser { unParser :: Alt (VarF e) a }
    deriving (Functor)

instance Applicative (Parser e) where
  pure = Parser . pure
  Parser f <*> Parser x = Parser (f <*> x)

instance Alternative (Parser e) where
  empty = Parser empty
  Parser f <|> Parser x = Parser (f <|> x)

-- | The string to prepend to the name of every declared environment variable
prefixed :: String -> Parser e a -> Parser e a
prefixed pre =
  Parser . hoistAlt (\v -> v { varfName = pre ++ varfName v }) . unParser


data VarF e a = VarF
  { varfName    :: String
  , varfReader  :: Reader e a
  , varfHelp    :: Maybe String
  , varfDef     :: Maybe a
  , varfHelpDef :: Maybe String
  } deriving (Functor)

-- | An environment variable's value parser. Use @(<=<)@ and @(>=>)@ to combine these
type Reader e a = String -> Either e a

-- | Parse a particular variable from the environment
--
-- @
-- >>> var 'str' \"EDITOR\" ('def' \"vim\" <> 'helpDef' show)
-- @
var :: Reader e a -> String -> Mod Var a -> Parser e a
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
  :: forall e a. Error.AsEmpty e
  => a -- ^ default value
  -> a -- ^ active value
  -> String -> Mod Flag a -> Parser e a
flag f t n (Mod g) = Parser . liftAlt $ VarF
  { varfName    = n
  , varfReader  = Right . either (const f) (const t) . (nonempty :: Reader e String)
  , varfHelp    = flagHelp
  , varfDef     = Just f
  , varfHelpDef = Nothing
  }
 where
  Flag { flagHelp } = g defaultFlag

-- | A simple boolean 'flag'
--
-- /Note:/ the same caveats apply.
switch :: Error.AsEmpty e => String -> Mod Flag Bool -> Parser e Bool
switch = flag False True

-- | The trivial reader
str :: IsString s => Reader e s
str = Right . fromString

-- | The reader that accepts only non-empty strings
nonempty :: (Error.AsEmpty e, IsString s) => Reader e s
nonempty = fmap fromString . go where go [] = Left Error.empty; go xs = Right xs

-- | The reader that uses the 'Read' instance of the type
auto :: (Error.AsInvalid e, Read a) => Reader e a
auto = \s -> case reads s of [(v, "")] -> Right v; _ -> Left (Error.invalid (show s))
{-# ANN auto "HLint: ignore Redundant lambda" #-}


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
