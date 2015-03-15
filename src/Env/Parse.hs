{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Env.Parse
  ( Parser(..)
  , VarF(..)
  , static
  , Error(..)
  , Mod(..)
  , Info(..)
  , defaultInfo
  , header
  , desc
  , footer
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
import           Data.Monoid (Monoid(..))
import           Data.String (IsString(..))

import           Env.Free
import           Env.Val


static :: Parser b -> [(String, String)] -> Either [Error] b
static (Parser p) (Map.fromList -> env) =
  toEither (runAlt go p)
 where
  go v = maybe id (\d x -> x <|> pure d) (varfDef v) (fromEither (readVar v =<< lookupVar v env))

lookupVar :: VarF a -> Map String String -> Either [Error] String
lookupVar v = note [ENoExistError (varfName v)] . Map.lookup (varfName v)

readVar :: VarF a -> String -> Either [Error] a
readVar v = mapLeft (pure . ParseError (varfName v)) . varfReader v

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

mapLeft :: (a -> b) -> Either a t -> Either b t
mapLeft f = either (Left . f) Right


-- | An environment parser
newtype Parser a = Parser { unParser :: Alt VarF a }
    deriving (Functor)

instance Applicative Parser where
  pure = Parser . pure
  Parser f <*> Parser x = Parser (f <*> x)

instance Alternative Parser where
  empty = Parser empty
  Parser f <|> Parser x = Parser (f <|> x)


data Error
  = ParseError String String
  | ENoExistError String
    deriving (Show, Eq)

data VarF a = VarF
  { varfName    :: String
  , varfReader  :: Reader a
  , varfHelp    :: Maybe String
  , varfDef     :: Maybe a
  , varfHelpDef :: Maybe String
  } deriving (Functor)

-- | An environment variable's value parser. Use @(<=<)@ and @(>=>)@ to combine these
type Reader a = String -> Either String a

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
  , varfReader  = Right . either (const f) (const t) . (nonempty :: Reader String)
  , varfHelp    = flagHelp
  , varfDef     = Just f
  , varfHelpDef = Nothing
  }
 where
  Flag { flagHelp } = g defaultFlag

-- | A simple boolean 'flag'
--
-- /Note:/ the same caveats apply.
switch :: String -> Mod Flag Bool -> Parser Bool
switch = flag False True

-- | The trivial reader
str :: IsString s => Reader s
str = Right . fromString

-- | The reader that accepts only non-empty strings
nonempty :: IsString s => Reader s
nonempty = fmap fromString . go where go [] = Left "a non-empty string is expected"; go xs = Right xs

-- | The reader that uses the 'Read' instance of the type
auto :: Read a => Reader a
auto = \s -> case reads s of [(v, "")] -> Right v; _ -> Left (show s ++ " is an invalid value")
{-# ANN auto "HLint: ignore Redundant lambda" #-}


-- | This represents a modification of the properties of a particular 'Parser'.
-- Combine them using the 'Monoid' instance.
newtype Mod t a = Mod (t a -> t a)

instance Monoid (Mod t a) where
  mempty = Mod id
  mappend (Mod f) (Mod g) = Mod (g . f)


-- | Parser's metadata
data Info a = Info
  { infoHeader   :: Maybe String
  , infoDesc     :: Maybe String
  , infoFooter   :: Maybe String
  , infoPrefixed :: Maybe String
  }

defaultInfo :: Info a
defaultInfo = Info
  { infoHeader   = Nothing
  , infoDesc     = Nothing
  , infoFooter   = Nothing
  , infoPrefixed = Nothing
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

-- | The string to prepend to every declared environment variable
prefixed :: String -> Mod Info a
prefixed h = Mod (\i -> i { infoPrefixed = Just h })


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
