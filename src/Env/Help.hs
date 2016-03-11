{-# LANGUAGE NamedFieldPuns #-}
module Env.Help
  ( helpInfo
  , helpDoc
  , Info
  , ErrorHandler
  , defaultInfo
  , defaultErrorHandler
  , header
  , desc
  , footer
  , handleError
  ) where

import           Data.Foldable (asum)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Ord (comparing)

import           Env.Error (Error)
import qualified Env.Error as Error
import           Env.Free
import           Env.Parse hiding (Mod)


helpInfo :: Info e -> Parser e b -> [(String, e)] -> String
helpInfo Info {infoHeader, infoDesc, infoFooter, infoHandleError} p errors =
  List.intercalate "\n\n" $ catMaybes
    [ infoHeader
    , fmap (List.intercalate "\n" . splitWords 50) infoDesc
    , Just (helpDoc p)
    , fmap (List.intercalate "\n" . splitWords 50) infoFooter
    ] ++ helpErrors infoHandleError errors

-- | A pretty-printed list of recognized environment variables suitable for usage messages
helpDoc :: Parser e a -> String
helpDoc p =
  List.intercalate "\n" ("Available environment variables:\n" : helpParserDoc p)

helpParserDoc :: Parser e a -> [String]
helpParserDoc = concat . Map.elems . foldAlt (\v -> Map.singleton (varfName v) (helpVarfDoc v)) . unParser

helpVarfDoc :: VarF e a -> [String]
helpVarfDoc VarF { varfName, varfHelp, varfHelpDef } =
  case varfHelp of
    Nothing -> [indent 2 varfName]
    Just h
      | k > 15    -> indent 2 varfName : map (indent 25) (splitWords 30 t)
      | otherwise ->
          case zipWith indent (23 - k : repeat 25) (splitWords 30 t) of
            (x : xs) -> (indent 2 varfName ++ x) : xs
            []       -> [indent 2 varfName]
     where k = length varfName
           t = maybe h (\s -> h ++ " (default: " ++ s ++")") varfHelpDef

splitWords :: Int -> String -> [String]
splitWords n = go [] 0 . words
 where
  go acc _ [] = prep acc
  go acc k (w : ws)
    | k + z < n = go (w : acc) (k + z) ws
    | z > n     = prep acc ++ case splitAt n w of (w', w'') -> w' : go [] 0 (w'' : ws)
    | otherwise = prep acc ++ go [w] z ws
   where
    z = length w

  prep []  = []
  prep acc = [unwords (reverse acc)]

indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

helpErrors :: ErrorHandler e -> [(String, e)] -> [String]
helpErrors _       [] = []
helpErrors handler fs =
  [ "Parsing errors:"
  , List.intercalate "\n" (mapMaybe (uncurry handler) (List.sortBy (comparing varName) fs))
  ]

-- | Parser's metadata
data Info e = Info
  { infoHeader      :: Maybe String
  , infoDesc        :: Maybe String
  , infoFooter      :: Maybe String
  , infoHandleError :: ErrorHandler e
  }

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler e = String -> e -> Maybe String

defaultInfo :: Info Error
defaultInfo = Info
  { infoHeader = Nothing
  , infoDesc = Nothing
  , infoFooter = Nothing
  , infoHandleError = defaultErrorHandler
  }

-- | A help text header (it usually includes the application's name and version)
header :: String -> Info e -> Info e
header h i = i {infoHeader=Just h}

-- | A short description
desc :: String -> Info e -> Info e
desc h i = i {infoDesc=Just h}

-- | A help text footer (it usually includes examples)
footer :: String -> Info e -> Info e
footer h i = i {infoFooter=Just h}

-- | An error handler
handleError :: ErrorHandler e -> Info x -> Info e
handleError handler i = i {infoHandleError=handler}

defaultErrorHandler :: (Error.AsUnset e, Error.AsEmpty e, Error.AsInvalid e) => ErrorHandler e
defaultErrorHandler name err =
  asum [handleUnsetError name err, handleEmptyError name err, handleInvalidError name err]

handleUnsetError :: Error.AsUnset e => ErrorHandler e
handleUnsetError name =
  fmap (\() -> indent 2 (name ++ " is unset")) . Error.tryUnset

handleEmptyError :: Error.AsEmpty e => ErrorHandler e
handleEmptyError name =
  fmap (\() -> indent 2 (name ++ " is empty")) . Error.tryEmpty

handleInvalidError :: Error.AsInvalid e => ErrorHandler e
handleInvalidError name =
  fmap (\val -> indent 2 (name ++ " has the invalid value " ++ val)) . Error.tryInvalid

varName :: (String, e) -> String
varName (n, _) = n
