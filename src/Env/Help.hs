{-# LANGUAGE NamedFieldPuns #-}
module Env.Help
  ( helpInfo
  , helpInfoWith
  , helpDoc
  , ErrorHandler
  , handleError
  ) where

import           Data.Foldable (asum)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Ord (comparing)

import           Env.Free
import           Env.Parse
import           Env.Error (Error(..))
import qualified Env.Error as Error


helpInfo :: Info a -> Parser Error b -> [(String, Error)] -> String
helpInfo = helpInfoWith handleError

helpInfoWith :: ErrorHandler e -> Info a -> Parser e b -> [(String, e)] -> String
helpInfoWith handler Info { infoHeader, infoDesc, infoFooter } p errors =
  List.intercalate "\n\n" $ catMaybes
    [ infoHeader
    , fmap (List.intercalate "\n" . splitWords 50) infoDesc
    , Just (helpDoc p)
    , fmap (List.intercalate "\n" . splitWords 50) infoFooter
    ] ++ helpErrors handler errors

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

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler e = String -> e -> Maybe String

helpErrors :: ErrorHandler e -> [(String, e)] -> [String]
helpErrors _       [] = []
helpErrors handler fs =
  [ "Parsing errors:"
  , List.intercalate "\n" (mapMaybe (uncurry handler) (List.sortBy (comparing varName) fs))
  ]

handleError :: (Error.AsUnset e, Error.AsEmpty e, Error.AsInvalid e) => ErrorHandler e
handleError name err =
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
