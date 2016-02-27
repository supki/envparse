{-# LANGUAGE NamedFieldPuns #-}
module Env.Help
  ( helpInfo
  , helpDoc
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Ord (comparing)

import           Env.Free
import           Env.Parse
import           Env.Error (Error(..), Unset(..), Empty(..), Invalid(..))


helpInfo :: Info a -> Parser e b -> [Error] -> String
helpInfo Info { infoHeader, infoDesc, infoFooter } p errors =
  List.intercalate "\n\n" $ catMaybes
    [ infoHeader
    , fmap (List.intercalate "\n" . splitWords 50) infoDesc
    , Just (helpDoc p)
    , fmap (List.intercalate "\n" . splitWords 50) infoFooter
    ] ++ helpErrors errors

-- | A pretty-printed list of recognized environment variables suitable for usage messages.
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

helpErrors :: [Error] -> [String]
helpErrors [] = []
helpErrors fs =
  [ "Parsing errors:"
  , List.intercalate "\n" (map helpError (List.sortBy (comparing varName) fs))
  ]

helpError :: Error -> String
helpError (UnsetError (Unset n)) = "  " ++ n ++ " is unset"
helpError (EmptyError (Empty n)) = "  " ++ n ++ " is empty"
helpError (InvalidError (Invalid n val)) = "  " ++ n ++ " has an invalid value " ++ val

varName :: Error -> String
varName (UnsetError (Unset n)) = n
varName (EmptyError (Empty n)) = n
varName (InvalidError (Invalid n _)) = n

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
