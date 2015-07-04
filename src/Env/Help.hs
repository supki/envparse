{-# LANGUAGE NamedFieldPuns #-}
module Env.Help
  ( helpInfo
  , helpDoc
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)

import           Env.Free
import           Env.Parse


helpInfo :: Info a -> Parser b -> [Error] -> String
helpInfo Info { infoHeader, infoDesc, infoFooter } p errors =
  List.intercalate "\n\n" $ catMaybes
    [ infoHeader
    , fmap (List.intercalate "\n" . splitWords 50) infoDesc
    , Just (helpDoc p)
    , fmap (List.intercalate "\n" . splitWords 50) infoFooter
    ] ++ helpErrors errors

-- | A pretty-printed list of recognized environment variables suitable for usage messages.
helpDoc :: Parser a -> String
helpDoc p =
  List.intercalate "\n" ("Available environment variables:\n" : helpParserDoc p)

helpParserDoc :: Parser a -> [String]
helpParserDoc = concat . Map.elems . foldAlt (\v -> Map.singleton (varfName v) (helpVarfDoc v)) . unParser

helpVarfDoc :: VarF a -> [String]
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
helpError (ParseError n e)  = "  " ++ n ++ " cannot be parsed: " ++ e
helpError (ENoExistError n) = "  " ++ n ++ " is unset"

varName :: Error -> String
varName (ParseError n _)  = n
varName (ENoExistError n) = n

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
indent n s = replicate n ' ' <> s
