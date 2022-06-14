{-# LANGUAGE NamedFieldPuns #-}
module Env.Internal.Help
  ( helpInfo
  , helpDoc
  , Info
  , ErrorHandler
  , defaultInfo
  , defaultErrorHandler
  , header
  , desc
  , footer
  , widthMax
  , handleError
  ) where

import           Data.Foldable (asum)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Ord (comparing)

import           Env.Internal.Error (Error)
import qualified Env.Internal.Error as Error
import           Env.Internal.Free
import           Env.Internal.Parser hiding (Mod)


helpInfo :: Info e -> Parser e b -> [(String, e)] -> String
helpInfo Info {infoHeader, infoDesc, infoFooter, infoHandleError, infoWidthMax} p errors =
  List.intercalate "\n\n" $ catMaybes
    [ infoHeader
    , fmap (List.intercalate "\n" . splitWords infoWidthMax) infoDesc
    , Just (helpDoc infoWidthMax p)
    , fmap (List.intercalate "\n" . splitWords infoWidthMax) infoFooter
    ] ++ helpErrors infoHandleError errors

-- | A pretty-printed list of recognized environment variables suitable for usage messages
helpDoc :: Int -> Parser e a -> String
helpDoc widthMax p =
  List.intercalate "\n" ("Available environment variables:\n" : helpParserDoc widthMax p)

helpParserDoc :: Int -> Parser e a -> [String]
helpParserDoc widthMax =
  concat . Map.elems . foldAlt (\v -> Map.singleton (varfName v) (helpVarfDoc widthMax v)) . unParser

helpVarfDoc :: Int -> VarF e a -> [String]
helpVarfDoc widthMax VarF {varfName, varfHelp, varfHelpDef} =
  case varfHelp of
    Nothing -> [indent vo varfName]
    Just h
      | k > nameWidthMax ->
          indent vo varfName : map (indent ho) (splitWords (widthMax - ho) t)
      | otherwise ->
          case zipWith indent (ho - vo - k : repeat ho) (splitWords (widthMax - ho) t) of
            (x : xs) -> (indent vo varfName ++ x) : xs
            []       -> [indent vo varfName]
     where
      k = length varfName
      t = maybe h (\s -> h ++ " (default: " ++ s ++")") varfHelpDef
 where
  -- The longest variable name that fits the compact view.
  nameWidthMax = ho - vo - 1 {- the space between the variable name and the help text -}
  vo = 2  -- variable name offset
  ho = 25 -- help text offset

splitWords :: Int -> String -> [String]
splitWords n =
  go [] 0 . words
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
indent n s =
  replicate n ' ' ++ s

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
  , infoWidthMax    :: Int
  }

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler e = String -> e -> Maybe String

defaultInfo :: Info Error
defaultInfo = Info
  { infoHeader = Nothing
  , infoDesc = Nothing
  , infoFooter = Nothing
  , infoHandleError = defaultErrorHandler
  , infoWidthMax = 80
  }

-- | Set the help text header (it usually includes the application's name and version)
header :: String -> Info e -> Info e
header h i = i {infoHeader=Just h}

-- | Set the short description
desc :: String -> Info e -> Info e
desc h i = i {infoDesc=Just h}

-- | Set the help text footer (it usually includes examples)
footer :: String -> Info e -> Info e
footer h i = i {infoFooter=Just h}

-- | Set the max info width.
--
-- /Note:/ It will be set to 26 columns if a smaller value is passed.
widthMax :: Int -> Info e -> Info e
widthMax n i = i {infoWidthMax=max 26 n}

-- | An error handler
handleError :: ErrorHandler e -> Info x -> Info e
handleError handler i = i {infoHandleError=handler}

-- | The default error handler
defaultErrorHandler :: (Error.AsUnset e, Error.AsEmpty e, Error.AsUnread e) => ErrorHandler e
defaultErrorHandler name err =
  asum [handleUnsetError name err, handleEmptyError name err, handleUnreadError name err]

handleUnsetError :: Error.AsUnset e => ErrorHandler e
handleUnsetError name =
  fmap (\() -> indent 2 (name ++ " is unset")) . Error.tryUnset

handleEmptyError :: Error.AsEmpty e => ErrorHandler e
handleEmptyError name =
  fmap (\() -> indent 2 (name ++ " is empty")) . Error.tryEmpty

handleUnreadError :: Error.AsUnread e => ErrorHandler e
handleUnreadError name =
  fmap (\val -> indent 2 (name ++ " has value " ++ val ++ " that cannot be parsed")) . Error.tryUnread

varName :: (String, e) -> String
varName (n, _) = n
