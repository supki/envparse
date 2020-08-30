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
  , varHelpMaxColumns
  , varNameColumn
  , varHelpColumn
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
helpInfo Info {infoHeader, infoDesc, infoFooter, infoFormatOpts, infoHandleError} p errors =
  List.intercalate "\n\n" $ catMaybes
    [ infoHeader
    , fmap (List.intercalate "\n" . splitWords 50) infoDesc
    , Just (helpDoc infoFormatOpts p)
    , fmap (List.intercalate "\n" . splitWords 50) infoFooter
    ] ++ helpErrors infoHandleError errors

-- | A pretty-printed list of recognized environment variables suitable for usage messages
helpDoc :: FormatOpts -> Parser e a -> String
helpDoc f p =
  List.intercalate "\n" ("Available environment variables:\n" : helpParserDoc f p)

helpParserDoc :: FormatOpts -> Parser e a -> [String]
helpParserDoc f =
  concat . Map.elems . foldAlt (\v -> Map.singleton (varfName v) (helpVarfDoc f v)) . unParser

helpVarfDoc :: FormatOpts -> VarF e a -> [String]
helpVarfDoc FormatOpts {fOptsVHelpMaxColumns, fOptsVNameColumn, fOptsVHelpColumn} VarF {varfName, varfHelp, varfHelpDef} =
  case varfHelp of
    Nothing -> [indent fOptsVNameColumn varfName]
    Just h
      | fOptsVNameColumn + k > (floor . (*) (0.7::Float) . fromIntegral) fOptsVHelpColumn
      -> indent fOptsVNameColumn varfName : map (indent fOptsVHelpColumn) (splitWords fOptsVHelpMaxColumns t)
      | otherwise ->
          case zipWith indent (fOptsVHelpColumn - fOptsVNameColumn - k : repeat fOptsVHelpColumn) (splitWords fOptsVHelpMaxColumns t) of
            (x : xs) -> (indent fOptsVNameColumn varfName ++ x) : xs
            []       -> [indent fOptsVNameColumn varfName]
     where k = length varfName
           t = maybe h (\s -> h ++ " (default: " ++ s ++")") varfHelpDef

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
  , infoFormatOpts  :: FormatOpts
  , infoHandleError :: ErrorHandler e
  }

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler e = String -> e -> Maybe String

data FormatOpts = FormatOpts
  { fOptsVNameColumn :: Int
  , fOptsVHelpColumn :: Int
  , fOptsVHelpMaxColumns :: Int
  }

defaultInfo :: Info Error
defaultInfo = Info
  { infoHeader = Nothing
  , infoDesc = Nothing
  , infoFooter = Nothing
  , infoFormatOpts = defaultFormatOpts
  , infoHandleError = defaultErrorHandler
  }

defaultFormatOpts :: FormatOpts
defaultFormatOpts = FormatOpts
  { fOptsVHelpMaxColumns = 55
  , fOptsVNameColumn = 2
  , fOptsVHelpColumn = 25
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

-- | Column at which the name of a variable starts (defaults to 2)
varNameColumn :: Int -> Info e -> Info e
varNameColumn n i = i {infoFormatOpts = (infoFormatOpts i) {fOptsVNameColumn=n}}

-- | Column at which the help of a variable starts (defaults to 25)
varHelpColumn :: Int -> Info e -> Info e
varHelpColumn n i = i {infoFormatOpts = (infoFormatOpts i) {fOptsVHelpColumn=n}}

-- | Maximum number of columns the help of a variable should fit in before jumping to the next line (defaults to 55)
varHelpMaxColumns :: Int -> Info e -> Info e
varHelpMaxColumns n i = i {infoFormatOpts = (infoFormatOpts i) {fOptsVHelpMaxColumns=n}}

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
