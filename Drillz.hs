{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Drillz where

import Config
import Control.Exception
import Data.Map (Map)
import Data.Text (Text)
import Data.Traversable
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Text as T

newtype Drills = Drills (Map Text [Drills]) deriving (Eq, Ord, Read, Show)

drillsToConfig :: Drills -> Value ()
drillsToConfig (Drills drills) = List () (go <$> M.toList drills) where
	go (nm, ds) = List () $ Text () nm : map drillsToConfig ds

data DrillParseError a
	= ExpectedProgressionName (Value a)
	| ExpectedProgression (Value a)
	| DuplicateAlternative a Text
	| ExpectedAlternatives (Value a)
	deriving (Eq, Read, Show, Typeable)

configToDrills :: Value a -> Either (DrillParseError a) Drills
configToDrills (List ann alts) = do
	pairs <- for alts $ \case
		List _ (Text _ nm : progression) -> (,) nm <$> traverse configToDrills progression
		List _ (v : _) -> Left (ExpectedProgressionName v)
		v -> Left (ExpectedProgression v)
	v <- sequence $ M.fromListWithKey (\k _ _ -> Left (DuplicateAlternative ann k)) (fmap (fmap pure) pairs)
	pure (Drills v)
configToDrills other = Left (ExpectedAlternatives other)

describeValue :: Value a -> String
describeValue Sections{} = "list of sections"
describeValue Number{} = "number"
describeValue Text{} = "string"
describeValue Atom{} = "bare atom" -- "atom" would be okay but starts with a vowel, which makes the "a atom" formation below a bit awkward
describeValue List{} = "list"

instance a ~ Position => Exception (DrillParseError a) where
	displayException (ExpectedProgressionName v) = displayException . ParseError (valueAnn v)
		$ "Expected a progression name (as a string), found a " ++ describeValue v ++ " instead."
	displayException (ExpectedProgression v) = displayException . ParseError (valueAnn v)
		$ "Expected a difficulty progression (as a non-empty list), found a" ++ desc ++ " instead." where
		desc = case v of List _ [] -> "n empty list"; _ -> ' ' : describeValue v
	displayException (DuplicateAlternative pos alt) = displayException . ParseError pos
		$ "Two alternative progressions with the same description (" ++ T.unpack alt ++ ")."
	displayException (ExpectedAlternatives v) = displayException . ParseError (valueAnn v)
		$ "Expected a collection of equally difficult alternatives (as sections), found a " ++ describeValue v ++ " instead."

parseIO :: Exception e => (Value Position -> Either e a) -> Text -> IO a
parseIO f t = case parse t of
	Left e -> throwIO e
	Right v -> case f v of
		Left e -> throwIO e
		Right a -> pure (const a ())
