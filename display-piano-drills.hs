{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Config
import Data.List
import Data.Text (Text)
import Drillz
import Text.Printf
import qualified Data.Text as T

idS :: String -> String
idS = id

idT :: Text -> Text
idT = id

octaveWise :: String -> [Int] -> [Int] -> (Text, [Drills])
octaveWise description speeds offsets = (,) (T.pack description) $
	[ alternatives
		[ T.pack $ printf "%s, %s, %d octaves/m" description alt speed
		| (speed, alts) <- zip (reverse $ take depth speedsRep) exercises
		, alt <- alts
		]
	| depth <- [1..length exercises + length speeds - 1]
	]
	where
	speedsRep = speeds ++ repeat (last speeds)
	motion = printf "%s motion, %s hand %d notes ahead, 3 octaves"

	synchronizedExercises = tail [undefined :: [String]
		, [ printf "%s hand only, %s, 4 octaves" hand dir
		  | hand <- hands
		  , dir <- [idS"ascending", "descending"]
		  ]
		, ["contrary motion, 3 octaves"]
		, tail [undefined
			, "parallel motion, ascending, 4 octaves"
			, "parallel motion, descending, 4 octaves"
			]
		, tail [undefined
			, "parallel motion, ascending then descending, 4 octaves"
			, "parallel motion, descending then ascending, 4 octaves"
			]
		]
	[offsetContraryExercises, offsetParallelExercises] =
		[	[[motion parity hand offset | hand <- hands]
		 	| offset <- offsets
		 	]
		| parity <- [idS"contrary", "parallel"]
		]
	exercises = map concat . transpose $ tail [undefined
		, synchronizedExercises
		, []:[]:offsetContraryExercises
		, []:[]:[]:offsetParallelExercises
		]

scale :: Text -> Text -> (Text, [Drills])
scale key mode = octaveWise
	(printf "%s %s scale" key mode)
	[40, 50, 60, 65, 70, 72, 73, 74, 75]
	[4, 2, 1]

data ChordType = Major | Minor | Major7 | Minor7 | MajorMinor7
	deriving (Eq, Ord, Read, Show, Bounded, Enum)

arpeggio :: Text -> ChordType -> (Text, [Drills])
arpeggio key chord = octaveWise
	(printf "%s %s arpeggio" key description)
	[-1] -- TODO: sit at a piano and work out what some sensible arpeggio speeds are
	offsets
	where
	triadOffsets = [2,1]
	quadOffsets = [3,2,1]
	offsets = case chord of
		Major -> triadOffsets
		Minor -> triadOffsets
		_ -> quadOffsets

	description = idS$case chord of
		Major -> "major"
		Minor -> "minor"
		Major7 -> "major seventh"
		Minor7 -> "minor seventh"
		MajorMinor7 -> "major-minor seventh"

classical :: Text -> (Text, [Drills])
classical name = ("classical-style study of " <> name, tail [undefined
	, drill $ "listen to a performance of " <> name
	, drill $ "sight read " <> name
	, alternatives
		[ "sight read the " <> hand <> " hand of " <> name
		| hand <- hands
		]
	, drill $ "melodic analysis: " <> name
	, drill $ "functional analysis: " <> name
	, drill $ "structural analysis: " <> name
	, drill "make some drills in <classical> for practicing ever-larger chunks of a piece and for memorization"
	])

transcribe :: Text -> (Text, [Drills])
transcribe name = ("transcribe " <> name, tail [undefined
	, drill $ "transcribe the melody notes of " <> name
	, drill $ "transcribe the chord changes for " <> name
	, drill $ "transcribe the bass line notes of " <> name
	, drill $ name <> " transcription cleanup: get the rhythms, time signature, and measure breaks right"
	, drill $ "flesh out some internal harmonization for " <> name
	, drill "make some drills in <transcribe> for practicing ever-larger chunks of a piece and for memorization"
	])

keys :: [Text]
keys = ["D♭", "A♭", "E♭", "B♭", "F", "C", "G", "D", "A", "E", "B", "F#"]

hands :: [Text]
hands = ["left", "right"]

allDrills :: Drills
allDrills = drills $ tail [undefined
	, ("scales", [drills
		[ scale key mode
		| key <- keys
		, mode <- ["major", "natural minor", "harmonic minor", "melodic minor"]
		]])
	, ("arpeggios", [drills
		[ arpeggio key chord
		| key <- keys
		, chord <- [minBound .. maxBound]
		]])
	, ("classical-style study", [drills . map classical $ tail [undefined
		, "Ondine by Ravel"
		, "Arabesque No. 1 by Debussy"
		, "La Fille aux Cheveux de Lin by Debussy"
		]])
	, ("transcription practice", [drills . map transcribe $ tail [undefined
		, "the Deep Space 9 theme"
		]])
	, ("TODO", [drill "make up some drills for classical-style study of sequences of pieces, then add the Well-Tempered Klavier, Goldberg Variations, and Suite Bergamesque"])
	]

main :: IO ()
main = print . pretty . drillsToConfig $ allDrills
