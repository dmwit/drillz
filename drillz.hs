import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Data.Maybe
import Paths_drillz
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Random
import Text.Printf
import qualified Data.Map as M

newtype Drills = Drills (Map String [Drills]) deriving (Eq, Ord, Read, Show)

drills :: [(String, [Drills])] -> Drills
drills = Drills . M.fromListWith (\_ _ -> error "duplicate key")

drill :: String -> Drills
drill description = drills [(description, [])]

alternatives :: [String] -> Drills
alternatives descriptions = drills [(description, []) | description <- descriptions]

newtype Progress = Progress (Map String (Int, Progress)) deriving (Eq, Ord, Read, Show)

noProgress :: Progress
noProgress = Progress M.empty

getProgress :: String -> Progress -> (Int, Progress)
getProgress nm (Progress progress) = M.findWithDefault (0, noProgress) nm progress

setProgress :: String -> Int -> Progress -> Progress -> Progress
setProgress nm n p (Progress progress) = Progress (M.insert nm (n, p) progress)

data Drill = Drill
	{ task :: String
	, path :: [(Int, String)] -- TODO: [(String, Int)] makes more sense
	} deriving (Eq, Ord, Read, Show)

selectDrill :: Drills -> Progress -> IO Drill
selectDrill = go [] where
	go as (Drills drills) progress = do
		n <- randomRIO (0, M.size drills-1)
		(name, progression) <- evaluate (M.elemAt n drills)
		case length progression of
			0 -> pure Drill
				{ task = name
				, path = reverse as
				}
			n -> go ((ix, name):as) drills' progress'
				where
				(ix, progress') = getProgress name progress
				drills' = head (drop ix progression ++ [last progression])

levels :: String -> Int -> (String, [Drills])
levels description n = (description, [maxLevel i | i <- [1..n]]) where
	maxLevel i = alternatives $
		[ printf "%s levels %d-%d" description j (j+4)
		| j <- [1, 6 .. i-9]
		] ++
		[ printf "%s levels 1-%d" description (i-1) | i > 2 && i < 6 ] ++
		[ printf "%s levels %d-%d" description lo ((lo+i-1)`div`2) | i >= 6 ] ++
		[ printf "%s levels %d-%d" description ((lo+i+1)`div`2) (i-1) | i >= 6 ] ++
		[ printf "%s level %d" description i ]
		where lo = i-4 - i `mod` 5

allDrills :: Drills
allDrills = drills $ tail [undefined
	, ("hover", tail [undefined
		, drill "hover in place"
		, drill "hover in place low to the ground"
		, alternatives $ tail [undefined
			, "hover low with bottom of car to right of screen"
			, "hover low with bottom of car to left of screen"
			]
		, drill "hover low with car backwards"
		, drills $ tail [undefined
			, ("hover without air roll", tail [undefined
				, drill "hover without air roll"
				, drill "hover without air roll low to the ground"
				, alternatives $ tail [undefined
					, "hover without air roll with bottom of car to right of screen"
					, "hover without air roll with bottom of car to left of screen"
					]
				, drill "hover without air roll with car backwards"
				])
			]
		, alternatives $ tail [undefined
			, "hover while air rolling clockwise"
			, "hover while air rolling counterclockwise"
			]
		, alternatives $ tail [undefined
			, "hover while air rolling clockwise low to the ground"
			, "hover while air rolling counterclockwise low to the ground"
			]
		])
	, ("figure eight", tail [undefined
		, drill "fly in figure eights on Pillars four times"
		, drill "fly in figure eights on Pillars four times with no air roll"
		, alternatives $ tail [undefined
			, "fly in figure eights on Pillars four times continually air rolling clockwise"
			, "fly in figure eights on Pillars four times continually air rolling conterclockwise"
			]
		, drill "fly in figure eights on Pillars four times continually boosting"
		])
	, ("bounce dribble", tail [undefined
		, alternatives $ tail [undefined
			, "dribble on the ground around the center circle turning right"
			, "dribble on the ground around the center circle turning left"
			]
		, alternatives $ tail [undefined
			, "dribble on the ground around the center circle turning right at boost-needed speed"
			, "dribble on the ground around the center circle turning left at boost-needed speed"
			]
		, alternatives $ tail [undefined
			, "bounce dribble circles turning right"
			, "bounce dribble circles turning left"
			, "bounce dribble zig zags"
			]
		])
	, ("hit the ball gently, then drive alongside it matching its speed until it stops", [])
	, ("speed jump: rings 3 - by dmc by dmc", tail [undefined
		, drill "speed jump: rings 3 - by dmc by dmc"
		, drill "speed jump: rings 3 - by dmc by dmc in <10 minutes"
		, drill "speed jump: rings 3 - by dmc by dmc with no air roll"
		, drill "speed jump: rings 3 - by dmc by dmc with no air roll in <10 minutes"
		, alternatives $ tail [undefined
			, "speed jump: rings 3 - by dmc by dmc continually air rolling clockwise"
			, "speed jump: rings 3 - by dmc by dmc continually air rolling counterclockwise"
			]
		, alternatives $ tail [undefined
			, "speed jump: rings 3 - by dmc by dmc continually air rolling clockwise in <10 minutes"
			, "speed jump: rings 3 - by dmc by dmc continually air rolling counterclockwise in <10 minutes"
			]
		, drill "speed jump: rings 3 - by dmc by dmc continually boosting"
		])
	, (++[drill "make up some drills for learning flicks"]) <$> levels "dribble 2 overhaul by 0xDigby" 30
	, levels "obstacle course #2" 30
	, levels "the wall" 8
	, levels "dribble training by virge" 8
	, levels "air dribbles (wall & ground) by iowa" 8
	, ("Hannah's playground", tail [undefined
		, drill "fly around the rings on Hannah's playground three times"
		, alternatives $ tail [undefined
			, "fly around the rings on Hannah's playground three times with no air roll"
			, "fly around the outer circle of the free play field three times"
			]
		, alternatives $ tail [undefined
			, "fly around the rings on Hannah's playground three times continually air rolling clockwise"
			, "fly around the rings on Hannah's playground three times continually air rolling counterclockwise"
			, "fly around the outer circle of the free play field three times with no air roll"
			, "fly around the inner circle of the free play field three times"
			]
		, alternatives $ tail [undefined
			, "fly around the rings on Hannah's playground three times continually boosting"
			, "fly around the outer circle of the free play field three times continually air rolling clockwise"
			, "fly around the outer circle of the free play field three times continually air rolling clockwise"
			, "fly around the inner circle of the free play field three times with no air roll"
			]
		, alternatives $ tail [undefined
			, "fly around the outer circle of the free play field three times continually boosting"
			, "fly around the inner circle of the free play field three times continually air rolling clockwise"
			, "fly around the inner circle of the free play field three times continually air rolling counterclockwise"
			]
		, alternatives $ tail [undefined
			, "fly around the rings on Hannah's playground three times continually boosting"
			, "fly around the outer circle of the free play field three times continually boosting"
			, "fly around the inner circle of the free play field three times continually boosting"
			]
		])
	, ("bumps and demos", tail [undefined
		, drill "go for bumps and demos before rotating back in a 1v3"
		, drill "go for bumps and demos before rotating back in a 1v4"
		, drill "go for bumps and demos before rotating back in a 3v3"
		])
	, ("1v1", [])
	, ("passing", tail [undefined
		, drill "make 1 intentional pass in a 3v3"
		, drill "make 2 intentional passes in a 3v3"
		, drill "make 3 intentional passes in a 3v3"
		, drill "make 4 intentional passes in a 3v3"
		, drill "make 5 intentional passes in a 3v3"
		])
	, ("think about your intention on each hit in a 3v3, then review the replay and think about possible intentions if there were any where you didn't do that", [])
	, ("aerial training", tail [undefined
		, drills [levels "rookie aerial training" 10]
		, drills [levels "pro aerial training" 10]
		, drills [levels "all-star aerial training" 10]
		])
	, ("aerial training, completed via catch+dribble", tail [undefined
		, drills [levels "pro aerial training, completed via catch and dribble" 10]
		, drills [levels "all-star aerial training, completed via catch and dribble" 10]
		])
	, ("aerial training, completed via powershot", tail [undefined
		, drills [levels "rookie aerial training, completed via powershot" 10]
		, drills [levels "pro aerial training, completed via powershot" 10]
		, drills [levels "all-star aerial training, completed via powershot" 10]
		])
	, ("play a 3v3 and focus on making a good touch without controlling the ball first", [])
	, levels "air roll shots by nicknac63" 10
	, levels "shooting consistency by wayprotein | a&m" 20
	, levels "shooting consistency by wayprotein | a&m, completed with air roll shots" 20
	, levels "basic wall defence by j." 3
	, ("play a 3v3 and focus on knowing where your teammates are at all times", [])
	, ("play a 3v3 and focus on knowing how much boost your teammates have", [])
	, ("half flips", tail [undefined
		, drill "backflip cancel"
		, alternatives $ tail [undefined
			, "front-flip cancel"
			, "backflip cancel, then air roll clockwise 180°"
			, "backflip cancel, then air roll counterclockwise 180°"
			]
		, alternatives $ tail [undefined
			, "diagonal back-right flip cancel"
			, "diagonal back-left flip cancel"
			, "front-flip cancel, then air roll clockwise 180°"
			, "front-flip cancel, then air roll counterclockwise 180°"
			, "while driving backward, backflip cancel, then air roll clockwise 180°"
			, "while driving backward, backflip cancel, then air roll counterclockwise 180°"
			]
		, alternatives $ tail [undefined
			, "diagonal front-right flip cancel"
			, "diagonal front-left flip cancel"
			, "while driving backward, diagonal back-right flip cancel"
			, "while driving backward, diagonal back-left flip cancel"
			, "while driving forward, front-flip cancel, then air roll clockwise 180°"
			, "while driving forward, front-flip cancel, then air roll counterclockwise 180°"
			, "while driving backward, boost and backflip cancel, then air roll clockwise 180°"
			, "while driving backward, boost and backflip cancel, then air roll counterclockwise 180°"
			]
		, alternatives $ tail [undefined
			, "while driving forward, diagonal front-right flip cancel"
			, "while driving forward, diagonal front-left flip cancel"
			, "while driving backward, boost and diagonal back-right flip cancel"
			, "while driving backward, boost and diagonal back-left flip cancel"
			, "while driving forward, boost and front-flip cancel, then air roll clockwise 180°"
			, "while driving forward, boost and front-flip cancel, then air roll counterclockwise 180°"
			, "backward half-flips in a 1v3"
			]
		, alternatives $ tail [undefined
			, "while driving forward, boost and diagonal front-right flip cancel"
			, "while driving forward, boost and diagonal front-left flip cancel"
			, "diagonal back-left half flips in a 1v3"
			, "diagonal back-right half flips in a 1v3"
			, "front half flips in a 1v3"
			, "backward half-flips in a 1v4"
			]
		, alternatives $ tail [undefined
			, "diagonal front-right half flips in a 1v3"
			, "diagonal front-left half flips in a 1v3"
			, "diagonal back-left half flips in a 1v4"
			, "diagonal back-right half flips in a 1v4"
			, "front half flips in a 1v4"
			, "backward half-flips in a 3v3"
			]
		, alternatives $ tail [undefined
			, "diagonal front-right half flips in a 1v4"
			, "diagonal front-left half flips in a 1v4"
			, "diagonal back-left half flips in a 3v3"
			, "diagonal back-right half flips in a 3v3"
			, "front half flips in a 3v3"
			, "backward half-flips in a 3v3"
			]
		, alternatives $ tail [undefined
			, "diagonal front-right half flips in a 3v3"
			, "diagonal front-left half flips in a 3v3"
			, "backward, diagonal backward, and front half flips in a 3v3"
			]
		, drill "half flips in a 3v3"
		])
	, ("backwards aerials", tail [undefined
		, alternatives $ tail [undefined
			, "in free play, drive up a corner, then fly backwards to the other corner"
			, "in free play, launch up the wall of the goal, then fly backwards to the top of the other goal"
			, "in free play, launch out from the goal ceiling, then fly backwards to the top of the other goal"
			]
		, drill "in free play, launch from a top corner of a goal, roll 180°, then fly diagonally across the map to the other top corner of the other goal"
		, drill "in free play, start an aerial from the ground, then fly backwards across the map"
		, drills [levels "rookie aerial training, completed via backwards aerial" 10]
		, drills [levels "pro aerial training, completed via backwards aerial" 10]
		, drills [levels "all-star aerial training, completed via backwards aerial" 10]
		])
	, ("fast aerial timing", tail [undefined
		, drill "pitch up, boost, and hold jump"
		, drill "set metronome to 250bpm; pitch up, boost, and hold jump on one click, then stop pitching up briefly on the next click"
		, drill "set metronome to 250bpm; pitch up, boost, and hold jump on one click, then stop pitching up briefly and tap jump on next click; hold jump as long as possible between the two clicks"
		, drill "fast aerial in 1v3 (250bpm between jumps, holding boost and pitching up)"
		, drill "fast aerial in 1v4 (250bpm between jumps, holding boost and pitching up)"
		, drill "fast aerial in 3v3 (250bpm between jumps, holding boost and pitching up)"
		])
	, ("low-boost fast aerial timing", tail [undefined
		, drill "pitch up and hold jump+accelerate"
		, drill "set metronome to 250bpm; pitch up and hold jump on one click, then tap jump on next click; hold jump as long as possible between the two clicks and accelerate the whole time"
		, drill "set metronome to 250bpm; on successive clicks, pitch up and hold jump, then tap jump, then tap boost; hold jump as long as possible between the first two clicks and accelerate the whole time"
		, drill "low-boost fast aerial in 1v3 (at 250bpm, jump; jump; tap boost all while pitching up and accelerating)"
		, drill "low-boost fast aerial in 1v4 (at 250bpm, jump; jump; tap boost all while pitching up and accelerating)"
		, drill "low-boost fast aerial in 3v3 (at 250bpm, jump; jump; tap boost all while pitching up and accelerating)"
		])
	, ("no-boost fast aerial timing", tail [undefined
		, drill "hold jump+accelerate and pitch up slightly -- 7° or less"
		, drill "set metronome to 250bpm; hold jump and pitch up 7° or less on one click, then tap jump on next click; hold jump as long as possible between the two clicks and accelerate the whole time"
		, drill "no-boost fast aerial in 1v3 (250bpm between jumps, pitch up 7° before second jump, always accelerate)"
		, drill "no-boost fast aerial in 1v4 (250bpm between jumps, pitch up 7° before second jump, always accelerate)"
		, drill "no-boost fast aerial in 3v3 (250bpm between jumps, pitch up 7° before second jump, always accelerate)"
		])
	, levels "practice fast aerials in aerial shots - pass by poquito" 48
	, levels "practice fast aerials in aerial shots - redirects by poquito" 40
	, levels "practice fast aerials in double jump aerials by doomsee" 15
	, ("aerial shot training", tail [undefined
		, drills [levels "novice aerials by wayprotein" 18]
		, drills [levels "jump aerials by doomsee" 15]
		, drills [levels "aerial off-wall by wheelchair {lflegs}" 15]
		])
	, ("speed jump: rings 1 - by dmc by dmc", tail [undefined
		, drill "speed jump: rings 1 - by dmc by dmc, start to giant ring"
		, drill "speed jump: rings 1 - by dmc by dmc, giant ring to first tube"
		, alternatives $ tail [undefined
			, "speed jump: rings 1 - by dmc by dmc, start to first tube"
			, "speed jump: rings 1 - by dmc by dmc, first tube to second tube"
			]
		, alternatives $ tail [undefined
			, "speed jump: rings 1 - by dmc by dmc, start to first tube"
			, "speed jump: rings 1 - by dmc by dmc, first tube to second tube"
			, "speed jump: rings 1 - by dmc by dmc, second tube to blimp"
			]
		, alternatives $ tail [undefined
			, "speed jump: rings 1 - by dmc by dmc, start to first tube"
			, "speed jump: rings 1 - by dmc by dmc, first tube to blimp"
			, "speed jump: rings 1 - by dmc by dmc, blimp to bottom of rivet"
			]
		, alternatives $ tail [undefined
			, "speed jump: rings 1 - by dmc by dmc, start to second tube"
			, "speed jump: rings 1 - by dmc by dmc, second tube to bottom of rivet"
			, "speed jump: rings 1 - by dmc by dmc, bottom of rivet to end"
			]
		, alternatives $ tail [undefined
			, "speed jump: rings 1 - by dmc by dmc, start to second tube"
			, "speed jump: rings 1 - by dmc by dmc, second tube to end"
			]
		, drill "speed jump: rings 1 - by dmc by dmc"
		, drill "make up some harder drills than just finishing speed jump: rings 1 - by dmc by dmc"
		])
	, ("Lethamyr's rings", tail [undefined
		, drill "Lethamyr's giant rings map by Leth"
		, drill "Lethamyr's tiny rings map by snowflake"
		, drill "make up some harder drills than just finishing Lethamyr's tiny rings map by snowflake"
		])
	, ("speed jump: rings 2 - by dmc by dmc", [])
	, ("turtle exhibition match to practice upside-down car control", tail [undefined
		, drill "turtle exhibition match: 1v1 against beginner bot (unlimited boost)"
		, drill "turtle exhibition match: 1v1 against rookie bot (unlimited boost)"
		, drill "turtle exhibition match: 1v1 against pro bot (unlimited boost)"
		, drill "turtle exhibition match: 1v1 against all-star bat (unlimited boost)"
		, drill "turtle exhibition match: 1v2 (unlimited boost)"
		, drill "turtle exhibition match: 1v3 (unlimited boost)"
		, drill "turtle exhibition match: 1v4 (unlimited boost)"
		])
	]

finished :: Drills -> Progress -> Bool
finished (Drills drills) (Progress progress) = M.isSubmapOfBy go drills progress where
	go ds (n, p) = n >= length ds

makeProgressOn :: Drill -> Drills -> Progress -> Progress
makeProgressOn d = go (path d) where
	go [] (Drills drills) (Progress progress) = Progress (M.insertWith combineProgress (task d) (1, noProgress) progress)
	go ((p, nm):as) (Drills drills) progress = fromMaybe progress $ do
		d <- M.lookup nm drills
		drills' <- listToMaybe (drop p d)
		let (_, childProgress) = getProgress nm progress
		    childProgress' = go as drills' childProgress
		    progress' = if finished drills' childProgress'
		    	then setProgress nm (p+1) noProgress progress
		    	else setProgress nm p childProgress' progress
		pure progress'

	combineProgress (n, p) (n', p') = (n+n', p)

offerProgress :: Drill -> Drills -> Progress -> Bool
offerProgress d = go (path d) where
	go ((p, nm):as) (Drills drills) progress = case M.lookup nm drills of
		_ | p < p' -> False
		Nothing -> error "internal error: selected a drill that doesn't exist??"
		Just ds -> case drop p ds of
			[] -> False
			d:_ -> go as d progress'
		where (p', progress') = getProgress nm progress
	go [] (Drills drills) progress = fst (getProgress (task d) progress) == 0

loopTime :: Int
loopTime = 10*60*1000*1000

selectLoop :: Drill -> Progress -> IO Drill
selectLoop d p = do
	d' <- selectDrill allDrills p
	if d == d'
		then selectLoop d p
		else pure d'

defaultProgress :: IOException -> IO Progress
defaultProgress _ = pure noProgress

drillThread :: IORef (Maybe Drill) -> MVar Progress -> Drill -> IO a
drillThread dRef pMVar d0 = do
	mp3 <- getDataFileName "drillz.mp3"
	loop mp3 d0
	where
	loop mp3 d = do
		putStrLn (task d)
		threadDelay loopTime
		p <- takeMVar pMVar
		if offerProgress d allDrills p
			then putStrLn "if that was easy, press enter" >> writeIORef dRef (Just d)
			else writeIORef dRef Nothing
		putMVar pMVar p
		forkIO (() <$ readProcess "mpv" ["--really-quiet", mp3] "")
		selectLoop d p >>= loop mp3

progressThread :: FilePath -> IORef (Maybe Drill) -> MVar Progress -> IO a
progressThread pFilename dRef pMVar = forever $ do
	getLine
	p <- takeMVar pMVar
	md <- readIORef dRef
	p' <- case md of
		Nothing -> do
			putStrLn $ "WARNING: no progress made\nthe previous drill was already marked easy (or there was no previous drill)"
			pure p
		Just d -> do
			putStrLn $ "making progress on " ++ task d
			let p' = makeProgressOn d allDrills p
			p' <$ writeFile pFilename (show p')
	putMVar pMVar p'

defaultProfile :: String
defaultProfile = "default"

drillsFilename :: String -> IO FilePath
drillsFilename profile = getUserDataDir ("drillz" </> "drills" </> profile)

progressFilename :: String -> IO FilePath
progressFilename profile = getUserConfigDir ("drillz" </> "progress" </> profile)

usage :: Int -> IO a
usage n = do
	exe <- getProgName
	d <- drillsFilename "PROFILE"
	p <- progressFilename "PROFILE"
	out "USAGE: %s [PROFILE | -p PROFILE]" exe
	out "Randomly cycle between drills in an exercise regimen, gradually making the exercises harder."
	out ""
	out "The regimen is stored in %s." d
	out "The record of how difficult to make each exercise is stored in %s." p
	out "The default PROFILE is %s." (show defaultProfile)
	exitWith (if n == 0 then ExitSuccess else ExitFailure n)
	where
	out :: HPrintfType r => String -> r
	out s = hPrintf (if n == 0 then stdout else stderr) (s ++ "\n")

main :: IO ()
main = do
	args <- getArgs
	profile <- case args of
		["-h"] -> usage 0
		["--help"] -> usage 0
		["-p", nm] -> pure nm
		[nm] -> pure nm
		[] -> pure defaultProfile
		_ -> usage 1
	pFilename <- progressFilename profile
	p <- catch (read <$> readFile pFilename) defaultProgress
	print p
	d <- selectDrill allDrills p
	dRef <- newIORef Nothing
	pMVar <- newMVar p
	forkIO (drillThread dRef pMVar d)
	progressThread pFilename dRef pMVar
