{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Config
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import Data.Typeable
import Drillz
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
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Progress = Progress (Map Text (Int, Progress)) deriving (Eq, Ord, Read, Show)

noProgress :: Progress
noProgress = Progress M.empty

getProgress :: Text -> Progress -> (Int, Progress)
getProgress nm (Progress progress) = M.findWithDefault (0, noProgress) nm progress

setProgress :: Text -> Int -> Progress -> Progress -> Progress
setProgress nm n p (Progress progress) = Progress (M.insert nm (n, p) progress)

progressToConfig :: Progress -> Value ()
progressToConfig (Progress progress) = List () (go <$> M.toList progress) where
	go (nm, (n, p)) = Sections ()
		[ "alternative" ~: Text () nm
		, "difficulty-level" ~: Number () (integerToNumber (toInteger n))
		, "progress" ~: progressToConfig p
		]
	s ~: v = Section () s v

data ProgressParseError a
	= ProgressExpectedIndex a Number
	| ProgressExpectedStruct (Value a)
	| ProgressDuplicateAlternative a Text
	| ProgressExpectedList (Value a)
	deriving (Eq, Read, Show, Typeable)

configToProgress :: Value a -> Either (ProgressParseError a) Progress
configToProgress (List ann alts) = do
	pairs <- for alts $ \case
		Sections _
			[ Section _ "alternative" (Text _ nm)
			, Section _ "difficulty-level" (Number nAnn num)
			, Section _ "progress" v
			] -> do
			n <- case numberToInteger num of
				Just n | 0 <= n && n <= toInteger (maxBound :: Int)
					-> pure (fromInteger n)
				_ -> Left (ProgressExpectedIndex nAnn num)
			p <- configToProgress v
			pure (nm, pure (n, p))
		v -> Left (ProgressExpectedStruct v)
	progress <- sequence $ M.fromListWithKey (\k _ _ -> Left (ProgressDuplicateAlternative ann k)) pairs
	pure (Progress progress)
configToProgress other = Left (ProgressExpectedList other)

instance a ~ Position => Exception (ProgressParseError a) where
	displayException (ProgressExpectedIndex pos n) = displayException . ParseError pos $ ""
		++ "Expected an index (a whole, positive number less than "
		++ show (maxBound :: Int)
		++ ", found " ++ show (pretty (Number () n)) ++ " instead."
	displayException (ProgressExpectedStruct v) = displayException . ParseError (valueAnn v)
		$ "Expected a section with keys alternative (with a string), difficulty-level (with a number), and progress, in that order."
	displayException (ProgressDuplicateAlternative pos alt) = displayException . ParseError pos
		$ "Two progress records for the same alternative (" ++ T.unpack alt ++ ")."
	displayException (ProgressExpectedList v) = displayException . ParseError (valueAnn v)
		$ "Expected a list of progress records, found a " ++ describeValue v ++ " instead."

data Drill = Drill
	{ task :: Text
	, path :: [(Int, Text)] -- TODO: [(Text, Int)] makes more sense
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
			_ -> go ((ix, name):as) drills' progress'
				where
				(ix, progress') = getProgress name progress
				drills' = head (drop ix progression ++ [last progression])

finished :: Drills -> Progress -> Bool
finished (Drills drills) (Progress progress) = M.isSubmapOfBy go drills progress where
	go ds (n, _) = n >= length ds

makeProgressOn :: Drill -> Drills -> Progress -> Progress
makeProgressOn d = go (path d) where
	go [] _ (Progress progress) = Progress (M.insertWith combineProgress (task d) (1, noProgress) progress)
	go ((p, nm):as) (Drills drills) progress = fromMaybe progress $ do
		ds <- M.lookup nm drills
		drills' <- listToMaybe (drop p ds)
		let (_, childProgress) = getProgress nm progress
		    childProgress' = go as drills' childProgress
		    progress' = if finished drills' childProgress'
		    	then setProgress nm (p+1) noProgress progress
		    	else setProgress nm p childProgress' progress
		pure progress'

	combineProgress (n, p) (n', _p') = (n+n', p)

offerProgress :: Drill -> Drills -> Progress -> Bool
offerProgress d = go (path d) where
	go ((p, nm):as) (Drills drills) progress = case M.lookup nm drills of
		_ | p < p' -> False
		Nothing -> error "internal error: selected a drill that doesn't exist??"
		Just ds -> case drop p ds of
			[] -> False
			alts:_ -> go as alts progress'
		where (p', progress') = getProgress nm progress
	go [] _ progress = fst (getProgress (task d) progress) == 0

loopTime :: Int
loopTime = 10*60*1000*1000

selectLoop :: Drills -> Drill -> Progress -> IO Drill
selectLoop ds d p = do
	d' <- selectDrill ds p
	if d == d'
		then selectLoop ds d p
		else pure d'

defaultProgress :: IOException -> IO Progress
defaultProgress _ = pure noProgress

drillThread :: Drills -> IORef (Maybe Drill) -> MVar Progress -> Drill -> IO a
drillThread ds dRef pMVar d0 = do
	mp3 <- getDataFileName "drillz.mp3"
	loop mp3 d0
	where
	loop mp3 d = do
		T.putStrLn (task d)
		threadDelay loopTime
		p <- takeMVar pMVar
		if offerProgress d ds p
			then putStrLn "if that was easy, press enter" >> writeIORef dRef (Just d)
			else writeIORef dRef Nothing
		putMVar pMVar p
		_ <- forkIO (() <$ readProcess "mpv" ["--really-quiet", mp3] "")
		selectLoop ds d p >>= loop mp3

progressThread :: Drills -> FilePath -> IORef (Maybe Drill) -> MVar Progress -> IO a
progressThread ds pFilename dRef pMVar = forever $ do
	_ <- getLine
	p <- takeMVar pMVar
	md <- readIORef dRef
	p' <- case md of
		Nothing -> do
			putStrLn $ "WARNING: no progress made\nthe previous drill was already marked easy (or there was no previous drill)"
			pure p
		Just d -> do
			putStr "making progress on "
			T.putStrLn $ task d
			let p' = makeProgressOn d ds p
			p' <$ (writeFile pFilename . show . pretty . progressToConfig) p'
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
	p <- catch (T.readFile pFilename >>= parseIO configToProgress) defaultProgress
	ds <- drillsFilename profile >>= T.readFile >>= parseIO configToDrills
	d <- selectDrill ds p
	dRef <- newIORef Nothing
	pMVar <- newMVar p
	_ <- forkIO (drillThread ds dRef pMVar d)
	progressThread ds pFilename dRef pMVar
