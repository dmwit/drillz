module Drillz where

import Data.Map (Map)

newtype Drills = Drills (Map String [Drills]) deriving (Eq, Ord, Read, Show)
