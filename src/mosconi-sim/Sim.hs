module Sim where
import System.Random (RandomGen)
import Match (Team)

data MosconiTeam = USA Team | Europe Team deriving (Eq, Show)

-- this would be an ADT from the other module, so i can just put all the
-- matchups in a list and `go` until I have a winner
data Matchup

pickWinner :: RandomGen g => g -> Matchup -> MosconiTeam
pickWinner gen matchup = undefined
