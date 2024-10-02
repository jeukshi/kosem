module Database.Kosem.PostgreSQL.Internal.P where

import Data.Coerce (coerce)
import Data.Text (Text)
import Text.Megaparsec (PosState (..))
import Text.Megaparsec.Pos (SourcePos (..), mkPos)

initPosState :: String -> PosState String
initPosState input =
    PosState
        { pstateInput = input
        , pstateOffset = 0
        , pstateSourcePos =
            SourcePos
                { sourceName = ""
                , sourceLine = mkPos 1
                , sourceColumn = mkPos 1
                }
        , pstateTabWidth = mkPos 1
        , pstateLinePrefix = ""
        }

-- TODO explain P
newtype P = MkP {unP :: Int}
    deriving (Show, Eq, Ord)

movePby :: P -> Int -> P
movePby p int = coerce $ coerce p + int
