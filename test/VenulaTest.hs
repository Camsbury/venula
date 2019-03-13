{- HLINT ignore "Reduce duplication" -}
--------------------------------------------------------------------------------
module VenulaTest where
------------ -------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Venula
--------------------------------------------------------------------------------
import Control.Lens.Operators
--------------------------------------------------------------------------------
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------------------------------------------

main :: IO ()
main
  = defaultMain
  . localOption (HedgehogTestLimit $ Just 5)
  $ testGroup "Venula commands"
  [
  ]
