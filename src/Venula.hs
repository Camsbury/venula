--------------------------------------------------------------------------------
-- |
-- Module: Venula
-- Description: Describe your state declaratively
-- Maintainers: Cameron Kingsbury <camsbury7@gmail.com>
-- Maturity: Draft
--
--
--------------------------------------------------------------------------------
module Venula
  ( Venula(..)
  ) where
--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------
import Hedgehog
--------------------------------------------------------------------------------
import qualified Hedgehog.Gen as Gen
--------------------------------------------------------------------------------
-- Venula Typeclass


-- | Define how to convert state into a 'Venula' state rep
class (Eq stateRep, Show stateRep) =>
  Venula stateRep where
    -- | Extract an in-memory representation of state
    toStateRep :: IO stateRep
    -- | Create state from the given stateRep
    fromStateRep :: stateRep -> IO ()



--------------------------------------------------------------------------------
-- Provided functions


-- | Check that the methods of 'Venula' do what they are meant to for the given
-- stateRep. Basically just checks that a generated representation of state
-- round trips using the 'Venula' methods.
checkStateRep
  :: Venula stateRep
  => Gen stateRep
  -> Property
checkStateRep genStateRep = property $ do
  sampleStateRep <- Gen.sample genStateRep
  calculatedStateRep <- lift $ do
    fromStateRep sampleStateRep
    toStateRep
  calculatedStateRep === sampleStateRep


-- | Define a way to test any function on state with an associated 'Venula'.
-- This takes the function itself, the initial representation of state,
-- and the representation of state corresponding to the expected result of
-- the function call.
--
-- Returns if the calculated state representation matches the expected result.
testFunctionOnState
  :: Venula stateRep
  => IO ()
  -> stateRep
  -> stateRep
  -> Property
testFunctionOnState functionOnState initialStateRep finalStateRep = property $ do
  calculatedStateRep <- lift $ do
    fromStateRep initialStateRep
    functionOnState
    toStateRep
  calculatedStateRep === finalStateRep

-- | Define a way to test any function on state with an associated 'Venula'.
-- This takes the function itself as well as a generator for pairs of
-- state representation, pre and post function call.
--
-- Returns if the calculated state representation matches the expected result.
testFunctionOnGeneratedState
  :: Venula stateRep
  => IO ()
  -> Gen (stateRep, stateRep)
  -> Property
testFunctionOnGeneratedState functionOnState genStateRepPair = property $ do
  (initialStateRep, finalStateRep) <- Gen.sample genStateRepPair
  calculatedStateRep <- lift $ do
    fromStateRep initialStateRep
    functionOnState
    toStateRep
  calculatedStateRep === finalStateRep
