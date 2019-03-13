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
  ( module Venula
  ) where
--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------
import Control.Lens.Operators
--------------------------------------------------------------------------------
import qualified Data.UUID     as UUID
import qualified Data.UUID.V4  as UUID
--------------------------------------------------------------------------------
import Data.UUID (UUID)
--------------------------------------------------------------------------------
import Control.Lens (makeFieldsNoPrefix, Iso')
--------------------------------------------------------------------------------
-- Graph Structure

-- | Represents a scalar
data VertexScalar
  = VertexInteger Integer
  | VertexFloat   Double
  | VertexText    Text
  | VertexBool    Bool
  deriving (Eq, Show)

-- | Represents an application-specific value
data Vertex stateType = Vertex
  { _vertexType  :: stateType
  , _vertexValue :: VertexScalar
  }
deriving instance Eq stateType => Eq (Vertex stateType)
deriving instance Show stateType => Show (Vertex stateType)

-- | Represents an application's state
data Graph stateType = Graph
  { _edges    :: HashMap UUID UUID
  , _vertices :: HashMap UUID (Vertex stateType)
  }
deriving instance Eq stateType => Eq (Graph stateType)
deriving instance Show stateType => Show (Graph stateType)


-- | Cardinality constraints on edges
data Cardinality stateType
  = MaybeOne stateType
  | One      stateType
  | Many     stateType
deriving instance Eq stateType => Eq (Cardinality stateType)
deriving instance Show stateType => Show (Cardinality stateType)

-- | Constraints placed values in the application
data Constraint stateType
  = Unique          stateType
  | ToCardinality   stateType (Cardinality stateType)
  | FromCardinality stateType (Cardinality stateType)
deriving instance Eq stateType => Eq (Constraint stateType)
deriving instance Show stateType => Show (Constraint stateType)


--------------------------------------------------------------------------------
-- Venula Typeclass

-- | Define how to convert state from its types and representation
-- into a "Venula" 'Graph'
class (Eq stateType, Show stateType) =>
  Venula stateType stateRep | stateRep -> stateType where
    venularPairs :: Iso' stateRep [(Vertex stateType, Vertex stateType)]

-- | Construct the graph from the provided type
-- Opts to maintain the specified constraints over
-- preserving passed pairs -> starting from tail
constructGraph
  :: forall stateType stateRep
  . (Venula stateType stateRep)
  => stateRep
  -> HashSet (Constraint stateType)
  -> Graph stateType
constructGraph sRep constraints
  = foldr (mergePair constraints) (Graph mempty mempty) $ sRep ^. venularPairs
  where
    mergePair
      :: HashSet (Constraint stateType)
      -> (Vertex stateType, Vertex stateType)
      -> Graph stateType
      -> Graph stateType
    mergePair = undefined
