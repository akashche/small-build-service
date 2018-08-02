
module Parser
    ( parseSpecJVMOutput
    ) where

import Prelude
    (
    )
import Data.Text (Text)
import qualified Data.Vector as Vector (fromList)

import Data (SpecJVMResults(..))

parseSpecJVMOutput :: Text -> Text -> SpecJVMResults
parseSpecJVMOutput _ _ = SpecJVMResults 0 (Vector.fromList [])
