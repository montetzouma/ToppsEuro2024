module Main (main) where

import qualified CollectionProcessors as CP
import qualified Parsers              as P


-- TODO: Print umlauts properly.
-- TODO: Try reducing code duplication (With/without parallels)
-- TODO: Supress do-bind warnings
-- TODO: Shouldn't need to have a newline at the end of the Got file.
main :: IO ()
main = do
  catalogue <- P.parseCatalogue
  got       <- P.parseGot

  let updatedCatalogue = CP.markGot got catalogue
      needs            = CP.findNeeds      updatedCatalogue
      duplicates       = CP.findDuplicates updatedCatalogue

  CP.writeNeeds      needs
  CP.writeDuplicates duplicates