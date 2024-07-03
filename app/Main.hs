module Main (main) where

import DataTypes

import qualified CollectionProcessors as CP
import qualified FilePaths            as FP
import qualified Parsers              as P


-- TODO: Shouldn't need to have a newline at the end of the Got file.
main :: IO ()
main = do
  catalogue <- P.parseCatalogue
  got       <- P.parseGot

  mapM_ (findAndWrite catalogue got) [True, False]


findAndWrite :: [Sticker] -> [Sticker] -> Bool -> IO ()
findAndWrite catalogue got careAboutParallels = do 
  let collection         = CP.createCollection catalogue careAboutParallels
      updatedCollection  = CP.markGot collection got careAboutParallels
      needs              = CP.findNeeds      updatedCollection
      duplicates         = CP.findDuplicates updatedCollection
      filePathNeed       = if careAboutParallels then FP.needPathWithParallels       else FP.needPathWithoutParallels 
      filePathDuplicates = if careAboutParallels then FP.duplicatesPathWithParallels else FP.duplicatesPathWithoutParallels

  writeFile filePathNeed       (show needs)
  writeFile filePathDuplicates (show duplicates)