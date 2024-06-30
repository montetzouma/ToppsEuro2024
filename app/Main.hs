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

  let updatedCatalogue = CP.markGot got catalogue

  mapM_ (findAndWrite updatedCatalogue) [True, False]


findAndWrite :: StickerCollection -> Bool -> IO ()
findAndWrite catalogue careAboutParallels = do 
  let needs              = CP.findNeeds      catalogue careAboutParallels
      duplicates         = CP.findDuplicates catalogue careAboutParallels
      filePathNeed       = if careAboutParallels then FP.needPathWithParallels       else FP.needPathWithoutParallels 
      filePathDuplicates = if careAboutParallels then FP.duplicatesPathWithParallels else FP.duplicatesPathWithoutParallels

  writeFile filePathNeed       (show needs)
  writeFile filePathDuplicates (show duplicates)