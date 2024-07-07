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

  -- Empty file contents
  writeFile FP.needPath       ""
  writeFile FP.duplicatesPath ""
 
  mapM_ (findAndWrite catalogue got) [False, True]


findAndWrite :: [Sticker] -> [Sticker] -> Bool -> IO ()
findAndWrite catalogue got careAboutParallels = do 
  let collection         = CP.createCollection catalogue careAboutParallels
      updatedCollection  = CP.markGot collection got careAboutParallels
      needs              = CP.findNeeds      updatedCollection
      duplicates         = CP.findDuplicates updatedCollection
      header             = if careAboutParallels then "INCLUDING PARALLEL VERSIONS\n" else "EXCLUDING PARALLEL VERSIONS\n" 

  appendFile FP.needPath       $ mconcat [header, show needs,      "\n\n\n"]
  appendFile FP.duplicatesPath $ mconcat [header, show duplicates, "\n\n\n"]