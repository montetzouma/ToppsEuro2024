module CollectionProcessors 
  ( findDuplicates
  , findNeeds
  , markGot
  , writeDuplicates
  , writeNeeds )
where

import DataTypes

import qualified Data.List as L
import qualified Data.Map  as Map

import qualified FilePaths as FP


markGot :: [Sticker] -> StickerCollection -> StickerCollection
markGot gotStickers StickerCollection{..} = StickerCollection
  { collectionWithParallels    = markGot' collectionWithParallels    gotStickers
  , collectionWithoutParallels = markGot' collectionWithoutParallels gotStickersWithoutParallels }
  where
    gotStickersWithoutParallels = map (\s->s{rarity=Nothing}) gotStickers

    markGot' :: Map.Map Sticker Int -> [Sticker] -> Map.Map Sticker Int
    markGot' = foldr (Map.adjust (+1))


findNeeds :: StickerCollection -> NeedInformation
findNeeds StickerCollection{..} = NeedInformation
  { needWithParallels     = needWithParallels     
  , needWithoutParallels  = needWithoutParallels  
  , nNeedWithParallels    = nNeedWithParallels    
  , nNeedWithoutParallels = nNeedWithoutParallels }
  where
    (needWithParallels   , nNeedWithParallels   ) = findNeeds' collectionWithParallels
    (needWithoutParallels, nNeedWithoutParallels) = findNeeds' collectionWithoutParallels

    findNeeds' :: Map.Map Sticker Int -> ([Sticker], Int)
    findNeeds' collection = 
      let need = Map.keys $ Map.filter (==0) collection
      in  (need, length need)

writeNeeds :: NeedInformation -> IO ()
writeNeeds NeedInformation{..} = do 
  writeNeeds' (needWithParallels,    nNeedWithParallels   ) FP.needPathWithParallels
  writeNeeds' (needWithoutParallels, nNeedWithoutParallels) FP.needPathWithoutParallels
  where
    writeNeeds' :: ([Sticker], Int) -> FilePath -> IO ()
    writeNeeds' (need, nNeed) filePath = do
      let header = mconcat ["You need ", show nNeed, " stickers.\n\n"]
          body   = L.intercalate ", " (map show need)

      writeFile filePath (header <> body)


findDuplicates :: StickerCollection -> DuplicatesInformation
findDuplicates StickerCollection{..} = DuplicatesInformation
  { duplicatesWithParallels     = duplicatesWithParallels     
  , duplicatesWithoutParallels  = duplicatesWithoutParallels  
  , nDuplicatesWithParallels    = nDuplicatesWithParallels    
  , nDuplicatesWithoutParallels = nDuplicatesWithoutParallels }
  where
    (duplicatesWithParallels   , nDuplicatesWithParallels    ) = findDuplicates' collectionWithParallels
    (duplicatesWithoutParallels, nDuplicatesWithoutParallels ) = findDuplicates' collectionWithoutParallels

    findDuplicates' :: Map.Map Sticker Int -> (Map.Map Sticker Int, Int)
    findDuplicates' collection = 
      let duplicates = subtract 1 <$> Map.filter (>1) collection
      in  (duplicates, sum duplicates)

writeDuplicates :: DuplicatesInformation -> IO ()
writeDuplicates DuplicatesInformation{..} = do 
  writeDuplicates' (duplicatesWithParallels,    nDuplicatesWithParallels   ) FP.duplicatesPathWithParallels
  writeDuplicates' (duplicatesWithoutParallels, nDuplicatesWithoutParallels) FP.duplicatesPathWithoutParallels
  where
    writeDuplicates' :: (Map.Map Sticker Int, Int) -> FilePath -> IO ()
    writeDuplicates' (duplicates, nDuplicates) filePath = do 
      let header = mconcat ["You have ", show nDuplicates, " duplicate stickers.\n\n"]
          body   = Map.foldrWithKey appendDuplicate "" duplicates

      writeFile filePath (header <> body)

    appendDuplicate :: Sticker -> Int -> String -> String
    appendDuplicate sticker       1 currentBody = mconcat [show sticker, ", ", currentBody]
    appendDuplicate sticker nCopies currentBody = mconcat [show sticker, "(", show nCopies, "), ", currentBody]