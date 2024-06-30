module CollectionProcessors 
  ( findDuplicates
  , findNeeds
  , markGot )
where

import DataTypes

import qualified Data.Map as Map


markGot :: [Sticker] -> StickerCollection -> StickerCollection
markGot gotStickers StickerCollection{..} = StickerCollection
  { collectionWithParallels    = markGot' collectionWithParallels    gotStickers
  , collectionWithoutParallels = markGot' collectionWithoutParallels gotStickersWithoutParallels }
  where
    gotStickersWithoutParallels = map (\s->s{rarity=Nothing}) gotStickers

    markGot' :: Map.Map Sticker Int -> [Sticker] -> Map.Map Sticker Int
    markGot' = foldr (Map.adjust (+1))


findNeeds :: StickerCollection -> Bool -> NeedInformation
findNeeds sc careAboutParallels = NeedInformation
  { need  = need     
  , nNeed = nNeed }
  where
    collection = whichCollection sc careAboutParallels
    need       = Map.keys $ Map.filter (==0) collection
    nNeed      = length need


findDuplicates :: StickerCollection -> Bool -> DuplicatesInformation
findDuplicates sc careAboutParallels = DuplicatesInformation
  { duplicates  = duplicates     
  , nDuplicates = nDuplicates }
  where
    collection  = whichCollection sc careAboutParallels
    duplicates  = subtract 1 <$> Map.filter (>1) collection
    nDuplicates = sum duplicates


whichCollection :: StickerCollection -> Bool -> Map.Map Sticker Int
whichCollection sc True  = collectionWithParallels    sc
whichCollection sc False = collectionWithoutParallels sc