module CollectionProcessors 
  ( createCollection
  , findDuplicates
  , findNeeds
  , markGot )
where

import DataTypes

import qualified Data.Map as Map


createCollection :: [Sticker] -> Bool -> StickerCollection
createCollection stickers careAboutParallels = Map.fromList (map (,0) stickers')
  where
    stickers' = 
      if   careAboutParallels
      then stickers 
      else map (\s->s{rarity=Nothing}) stickers


markGot :: StickerCollection -> [Sticker] -> Bool -> StickerCollection
markGot collection got careAboutParallels = foldr (Map.adjust (+1)) collection got'
  where
    got' = 
      if   careAboutParallels
      then got 
      else map (\g->g{rarity=Nothing}) got


findNeeds :: StickerCollection -> NeedInformation
findNeeds collection = NeedInformation
  { need  = need     
  , nNeed = nNeed }
  where
    need  = Map.keys $ Map.filter (==0) collection
    nNeed = length need


findDuplicates :: StickerCollection -> DuplicatesInformation
findDuplicates collection = DuplicatesInformation
  { duplicates  = duplicates     
  , nDuplicates = nDuplicates }
  where
    duplicates  = subtract 1 <$> Map.filter (>1) collection
    nDuplicates = sum duplicates