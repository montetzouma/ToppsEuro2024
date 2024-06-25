module DataTypes
  ( Chapter               (..)
  , DuplicatesInformation (..)
  , NeedInformation       (..)
  , Rarity                (..)
  , Sticker               (..)
  , StickerCollection     (..)
  , Subchapter            (..) )
where

import qualified Data.Map   as Map 
import qualified Data.Maybe as Maybe

import qualified FilePaths  as FP


data Chapter
  -- Introduction
  = TOPPS
  | UEFA
  -- Germany Host of the UEFA Euro 2024
  | EURO
  -- Group A
  | GA
  | GER
  | SCO
  | HUN
  | SUI
  -- Group B
  | GB
  | ESP
  | CRO 
  | ITA
  | ALB
  -- Group C
  | GC
  | SVN
  | DEN
  | SRB
  | ENG
  -- Managers/Dream Team
  | MM
  -- Group D
  | GD
  | NED
  | AUT
  | FRA
  -- Group D (Playoffs)
  | POL
  | EST
  | WAL
  | FIN
  -- Group E
  | GE
  | BEL
  | SVK
  | ROM
  -- Group E (Playoffs)
  | ISR
  | ICE
  | BIH
  | UKR
  --Group F
  | GF
  | TUR
  | POR
  | CZE
  -- Group F (Playoffs)
  | GEO 
  | LUX
  | GRE
  | KAZ
  -- Legends
  | LEG 
  deriving (Eq, Ord, Read, Show)


data Subchapter
  = P      Int
  | PTW
  | SP      
  | TOP    Int
  | Number Int
  deriving (Eq, Ord)

instance Show Subchapter where
  show (P      n) = unwords ["P", show n]
  show PTW        = "PTW"
  show SP         = "SP"
  show (TOP    n) = unwords ["TOP", show n]
  show (Number n) = show n


data Rarity
  = Common              -- Not rare (no foil at all or silver foil)
  | StarPlayerUnsigned  -- Unsigned star player (gold foil)
  | StarPlayerSigned    -- Signed   star player (gold foil - rarer)
  | MegaEcoBoxExclusive -- Red Dots 
  | Rare                -- Purple
  | VeryRare            -- Topps Foil
  | SuperRare           -- Green
  | MegaRare            -- Blue
  | UltraRare           -- Black
  | OneOfAKind          -- Gold
  deriving (Enum, Eq, Ord)

instance Show Rarity where
  show Common              = ""
  show StarPlayerUnsigned  = "Unsigned Gold Foil"
  show StarPlayerSigned    = "Signed Gold Foil"
  show MegaEcoBoxExclusive = "Red Dots Foil (Mega Eco Box Exclusive)"
  show Rare                = "Purple Foil (Rare)" 
  show VeryRare            = "Topps Foil (Very Rare)"
  show SuperRare           = "Green Foil (Super Rare)"
  show MegaRare            = "Blue Foil (Mega Rare)"
  show UltraRare           = "Black Foil (Ultra Rare)"
  show OneOfAKind          = "Gold Foil (One of a kind)"


data Sticker = Sticker
  { chapter    :: Chapter
  , subchapter :: Subchapter
  , info       :: String 
  , rarity     :: Maybe Rarity }  -- Set Rarity=Nothing if you don't care about parallel versions.


instance Eq Sticker where
  (==) Sticker{chapter=chapter1, subchapter=subchapter1, rarity=rarity1} Sticker{chapter=chapter2, subchapter=subchapter2, rarity=rarity2}
    =  chapter1    == chapter2
    && subchapter1 == subchapter2
    && rarity1     == rarity2

instance Ord Sticker where
  compare Sticker{chapter=chapter1, subchapter=subchapter1, rarity=rarity1} Sticker{chapter=chapter2, subchapter=subchapter2, rarity=rarity2}
    = compare (chapter1, subchapter1, rarity1) (chapter2, subchapter2, rarity2)

instance Show Sticker where
  show Sticker{..} = wholeString
    where
      mainString = mconcat [show chapter, " ", show subchapter, ".  ", show info]

      wholeString = 
        if   rarity == Just Common || Maybe.isNothing rarity 
        then mainString
        else mconcat [mainString, " - ", show (Maybe.fromJust rarity)]


data StickerCollection = StickerCollection
  { collectionWithParallels    :: Map.Map Sticker Int
  , collectionWithoutParallels :: Map.Map Sticker Int }


data NeedInformation = NeedInformation
  { needWithParallels     :: [Sticker]
  , needWithoutParallels  :: [Sticker]
  , nNeedWithParallels    :: Int
  , nNeedWithoutParallels :: Int }


data DuplicatesInformation = DuplicatesInformation
  { duplicatesWithParallels     :: Map.Map Sticker Int
  , duplicatesWithoutParallels  :: Map.Map Sticker Int
  , nDuplicatesWithParallels    :: Int
  , nDuplicatesWithoutParallels :: Int }