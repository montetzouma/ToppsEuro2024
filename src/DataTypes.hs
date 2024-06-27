module DataTypes
  ( Chapter               (..)
  , DuplicatesInformation (..)
  , NeedInformation       (..)
  , Rarity                (..)
  , Sticker               (..)
  , StickerCollection     (..)
  , Subchapter            (..) )
where

import qualified Data.Map as Map


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
  | POL_EST   -- For two-fold sticker of star players
  | WAL_FIN   -- For two-fold sticker of star players
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
  | ISR_ICE   -- For two-fold sticker of star players
  | BIH_UKR   -- For two-fold sticker of star players
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
  | GEO_LUX   -- For two-fold sticker of star players
  | GRE_KAZ   -- For two-fold sticker of star players
  | GEO 
  | LUX
  | GRE
  | KAZ
  -- Legends
  | LEG 
  deriving (Eq, Ord, Read, Show)
-- The Read instances of two-fold won't be found in Cartophilic's catalogue but can be used when parsing your Got list.


data Subchapter
  = P          Int
  | PTW    
  | SP          
  | TOP        Int
  | Number     Int
  | TwoNumbers Int Int 
  deriving (Eq, Ord)

instance Show Subchapter where
  show (P n)              = mconcat ["-P", show n]
  show PTW                = "-PTW"
  show SP                 = "-SP"
  show (TOP n)            = mconcat ["-TOP", show n]
  show (Number n)         = show n
  show (TwoNumbers n1 n2) = mconcat [show n1, "-", show n2]

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
  show StarPlayerSigned    = "-s"
  show MegaEcoBoxExclusive = "-eu"
  show Rare                = "-p" 
  show VeryRare            = "-tp"
  show SuperRare           = "-g"
  show MegaRare            = "-bu"
  show UltraRare           = "-b"
  show OneOfAKind          = "-g"
  show _                   = ""


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
  show Sticker{chapter, subchapter, rarity} = wholeString
    where
      mainString = mconcat [show chapter, show subchapter]

      rarityString = case rarity of 
        Just r  -> show r 
        Nothing -> ""

      wholeString = mconcat [mainString, rarityString]


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