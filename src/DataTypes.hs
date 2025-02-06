module DataTypes
  ( Chapter               (..)
  , DuplicatesInformation (..)
  , NeedInformation       (..)
  , Rarity                (..)
  , Sticker               (..)
  , StickerCollection     
  , Subchapter            (..) )
where

import qualified Data.List       as L
import qualified Data.List.Extra as LE
import qualified Data.Map        as Map


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
  deriving (Eq, Ord, Read)
-- The Read instances of two-fold chapters won't be found in Cartophilic's catalogue but can be used when parsing your Got list.

instance Show Chapter where
  show TOPPS   = "TOPPS"
  show UEFA    = "UEFA" 
  show EURO    = "EURO"
  show GA      = "GA"
  show GER     = "GER"
  show SCO     = "SCO"
  show HUN     = "HUN"
  show SUI     = "SUI"
  show GB      = "GB"
  show ESP     = "ESP"
  show CRO     = "CRO"
  show ITA     = "ITA"
  show ALB     = "ALB" 
  show GC      = "GC"
  show SVN     = "SVN"
  show DEN     = "DEN"
  show SRB     = "SRB"
  show ENG     = "ENG"
  show MM      = "MM"
  show GD      = "GD"
  show NED     = "NED"
  show AUT     = "AUT"
  show FRA     = "FRA"
  show POL_EST = "POL-EST"   
  show WAL_FIN = "WAL-FIN" 
  show POL     = "POL"
  show EST     = "EST"      
  show WAL     = "WAL"
  show FIN     = "FIN"
  show GE      = "GE"
  show BEL     = "BEL"
  show SVK     = "SVK"
  show ROM     = "ROM"
  show ISR_ICE = "ISR-ICE"  
  show BIH_UKR = "BIH-UKR" 
  show ISR     = "ISR"
  show ICE     = "ICE"
  show BIH     = "BIH"
  show UKR     = "UKR"
  show GF      = "GF"
  show TUR     = "TUR"
  show POR     = "POR"
  show CZE     = "CZE"
  show GEO_LUX = "GEO-LUX"  
  show GRE_KAZ = "GRE-KAZ"  
  show GEO     = "GEO"
  show LUX     = "LUX"
  show GRE     = "GRE"
  show KAZ     = "KAZ"
  show LEG     = "LEG" 

instance Semigroup Chapter where
  (<>) POL EST = POL_EST
  (<>) WAL FIN = WAL_FIN
  (<>) ISR ICE = ISR_ICE 
  (<>) BIH UKR = BIH_UKR
  (<>) GEO LUX = GEO_LUX
  (<>) GRE KAZ = GRE_KAZ

  -- So you don't need to worry which country you should type first in your got list.
  (<>) EST POL = POL_EST
  (<>) FIN WAL = WAL_FIN
  (<>) ICE ISR = ISR_ICE 
  (<>) UKR BIH = BIH_UKR
  (<>) LUX GEO = GEO_LUX
  (<>) KAZ GRE = GRE_KAZ

  (<>) MM  MM  = MM
  (<>) POL POL = POL
  (<>) WAL WAL = WAL
  (<>) ISR ISR = ISR
  (<>) BIH BIH = BIH
  (<>) GEO GEO = GEO
  (<>) GRE GRE = GRE
  (<>) EST EST = EST
  (<>) FIN FIN = FIN
  (<>) ICE ICE = ICE
  (<>) UKR UKR = UKR
  (<>) LUX LUX = LUX
  (<>) KAZ KAZ = KAZ

  (<>) _   _   = error "You are trying to create a two-fold sticker with a chapter which doesn't exist!"


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

instance Semigroup Subchapter where
  (<>) SP          SP          = SP
  (<>) (Number n1) (Number n2) = TwoNumbers n1 n2
  (<>) _           _           = error "You are trying to create a two-fold sticker with a subchapter which doesn't exist!"


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
  show SuperRare           = "-gr"
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


type StickerCollection = Map.Map Sticker Int


data NeedInformation = NeedInformation
  { need  :: [Sticker]
  , nNeed :: Int }

instance Show NeedInformation where
  show NeedInformation{..} = header <> body
    where
      header = mconcat ["You need ", show nNeed, " sticker(s).\n\n"]
      body   = L.intercalate ", " (map show need)


data DuplicatesInformation = DuplicatesInformation
  { duplicates  :: Map.Map Sticker Int
  , nDuplicates :: Int }

instance Show DuplicatesInformation where
  show  DuplicatesInformation{..} = header <> body
    where
      header = mconcat ["You have ", show nDuplicates, " duplicate sticker(s).\n\n"]
      body   = LE.dropSuffix ", " $ Map.foldrWithKey appendDuplicate "" duplicates

      appendDuplicate :: Sticker -> Int -> String -> String
      appendDuplicate sticker       1 currentBody = mconcat [show sticker, ", ", currentBody]
      appendDuplicate sticker nCopies currentBody = mconcat [show sticker, "(", show nCopies, "), ", currentBody]
