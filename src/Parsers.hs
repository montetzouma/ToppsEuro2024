module Parsers 
  ( parseCatalogue
  , parseGot )
where

import Control.Applicative ((<|>), optional)

import DataTypes

import qualified Data.Char                  as C
import qualified Data.List                  as L
import qualified Data.List.Split            as LS
import qualified Data.Map                   as Map
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Void                  as V
import qualified FilePaths                  as FP
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Char.Lexer as ML 
import qualified Text.Read                  as R


-- TODO: Replace String with Text
type Parser = M.Parsec V.Void String


chapterParser :: Parser Chapter
chapterParser = do
  maybeChapter <- R.readMaybe <$> M.takeWhileP Nothing C.isAlpha

  case maybeChapter of
    Nothing      -> M.failure Nothing S.empty
    Just chapter -> return chapter


subchapterParser :: Parser Subchapter
subchapterParser 
  =   M.try landmarkParser
  <|> M.try playerToWatchParser
  <|> starPlayerParser
  <|> topPlayerParser 
  <|> numberParser

landmarkParser :: Parser Subchapter
landmarkParser = do
  MC.char 'P'
  MC.space
  P <$> ML.decimal

playerToWatchParser :: Parser Subchapter 
playerToWatchParser = do
  MC.string' "PTW"
  return PTW

starPlayerParser :: Parser Subchapter
starPlayerParser = do
  MC.string' "SP"
  return SP

topPlayerParser :: Parser Subchapter
topPlayerParser = do
  MC.string' "TOP"
  MC.space
  TOP <$> ML.decimal

numberParser :: Parser Subchapter
numberParser = Number <$> ML.decimal


chapterSubchapterInfoParser :: Parser (Chapter, Subchapter, String)
chapterSubchapterInfoParser = do
  -- Covers the case of a two-part mini-sticker
  optional (MC.char '+')
  MC.space
  
  chapter <- chapterParser 
  MC.space
  
  subchapter <- subchapterParser
  optional (MC.char '.')
  -- TODO: Why did I need to remove MC.space here?
  info <- M.takeWhileP Nothing (/= '\n')
  MC.newline
  
  return (chapter, subchapter, info) 

catalogueUsefulLineParser :: Parser [Sticker]
catalogueUsefulLineParser = do 
  (chapter, subchapter, info) <- chapterSubchapterInfoParser

  let rarities  = allRarities info
      cleanInfo = cleanupInfo info 
      stickers  = map (Sticker chapter subchapter cleanInfo . Just) rarities

  return stickers
  where
    -- TODO: These should be replaced by Text versions of isInfixOf
    allRarities :: String -> [Rarity]
    allRarities info
      | "Gold Foil" `L.isInfixOf` info = allStarPlayerRarities
      | "Holo Foil" `L.isInfixOf` info = allNonStarPlayerRarities
      | otherwise                      = [Common] 

    allStarPlayerRarities :: [Rarity]
    allStarPlayerRarities = [StarPlayerUnsigned, StarPlayerSigned]

    allNonStarPlayerRarities :: [Rarity]
    allNonStarPlayerRarities = [Common, MegaEcoBoxExclusive .. OneOfAKind]

-- TODO: If possible remove pack and unpack after switching to strings
cleanupInfo :: String -> String
cleanupInfo info = info'
  where
    infos = LS.splitOneOf "(-" info
    info' = (T.unpack . T.strip . T.pack) (head infos)

catalogueUselessLineParser :: Parser [Sticker]
catalogueUselessLineParser = do
  M.takeWhileP Nothing (/= '\n')
  MC.newline
  return []

catalogueLineParser :: Parser [Sticker]
catalogueLineParser = M.try catalogueUsefulLineParser <|> catalogueUselessLineParser

catalogueParser :: Parser [Sticker]
catalogueParser = concat . filter (not . null) <$> M.some catalogueLineParser

parseCatalogue :: IO StickerCollection
parseCatalogue = do
  catalogue <- readFile FP.cataloguePath
  case M.runParser catalogueParser FP.cataloguePath catalogue of
    Left  e        -> error  (show e)
    Right stickers -> return (createCollection stickers)
  where
    createCollection :: [Sticker] -> StickerCollection
    createCollection stickers
      = let stickersWithoutParallels = map (\s->s{rarity=Nothing}) stickers
        in  StickerCollection
              { collectionWithParallels    = Map.fromList (map (,0) stickers)
              , collectionWithoutParallels = Map.fromList (map (,0) stickersWithoutParallels) }


gotLineParser :: Parser Sticker
gotLineParser = do
  (chapter, subchapter, info) <- chapterSubchapterInfoParser

  let rarity    = findRarityInInfo info
      cleanInfo = cleanupInfo info 
      sticker   = Sticker 
        { chapter    = chapter
        , subchapter = subchapter
        , info       = cleanInfo
        , rarity     = Just rarity }

  return sticker
  where
    -- TODO: These should be replaced by Text versions of isInfixOf
    findRarityInInfo :: String -> Rarity
    findRarityInInfo info
      -- TODO: Allow any capitalisation
      | "Unsigned" `L.isInfixOf` info = StarPlayerUnsigned
      | "Signed"   `L.isInfixOf` info = StarPlayerSigned
      | "Red"      `L.isInfixOf` info = MegaEcoBoxExclusive
      | "Purple"   `L.isInfixOf` info = Rare
      | "Topps"    `L.isInfixOf` info = VeryRare
      | "Green"    `L.isInfixOf` info = SuperRare
      | "Blue"     `L.isInfixOf` info = MegaRare
      | "Black"    `L.isInfixOf` info = UltraRare
      | "Gold"     `L.isInfixOf` info = OneOfAKind
      | otherwise                     = Common

gotParser :: Parser [Sticker]
gotParser = M.many gotLineParser 

parseGot :: IO [Sticker]
parseGot = do
  got <- readFile FP.gotPath
  case M.runParser gotParser FP.gotPath got of
    Left  e        -> error  (show e)
    Right stickers -> return stickers