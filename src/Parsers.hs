module Parsers 
  ( parseCatalogue
  , parseGot )
where

import Control.Applicative ((<|>), optional)

import DataTypes

import qualified Data.Char                  as C
import qualified Data.List                  as L
import qualified Data.List.Extra            as LE
import qualified Data.List.Split            as LS
import qualified Data.Map                   as Map
import qualified Data.Set                   as S
import qualified Data.Void                  as V
import qualified FilePaths                  as FP
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Char.Lexer as ML 
import qualified Text.Read                  as R


-- TODO: Replace String with Text
type Parser = M.Parsec V.Void String


chapterParser :: Parser Chapter
chapterParser 
  =   M.try twoFoldChapterParser
  <|> oneFoldChapterParser

oneFoldChapterParser :: Parser Chapter
oneFoldChapterParser = do
  maybeChapter <- R.readMaybe . map C.toUpper <$> M.takeWhileP Nothing C.isAlpha

  case maybeChapter of
    Nothing      -> M.failure Nothing S.empty
    Just chapter -> return chapter

twoFoldChapterParser :: Parser Chapter
twoFoldChapterParser = do
  ch1 <- oneFoldChapterParser
  MC.char '_'
  ch2 <- oneFoldChapterParser

  return $ ch1 <> ch2


subchapterParser :: Parser Subchapter
subchapterParser 
  =   M.try landmarkParser
  <|> M.try playerToWatchParser
  <|> starPlayerParser
  <|> topPlayerParser 
  <|> M.try twoNumbersParser
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

-- This syntax will never be found in Cartophilic's catalogue but will be used in the Got list.
twoNumbersParser :: Parser Subchapter
twoNumbersParser = do
  n1 <- ML.decimal
  MC.char '-'
  n2 <- ML.decimal
  return (TwoNumbers n1 n2) 


chapterSubchapterInfoParser :: Parser (Chapter, Subchapter, String)
chapterSubchapterInfoParser = do
  chapter <- chapterParser 
  MC.space
  
  subchapter <- subchapterParser
  optional (MC.char '.')
  -- TODO: Why did I need to remove MC.space here?
  info <- M.takeWhileP Nothing (/= '\n')
  MC.newline
  
  return (chapter, subchapter, info) 

catalogueOneFoldStickerParser :: Parser [Sticker]
catalogueOneFoldStickerParser = do  
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

catalogueTwoFoldStickerParser :: Parser [Sticker]
catalogueTwoFoldStickerParser = do 
  (chapter1, subchapter1, info1) <- chapterSubchapterInfoParser

  MC.char '+'
  MC.space

  (chapter2, subchapter2, info2) <- chapterSubchapterInfoParser

  -- Two-fold stickers never have a foil
  let joinedChapter    = chapter1    <> chapter2
      joinedSubchapter = subchapter1 <> subchapter2
      joinedInfo       = mconcat [cleanupInfo info1, " & ", cleanupInfo info2]
      sticker          = Sticker joinedChapter joinedSubchapter joinedInfo (Just Common)

  return [sticker]


catalogueUsefulLineParser :: Parser [Sticker]
catalogueUsefulLineParser
  =   M.try catalogueTwoFoldStickerParser
  <|> M.try catalogueOneFoldStickerParser

cleanupInfo :: String -> String
cleanupInfo info = info'
  where
    infos = LS.splitOneOf "(-" info
    info' = LE.trim (head infos)

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
      | "unsigned" `L.isInfixOf` infoLower = StarPlayerUnsigned
      | "signed"   `L.isInfixOf` infoLower = StarPlayerSigned
      | "red"      `L.isInfixOf` infoLower = MegaEcoBoxExclusive
      | "purple"   `L.isInfixOf` infoLower = Rare
      | "topps"    `L.isInfixOf` infoLower = VeryRare
      | "green"    `L.isInfixOf` infoLower = SuperRare
      | "blue"     `L.isInfixOf` infoLower = MegaRare
      | "black"    `L.isInfixOf` infoLower = UltraRare
      | "gold"     `L.isInfixOf` infoLower = OneOfAKind
      | otherwise                          = Common
      where
        infoLower = map C.toLower info

gotParser :: Parser [Sticker]
gotParser = M.many gotLineParser 

parseGot :: IO [Sticker]
parseGot = do
  got <- readFile FP.gotPath
  case M.runParser gotParser FP.gotPath got of
    Left  e        -> error  (show e)
    Right stickers -> return stickers