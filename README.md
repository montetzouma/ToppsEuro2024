# Topps Euro 2024 Stickers Tracker
Keep track of the complete list of stickers, the ones you got, the ones you need and your duplicates.

For each list two versions are presented: One which includes parallel versions and one which doesn't. 

Note that the [rarity colours](https://cartophilic-info-exch.blogspot.com/2024/04/topps-uefa-euro-2024-14-parallel.html) in this app correspond to the non-Swiss version of the album.

Complete list of stickers fetched from [Cartophilic's blog](https://cartophilic-info-exch.blogspot.com/2024/04/topps-uefa-euro-2024-10-checklist.html).


## Instructions
This is a [Haskell](https://www.haskell.org/) and [Stack](https://docs.haskellstack.org/en/stable/) project. To use it you should install Haskell and the Stack tool. To compile it use the commands `stack build` and `stack exec ToppsEuro2024-exe` in the relevant directory.
  
The program will create the full list of stickers using `/files/Cartophilic_Catalogue.txt`. Don't edit this file!

Add the stickers you already have in `/files/Got.txt`. When you acquire more stickers add them in this file. If you swap some away remove them from this file.

The program will produce your list of duplicate stickers (together with how many extra copies of each you've got) and the list of stickers you still need. If you care about parallel versions see the `files/With_Parallels` directory, if you don't see `files/Without_Parallels`.

In `/files/Got.txt` when adding foil stickers you can indicate their rarity by adding one of the following keywords at the end of the respective line:
| Keyword  | Meaning                                        |
| ---------| -----------------------------------------------|
| Unsigned | An unsigned gold foil sticker of a star player |
| Signed   | A signed gold foil sticker of a star player    |
| Red      | Red Dots foil (Mega Eco Box exclusive)         |
| Purple   | Purple foil (Rare)                             |
| Topps    | Silver foil with Topps logo (Very rare)        |
| Green    | Green foil (Super rare)                        |
| Blue     | Blue foil (Mega rare)                          |
| Black    | Black foil (Ultra rare)                        |
| Gold     | Gold foil (One of a kind)                      |

The keywords should be exactly the same as the ones in the Keyword column of the table above and be at the end of the line! If the sticker has no rarity (or you don't care about it) you don't need to add anything.