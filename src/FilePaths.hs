module FilePaths 
  ( cataloguePath                 
  , gotPath                       
  , needPathWithParallels         
  , needPathWithoutParallels      
  , duplicatesPathWithParallels   
  , duplicatesPathWithoutParallels )
where


cataloguePath, gotPath, needPathWithParallels, duplicatesPathWithParallels, needPathWithoutParallels, duplicatesPathWithoutParallels :: FilePath
cataloguePath                  = "files/Cartophilic_Catalogue.txt"
gotPath                        = "files/Got.txt"
needPathWithParallels          = "files/With_Parallels/Need.txt"
needPathWithoutParallels       = "files/Without_Parallels/Need.txt"
duplicatesPathWithParallels    = "files/With_Parallels/Duplicates.txt"
duplicatesPathWithoutParallels = "files/Without_Parallels/Duplicates.txt"