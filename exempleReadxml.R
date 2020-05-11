# Test lecture fichier
require("XML")
path <- "/home/jgodet/Téléchargements/AllPublicXML/" #dézippé ici chez moi

NCTid <- "NCT04150042"
fileList <- list.files(path = path, recursive = TRUE)
indFile <- grep(pattern = NCTid,x = fileList)

data <- xmlParse(fileList[indFile])
