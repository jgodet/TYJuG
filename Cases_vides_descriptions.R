#Proportions d'études ayant une description vide

require(XML)
require(parallel)

path <- "/Users/taiohy/documents/mes documents/Fac/Projet Professionnel/Stage/Espace de travail/AllPublicXML"

listF <- list.files(path, recursive = T, full.names = T, pattern = ".xml")
listF <- unlist(listF)
length(listF)
#[1] 338999

yesNo <- function(filepath){
  parsedXML <- XML::xmlParse(file = filepath)
  nodesetDD <- XML::getNodeSet(doc = parsedXML, path= "//detailed_description")
  return(length(nodesetDD)>0)
}

nVec1 <- mclapply(listF[1:100000], yesNo,mc.cores = 10)
nVec2 <- mclapply(listF[100001:200000], yesNo,mc.cores = 10)
nVec3 <- mclapply(listF[200001:300000], yesNo,mc.cores = 10)
nVec4 <- mclapply(listF[300001:338999], yesNo,mc.cores = 10)

nVec <- unlist(c(nVec1, nVec2, nVec3, nVec4))

table(nVec)
#' nVec
#' FALSE   TRUE 
#' 115587 223412 

sum(nVec)/length(nVec)
#'[1] 0.6590344