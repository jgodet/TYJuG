
require(XML)
path <- "/home/jgodet/Téléchargements/AllPublicXML"

#je liste lse fichiers avec un chemin complet
listF <- list.files(path, recursive = T, full.names = T, pattern = ".xml")

length(listF) #337235 files

listF[1]
x= listF[2]
  
yesNo <- function(x){ #x is a file path
  parsedXML <- XML::xmlParse(file = x)
  nodesetPO <- XML::getNodeSet(doc = parsedXML, path= "//primary_outcome")
  return(length(nodesetPO)>0)
}

#j'essaie la fonction
nVec <- yesNo(listF[3562])

#j'essaie sur une petite quantité (1/10 par ex)
system.time(nVec <- unname(mapply(listF[1:30000], FUN = yesNo)))
#Ca passe mais un peu long -  avec un peu de patience tu dois pouvoir faire tourner ça 

#sinon parraleliser
require(parallel)
system.time(nVec <- mclapply(listF[1:30000], yesNo,mc.cores = 10)) #nb de cores à chager fn machine

#go  - splitté en 3 car super lent avec vecteur complet???
nVec1 <- mclapply(listF[1:100000], yesNo,mc.cores = 10)
nVec2 <- mclapply(listF[100001:200000], yesNo,mc.cores = 10)
nVec3 <- mclapply(listF[200001:300000], yesNo,mc.cores = 10)
nVec4 <- mclapply(listF[300001:337235], yesNo,mc.cores = 10)

nVec <-unlist(c(nVec1, nVec2, nVec3, nVec4)) 

length(nVec)
table(nVec)
sum(nVec)/length(nVec)



