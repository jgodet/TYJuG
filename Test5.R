#Tests 5#

########
# Vecteur comprenant liste des NCTid correspondant aux queries
# getStudyFields()
#####
## Exemple
#####
NCT<-getStudyFields(expr="COVID+AND+hydroxychloroquine", fields=c("NCTId"))
NCT
########
# Sortir outcomes d'un fichier XML 
# outcomesXML()
#####
outcomesXML<-function(file){
  parsedXML<-XML::xmlParse(file)
  nodeset<- XML::getNodeSet(doc=parsedXML, path = c("//primary_outcome", "//secondary_outcome"))
  #nodes "outcome" ou "outcomes" n'existent pas dans tous les fichiers CT
  XMLdf<-XML::xmlToDataFrame(doc=parsedXML, nodes = nodeset)
  return(XMLdf)
}
#####
## Exemples
#####
EC1_outcomes <- outcomesXML(file = "NCT03478891.xml")
View(EC1_outcomes)
EC2_outcomes <- outcomesXML(file = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail/AllPublicXML/NCT0434xxxx/NCT04349228.xml")
View(EC2_outcomes)
########
# Chemin d'accès vers 1 fichier
# pathFile()
#####
pathFile<-function(NCTid, locPath)
  #NCTid = numero de l'etude sur CT, locPath = chemin d'acces localisation du dossier "AllPublicXML"
  {
  dirbase<-paste(locPath, "/AllPublicXML/", sep="")
  subdir<-paste(substr(NCTid,1,7),"xxxx/",sep = "")
  file<-paste(NCTid,".xml",sep="")
  findir<-paste(dirbase,subdir,file, sep="")
  return(findir)
}
#####
## Exemples
#####
ei1<-pathFile(NCTid = NCT[3], locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")
ei1
#' [1] "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail/AllPublicXML/NCT0434xxxx/NCT04349228.xml"
ei2<-pathFile(NCT="NCT04150042", locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")
ei2
#' path<-pathFile(NCT="NCT04150042", locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")
ei3 <- pathFile(NCT="NCT04371406", locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")
ei3
########
# Sortir outcomes a partir de queries
# clintri_outcomes()
#####
clintri_outcomes<-function(expr, fields = "NCTid", locPath){
  NCTid.list <- getStudyFields(expr = expr, fields = fields)
  NCTid.list <- NCTid.list[,1]
  for(i in 1:length(NCTid.list)){
    xml.file <- pathFile(NCTid = NCTid.list[i], locPath = locPath)
    outcomes <- rbind(outcomesXML(file = xml.file))
  }
}

#####
# Tests
#####
EC_OC <- clintri_outcomes(expr = "COVID+AND+hydroxychloroquine", locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")

########
# Draft
#####

pathDF<-function(nr, lp){
  for(i in 1:length(nr)){
    df<-data.frame()
    df[1,i]<-cbind(pathFile(NCT=nr[i], locPath = lp))
  }
  return(df)
}

DF1<-pathDF(nr = NCT, lp="/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")
DF1
summary(DF1)

parseXML_clintri<-function(path){
  for(i in 1:length(NCT))
  parseXML<-XML::xmlParse(path)
}

NCT<-NCT[,1]
pathFile(NCT = NCT[3], locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")


pathFile(NCT=NCT[30], locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")

data <- XML::xmlParse(paths(NCT = NCT))
xmldf<- XML::xmlToDataFrame(data)
data$outcomes

## Exemple JuG
readIt <- function(path1){
  a< - read (path1)
  return( a)
}

results <- numeric()
for (i in list.files(path)){
  results <- rbinds(readIt(i))}
##
