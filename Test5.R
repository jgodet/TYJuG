#Tests 5#

########
# Vecteur comprenant liste des NCTid correspondant aux queries
# getStudyFields()
#####
## Exemple
#####
NCT<-getStudyFields(expr="COVID+AND+hydroxychloroquine", fields=c("NCTId"), max_rnk = 1000)
NCT
NCT2<-getStudyFields(expr="FAVIPIRAVIR", fields=c("NCTId"),max_rnk=50)
NCT2
########
# Sortir outcomes d'un fichier XML 
# outcomesXML()
#####

outcomesXML<-function(file){
  parsedXML <- XML::xmlParse(file = file)
  nodesetPO <- XML::getNodeSet(doc = parsedXML, path= "//primary_outcome")
  XMLdfPO <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetPO)
  for(i in 1:nrow(XMLdfPO)){
      XMLdfPO$Type="Primary outcome"
  }
  nodesetSO <- XML::getNodeSet(doc = parsedXML, path = "//secondary_outcome")
  XMLdfSO <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetSO)
  if(is.null(XMLdfSO[1,1])==F){
  for(i in 1:nrow(XMLdfSO)){
    XMLdfSO$Type="Secondary outcome"
  }
  XMLdf <- merge(XMLdfPO, XMLdfSO, all=T)
  }
  else XMLdf <- XMLdfPO
  NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
  id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
  for(i in 1:nrow(XMLdf)){
    XMLdf$NCTid=id_info_df[1,1]
  }
  return(XMLdf)
}

#####
## Exemples
#####
EC1_outcomes <- outcomesXML(file = "NCT03478891.xml")
View(EC1_outcomes)
summary(EC1_outcomes)
EC2_outcomes <- outcomesXML(file = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail/AllPublicXML/NCT0434xxxx/NCT04342156.xml")
View(EC2_outcomes)
summary(EC2_outcomes)
DF<- merge(EC1_outcomes, EC2_outcomes, all=T)
View(DF)
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
ei1<-pathFile(NCTid = NCT[3,1], locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail")
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
clintri_outcomes<-function(expr, fields = "NCTid", locPath, max_rnk = NULL){
  NCTid.list <- getStudyFields(expr = expr, fields = fields, max_rnk = max_rnk)
  NCTid.list <- NCTid.list[,1]
  outcomes <- NULL
  for(i in 1:length(NCTid.list)){
    xml.file <- pathFile(NCTid = NCTid.list[i], locPath = locPath)
    outcomes.df <- outcomesXML(file = xml.file)
    outcomes <- merge(outcomes, outcomes.df, all=T)
  }
  outcomes
}

#####
# Tests
#####
EC_OC <- clintri_outcomes(expr = "COVID+AND+hydroxychloroquine", locPath = "/Users/taiohy/documents/mes documents/fac/projet professionnel/espace de travail", max_rnk = 20)
View(EC_OC)
NCT<-getStudyFields(expr="COVID+AND+hydroxychloroquine", fields=c("NCTId"), max_rnk = 3)
NCT
