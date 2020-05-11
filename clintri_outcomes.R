getwd()
library("rclinicaltrials")
library("xml2")
library("XML")
library("methods")
library("rclinicaltrials")

#' Fonctions prerequises qui ne s'installent pas automatiquement avec le package rclinicaltrials :
#' parse_study_xml()
#' xmltodf()

# Variable = NCT (numéro, pas fichier)
clintri_outcomes<-function(NCT){
  NCT<-NCT[,1]
  dirbase<-"./AllPublicXML/"
  for(i in 1:length(NCT)){
    subdir<-paste(substr(NCT[i],1,7),"xxxx/",sep = "")
    file<-paste(NCT[i],".xml",sep="")
    findir<-paste(dirbase,subdir,file, sep="")
    parseXML<-parse_study_xml(file=findir)
    outcomes<-parseXML$outcomes
    outcomes
  }
}


#Exemples
obj1<-clintri_outcomes(NCT="NCT04331834")
obj1
View(obj1)

### Essais ###
x<-getStudyField(expr = 'COVID+AND+hydroxychloroquine', fields = c("NCTId"))
View(x)
# e.i. : clintri_outcomes(NCT="NCT03478891")
z<-clintri_outcomes(NCT="NCT03650049")
View(z)
