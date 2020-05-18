#' pathFile()
#' Written by Taiohy
#' May 13 2020

#' Return directory to clinical trial .xml file stored in "AllPublicXML" folder
#' @param NCTid NCTid
#' @param locPath Directory of "AllPublicXML" folder
#' @example pathFile(NCT="NCT04150042", dirBase = dirBase)

pathFile<-function(NCTid, dirBase = dirBase){
  dirbase<-paste(locPath, "/AllPublicXML/", sep="")
  subdir<-paste(substr(NCTid,1,7),"xxxx/",sep = "")
  file<-paste(NCTid,".xml",sep="")
  findir<-paste(dirbase,subdir,file, sep="")
  return(findir)
}
