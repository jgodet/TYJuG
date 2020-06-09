#' phaseXML()
#' Written by TaiohY
#' May 93 2020

#' Return phase of a clinical trialfrom .xml file
#' @param file .xml file of clinical trial 
#' @example phaseXML(file = "NCT03478891.xml")


phaseXML<-function(file){
  parsedXML <- XML::xmlParse(file = file)
  nodesetPh <- XML::getNodeSet(doc = parsedXML, path= "//phase")
  XMLdfPh <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetPh)
  if(is.null(XMLdfPh[1,1])|(XMLdfPh[1,1]=="N/A")){
    XMLdfPh[1,1] <- NA
  }
  return(XMLdfPh)
}
