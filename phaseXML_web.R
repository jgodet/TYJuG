#' phaseXML_web
#' Written by TaiohY
#' May 19 2020

#' Return phase of a clinical trial from .xml file
#' @param NCTid
#' @Examples 
#' outcomesXML_web(NCTid = "NCT04319796")
#' outcomesXML_web(NCTid = "NCT03478891")


phaseXML_web<-function(NCTid){
  urlCTxml <- urlCTFile(NCTid = NCTid)
  File<-httr::GET(url = urlCTxml)
  parsedXML <- XML::xmlParse(file= httr::content(File, "text"))
  
  nodesetPh <- XML::getNodeSet(doc = parsedXML, path= "//phase")
  XMLdfPh <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetPh)
  if(is.null(XMLdfPh[1,1])|(XMLdfPh[1,1]=="N/A")){
    XMLdfPh[1,1] <- NA
  }
  return(XMLdfPh)
}