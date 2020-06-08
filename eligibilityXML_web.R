#' eligibilityXML_web
#' written by TaiohY
#' May 18 2020

#' Return as a Data Frame eligibility criteria of a clinical trial from clinicaltraials.gov database
#' @param NCTid 
#' @example eligibilityXML_web(NCTid = "NCT03478891")

eligibilityXML_web <- function (NCTid){
  urlCTxml <- urlCTFile(NCTid = NCTid)
  File<-httr::GET(url = urlCTxml)
  parsedXML <- XML::xmlParse(file= httr::content(File, "text"))
  nodesetEL <- XML::getNodeSet(doc = parsedXML, path= "//eligibility")
  XMLdfEL <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetEL)
  NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
  id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
  for(i in 1:nrow(XMLdfEL)){
    XMLdfEL$NCTid=id_info_df[1,1]
  }
  return(XMLdfEL)
}
