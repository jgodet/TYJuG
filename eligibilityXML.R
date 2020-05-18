#' eligibilityXML
#' written by TaiohY
#' May 18 2020

#' Return as a Data Frame eligibility criteria of a clinical trial from .xml file
#' @param file .xml file of clinical trial 
#' @example eligibilityXML(file = "NCT03478891.xml")

eligibilityXML <- function (file){
  parsedXML <- XML::xmlParse(file = file)
  nodesetEL <- XML::getNodeSet(doc = parsedXML, path= "//eligibility")
  XMLdfEL <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetEL)
  NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
  id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
  for(i in 1:nrow(XMLdfEL)){
    XMLdfEL$NCTid=id_info_df[1,1]
  }
  return(XMLdfEL)
}