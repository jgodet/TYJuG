#' outcomesXML()
#' Written by TaiohY
#' May 13 2020

#' Return as a Data Frame primary and secondary outcomes of a clinical trialfrom .xml file
#' @param file .xml file of clinical trial 
#' @example outcomesXML(file = "NCT03478891.xml")

outcomesXML<-function(file){
  parsedXML <- XML::xmlParse(file = file)
  nodesetPO <- XML::getNodeSet(doc = parsedXML, path= "//primary_outcome")
  XMLdfPO <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetPO)
  if(is.null(XMLdfPO[1,1])){
    XMLdf <- NULL
  }
  else {
    XMLdfPO$Type="Primary outcome"
    nodesetSO <- XML::getNodeSet(doc = parsedXML, path = "//secondary_outcome")
    XMLdfSO <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetSO)
    if(!is.null(XMLdfSO[1,1])){
      XMLdfSO$Type <- "Secondary outcome"
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
}
