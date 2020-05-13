#' outcomesXML_web
#' Written by TaiohY
#' May 13 2020

#' Return as a Data Frame primary and secondary outcomes of a clinical trialfrom .xml file
#' @param NCTid
#' @Examples 
#' outcomesXML_web(NCTid = "NCT04319796")
#' outcomesXML_web(NCTid = "NCT03478891")


outcomesXML_web<-function(NCTid){
  urlCTxml <- urlCTFile(NCTid = NCTid)
  File<-httr::GET(url = urlCTxml)
  parsedXML <- XML::xmlParse(file= httr::content(File, "text"))
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

?httr
