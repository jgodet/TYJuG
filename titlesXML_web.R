# titlesXML_web
# Writtent by TaiohY
# Jun 29 2020

#' Return as a Data Frame titles of a clinical from NCTid
#' @param NCTid
#' @Example titlesXML_web(NCTid = "NCT04319796")

titlesXML_web<-function(NCTid){
  urlCTxml <- urlCTFile(NCTid = NCTid)
  File<-httr::GET(url = urlCTxml)
  parsedXML <- XML::xmlParse(file= httr::content(File, "text"))
  nodesetOT <- XML::getNodeSet(doc = parsedXML, path= "//official_title")
  XMLdf <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetOT)
  if(is.null(XMLdf[1,1])){
    XMLdf <- NULL
  }
  else{
    colnames(XMLdf) <- "Title"
    NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
    id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
    XMLdf$NCTid=id_info_df[1,1]
  }
  return(XMLdf)
}
