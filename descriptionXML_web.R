#' descriptionXML_web
#' written by TaiohY
#' Jun 08 2020

#' Return as a Data Frame description of a clinical trial from clinicaltraials.gov database
#' @param NCTid 
#' @example descriptionXML_web(NCTid = "NCT03478891")

descriptionXML_web <- function (NCTid){
  urlCTxml <- urlCTFile(NCTid = NCTid)
  File<-httr::GET(url = urlCTxml)
  parsedXML <- XML::xmlParse(file= httr::content(File, "text"))
  
  nodesetDes <- XML::getNodeSet(doc = parsedXML, path= c("//brief_summary","//detailed_description"))
  XMLdfDes <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetDes)
  NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
  id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
  XMLdfDes$NCTid=id_info_df[1,1]
  return(XMLdfDes)
}
