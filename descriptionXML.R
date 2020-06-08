#' descriptionXML
#' written by TaiohY
#' Jun 03 2020

#' Return as a Data Frame description of a clinical trial from .xml file
#' @param file .xml file of clinical trial 
#' @example descriptionXML(file = "NCT03478891.xml")
#' 
descriptionXML <- function (file){
parsedXML <- XML::xmlParse(file = file)
nodesetDes <- XML::getNodeSet(doc = parsedXML, path= c("//brief_summary","//detailed_description"))
XMLdfDes <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetDes)
NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
for(i in 1:nrow(XMLdfDes)){
  XMLdfDes$NCTid=id_info_df[1,1]
}
return(XMLdfDes)
}