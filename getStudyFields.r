# getStudyFields.r
# written by JuG
# May 08 2020


#' Returns values from selected API fields for a large set of study records
#' @author JuG
#' @description https://clinicaltrials.gov/api/gui/ref/api_urls
#' @param expr Search Expression (sep with +, use AND, OR, ...)
#' @param fields Study Fields (see https://clinicaltrials.gov/api/info/study_fields_list for a list of available fields)
#' @param max_rnk Maximum Rank (default is min(numberReturns, 20))
#' @details
#' @examples
#' getStudyFields(expr = 'COVID+AND+hydroxychloroquine', fields = c("NCTId"))
#' getStudyFields(expr = 'COVID+AND+hydroxychloroquine', fields = c("NCTId"),max_rnk=50)
#' getStudyFields(expr = 'COVID+AND+hydroxychloroquine', fields = c("NCTId", "BriefTitle"))
#' @return data.frame
#' @export


getStudyFields<- function(expr, fields, max_rnk=NULL){

  urlBase <- "https://clinicaltrials.gov/api/query/study_fields?"
  urlExpr <- paste("expr=", expr, sep='')
  fields_txt <- paste(fields, collapse="%2C")
  urlFields <- paste("&fields=", fields_txt, sep='')
  urlRank = paste("&min_rnk=1&max_rnk=", max_rnk, sep='')

  urlFinal <- paste(urlBase,urlExpr,urlFields,urlRank,"&fmt=xml",sep='')

  search_result <- httr::GET(urlFinal)

  parsed_result <- XML::xmlParse(httr::content(search_result, as = "text"))

  xmldf <- XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed_result, "//StudyFields"))
  colnames(xmldf) <- fields
  return(xmldf)
}
