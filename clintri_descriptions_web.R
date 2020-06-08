#' clintri_descriptions_web
#' written by TaiohY
#' Jun 08 2020

#' Returns informations about clinical trials outcomes from CT.gov data base
#' @param expr Search Expression (sep with +, use AND, OR, ...)
#' @param fields Study Fields (see https://clinicaltrials.gov/api/info/study_fields_list for a list of available fields)
#' @param max_rnk Maximum Rank (default is min(numberReturns, 20))
#' @example clintri_descriptions_web(expr = "COVID+AND+hydroxychloroquine", max_rnk = 20)


clintri_descriptions_web<-function(expr, fields = "NCTid", max_rnk = NULL){
  NCTid.list <- getStudyFields(expr = expr, fields = fields, max_rnk = max_rnk)
  table<-lapply(FUN=descriptionXML_web, NCTid.list[1:nrow(NCTid.list),])
  descriptions<-data.table::rbindlist(table, fill = T)
  descriptions
}