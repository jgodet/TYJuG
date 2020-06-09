#' clintri_outcomes
#' written by TaiohY
#' May 13 2020

#' Returns informations about clinical trials outcomes from downloaded CT.gov data base
#' @param expr Search Expression (sep with +, use AND, OR, ...)
#' @param fields Study Fields (see https://clinicaltrials.gov/api/info/study_fields_list for a list of available fields)
#' @param dirBase Directory of "AllPublicXML" folder
#' @param max_rnk Maximum Rank (default is min(numberReturns, 20))
#' @example clintri_outcomes(expr = "COVID+AND+hydroxychloroquine", dirBase = "/Users/taiohy/Documents/", max_rnk = 20)


clintri_outcomes<-function(expr, fields = "NCTid", dirBase, max_rnk = NULL){
  NCTid.list <- getStudyFields(expr = expr, fields = fields, max_rnk = max_rnk)
  files.paths <- mapply(NCTid.list, FUN = pathFile)
  table <- mapply(files.paths, FUN = outcomesXML)
  outcomes <- data.table::rbindlist(table, fill=T)
  outcomes
}
