#' clintri_eligibility
#' written by TaiohY
#' May 18 2020

#' Returns informations about clinical trials eligibility criteria from downloaded CT.gov data base
#' @param expr Search Expression (sep with +, use AND, OR, ...)
#' @param fields Study Fields (see https://clinicaltrials.gov/api/info/study_fields_list for a list of available fields)
#' @param dirBase Directory of "AllPublicXML" folder
#' @param max_rnk Maximum Rank (default is min(numberReturns, 20))
#' @example clintri_eligibility(expr = "LUPUS", dirBase = "/Users/taiohy/Documents/", max_rnk = 20)


clintri_eligibility<-function(expr, fields = "NCTid", dirBase, max_rnk = NULL){
  NCTid.list <- getStudyFields(expr = expr, fields = fields, max_rnk = max_rnk)
  files.paths <- mapply(NCTid.list, FUN = pathFile)
  table <- lapply(files.paths, FUN = eligibilityXML)
  eligibility <- data.table::rbindlist(table, fill=T)
  eligibility <- eligibility[,c("NCTid","criteria","gender","minimum_age","maximum_age","healthy_volunteers","study_pop","sampling_method")]
}