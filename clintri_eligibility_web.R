#' clintri_eligibility_web
#' written by TaiohY
#' May 18 2020

#' Returns informations about clinical trials eligibility criteria from CT.gov data base
#' @param expr Search Expression (sep with +, use AND, OR, ...)
#' @param fields Study Fields (see https://clinicaltrials.gov/api/info/study_fields_list for a list of available fields)
#' @param max_rnk Maximum Rank (default is min(numberReturns, 20))
#' @example clintri_eligibility_web(expr = "LUPUS", max_rnk = 20)


clintri_eligibility_web<-function(expr, fields = "NCTid", max_rnk = NULL){
  NCTid.list <- getStudyFields(expr = expr, fields = fields, max_rnk = max_rnk)
  table <- mapply(FUN = eligibilityXML_web, NCTid.list[1:nrow(NCTid.list),])
  eligibility <- data.table::rbindlist(table, fill=T)
  eligibility <- eligibility[,c("NCTid","criteria","gender","minimum_age","maximum_age","healthy_volunteers","study_pop","sampling_method")]
}
