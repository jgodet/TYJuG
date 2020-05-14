#' clintri_outcomes_web
#' written by TaiohY
#' May 13 2020

#' Returns informations about clinical trials outcomes from CT.gov data base
#' @param expr Search Expression (sep with +, use AND, OR, ...)
#' @param fields Study Fields (see https://clinicaltrials.gov/api/info/study_fields_list for a list of available fields)
#' @param locPath Directory of "AllPublicXML" folder
#' @param max_rnk Maximum Rank (default is min(numberReturns, 20))
#' @example clintri_outcomes(expr = "COVID+AND+hydroxychloroquine", locPath = "/Users/taiohy/Documents/", max_rnk = 20)


clintri_outcomes_web<-function(expr, fields = "NCTid", locPath, max_rnk = NULL){
  NCTid.list <- getStudyFields(expr = expr, fields = fields, max_rnk = max_rnk)
  NCTid.list <- NCTid.list[,1]
  outcomes <- NULL
  for(i in 1:length(NCTid.list)){
    outcomes.df <- outcomesXML_web(NCTid = NCTid.list[i])
    outcomes <- merge(outcomes, outcomes.df, all=T)
  }
  outcomes
}
