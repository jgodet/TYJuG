#' clintri_outcomes
#' written by TaiohY
#' May 13 2020

#' Returns informations about clinical trials outcomes from downloaded CT.gov data base
#' @param expr Search Expression (sep with +, use AND, OR, ...)
#' @param fields Study Fields (see https://clinicaltrials.gov/api/info/study_fields_list for a list of available fields)
#' @param locPath Directory of "AllPublicXML" folder
#' @param max_rnk Maximum Rank (default is min(numberReturns, 20))
#' @example clintri_outcomes(expr = "COVID+AND+hydroxychloroquine", locPath = "/Users/taiohy/Documents/", max_rnk = 20)


clintri_outcomes<-function(expr, fields = "NCTid", locPath, max_rnk = NULL){
  NCTid.list <- getStudyFields(expr = expr, fields = fields, max_rnk = max_rnk)
  NCTid.list <- NCTid.list[,1]
  outcomes <- NULL
  for(i in 1:length(NCTid.list)){
    xml.file <- pathFile(NCTid = NCTid.list[i], locPath = locPath)
    outcomes.df <- outcomesXML(file = xml.file)
    outcomes <- merge(outcomes, outcomes.df, all=T)
  }
  outcomes
}

