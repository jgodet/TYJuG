#' NCTidToTitles_web
#' written by TaiohY
#' May 13 2020

#' Returns titles of clinical trials outcomes from CT.gov data base from NCTids
#' @param NCTid.list list of NCTid to look for
#' @example 
#' List <- as.data.frame(c("NCT00440726", "NCT00506597", "NCT00993135"))
#' NCTidToTitles_web(NCTid.list = List)

NCTidToTitles_web <- function(NCTid.list){
  table <- lapply(FUN=titlesXML_web, NCTid.list[1:nrow(NCTid.list),])
  table <- data.table::rbindlist(table, fill = T)
  return(table)
}
