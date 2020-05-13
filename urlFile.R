#' urlFile()
#' Written by TaiohY
#' May13 2020

#' Return the url to display an individual study protocol as an xml in your browser
#' @param NCTid
#' @Example
#' urlCTFile(NCTid = "NCT04319796")

urlCTFile<-function(NCTid){
  url <- paste("http://clinicaltrials.gov/ct2/show/", NCTid, "?displayxml=true", sep = "")
  return(url)
}
