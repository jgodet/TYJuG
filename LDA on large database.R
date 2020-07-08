#LDA on large database

#####
require(XML)

path <- "/Users/taiohy/documents/mes documents/Fac/Projet Professionnel/Stage/Espace de travail/AllPublicXML"


listF <- list.files(path, recursive = T, full.names = T, pattern = ".xml")
listF<-unlist(listF)
length(listF) #338999
path <- listF[7]
listF[1]


descriptionXML <- function (path){
  parsedXML <- XML::xmlParse(file = path)
  nodesetDes <- XML::getNodeSet(doc = parsedXML, path= c("//brief_summary","//detailed_description"))
  XMLdfDes <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetDes)
  if(!is.null(XMLdfDes[1,1])){
    NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
    id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
    XMLdfDes$NCTid=id_info_df[1,1]
    return(XMLdfDes)
  }
  else XMLdfDes <- NULL
}

parsedXML <- XML::xmlParse(file = listF[20136])
nodesetDes <- XML::getNodeSet(doc = parsedXML, path = c("//brief_summary","//detailed_description"))
XMLdfDes <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetDes)

NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)

XMLdfDes$NCTid=id_info_df[1,1]


require(dplyr)
system.time(nVec <- mclapply(listF[1:50000], FUN = descriptionXML, mc.cores = 10))
View(head(nVec))

table <- data.table::rbindlist(nVec, fill=T)
View(head(table))
table.1 <- table %>%
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
table.2 <- distinct(table.1[,-1])
dim(table.2)

require(parallel)
des.nVec1 <- mclapply(listF[1:50000], FUN = descriptionXML, mc.cores = 10)
des.nVec2 <- mclapply(listF[50001:100000], FUN = descriptionXML, mc.cores = 10)
des.nVec3 <- mclapply(listF[100001:150000], FUN = descriptionXML, mc.cores = 10)
des.nVec4 <- mclapply(listF[150001:200000], FUN = descriptionXML, mc.cores = 10)
#des.nVec5 <- mclapply(listF[200001:250000], FUN = descriptionXML, mc.cores = 10)
#des.nVec6 <- mclapply(listF[250001:300000], FUN = descriptionXML, mc.cores = 10)
#des.nVec7 <- mclapply(listF[300001:338999], FUN = descriptionXML, mc.cores = 10)
#mon ordinateur plante à partir de des.nVec5

table <- data.table::rbindlist(c(des.nVec1, des.nVec2, 
                                 des.nVec3, des.nVec4
                                 ), 
                               fill = T)
table.1 <- table %>%
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
table.2 <- distinct(table.1[,-1])

require(tidytext)
data("stop_words")
table.2$text <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", table.2$text))

BDD_td <- table.2 %>%
  unnest_tokens(word, text, token = "words")%>%
  anti_join(stop_words)

BDD_frq <- BDD_td %>%
  count(NCTid, word)%>%
  group_by(NCTid)

BDD_dtm <- BDD_frq %>%
  cast_dtm(NCTid, word, n)
nrow(uwTable) #58022

require("topicmodels")
BDD_lda <- LDA(BDD_dtm, k = 800)
#####


