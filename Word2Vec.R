#Word2vec

devtools::install_github("mukul13/rword2vec")
library("rword2vec")
ls("package:rword2vec")
library("dplyr")

CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 100)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 100)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
BDD <- bind_rows(CT_des1, CT_des2) 
BDD <- BDD %>% 
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
BDD <- distinct(BDD[,-1])
text <- BDD$text
text1 <- paste0(text, collapse = " ")
View(text1)
model <- word2vec(train_file = text1, output_file = "vec.bin", binary = 1)
