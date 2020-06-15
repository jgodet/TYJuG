#Clintri_FactoMineR

library("FactoMineR")
library("mvtnorm")
library("dplyr")
library("tidyr")
library("keras")
library("tidytext")

#Préparation des données
CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 100)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 100)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
daten <- bind_rows(CT_des1, CT_des2) 
daten <- daten %>% mutate_at(c("NCTid", "Grp"), factor)
daten <- daten %>% group_by(NCTid) %>% mutate(text = paste0(textblock, collapse = " "))
#dim(daten)
daten2 <- distinct(daten[,-1])
#dim(daten2)
tokenizer2 <- text_tokenizer(num_words = 7000, filters = "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n1234567890") %>% 
  fit_text_tokenizer(daten2$text)
mat <- texts_to_matrix(tokenizer = tokenizer2, texts =  daten2$text, mode = "tfidf")
res <- apply(mat, MARGIN = 2, FUN = mean)
dim(mat)
View(mat)
length(tokenizer2$word_counts)
View(tokenizer2$word_counts)
mat <- mat[,res>0]
dim(mat)
colnames(mat) <- names(tokenizer2$word_index)
rownames(mat) <- paste0(daten2$NCTid, daten2$Grp, sep = " ")
View(mat)

res.pca <- PCA(mat,scale.unit=T, ncp=2, graph = T)
res.pca
plot.PCA(res.pca)

#Autre preparation des données
CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 100)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 100)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
daten <- bind_rows(CT_des1, CT_des2) 
daten <- daten %>% mutate_at(c("NCTid", "Grp"), factor)
daten <- daten %>% group_by(NCTid) %>% mutate(text = paste0(textblock, collapse = " "))
daten <- distinct(daten[,-1])
daten <- unite(daten, id, NCTid, Grp, sep = " ")
View(daten)
#dim(daten)
daten2 <- daten %>%
  unnest_tokens(word, text, token = "words")
data("stop_words")
daten2 <- daten2 %>%
  anti_join(stop_words)
daten2 <- group_by(daten2,id)
dim(daten2)
View(daten2)
daten3 <- daten2 %>%
  separate(word, as.vector(daten2$id, token = "words"))
O_H_daten2 <- daten2
for(unique_value in unique(O_H_daten2$word)){
  O_H_daten2[paste(unique_value)]<-ifelse(O_H_daten2$word==unique_value, 1,0)
}
dim(O_H_daten2)
View(O_H_daten2)
O_H_daten2$id <- daten2$id
O_H_daten3 <- summarise_all(O_H_daten2[,-2],funs(sum))
O_H_daten4 <- O_H_daten3[,-1]
row.names(O_H_daten4) <- O_H_daten3$id
View(O_H_daten4)
res.pca <- PCA(O_H_daten4, scale.unit=TRUE, ncp=2, graph = T)
