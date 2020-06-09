data("mtcars")
View(mtcars)
as.matrix(mtcars)
d<-dist(as.matrix(mtcars))
d
hc<- hclust(d)
plot(hc)

library(keras)
library(tidytext)
library(dplyr)
data("stop_words")

#Descriptions EC première expression####
CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 30)
CT_des1_trigrams <- CT_des1 %>%
  unnest_tokens(trigram, textblock, token = "ngrams", n=3)
CT_des1_trigrams_sep <- CT_des1_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
CT_des1_trigrams_filtered <- CT_des1_trigrams_sep %>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word)
tidy_CT_des1 <- CT_des1_trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

View(tidy_CT_des1)

O_H_TR_des1 <- tidy_CT_des1
for(unique_value in unique(O_H_TR_des1$trigram)){
  O_H_TR_des1[paste(unique_value)]<-ifelse(O_H_TR_des1$trigram==unique_value, 1,0)
}

View(O_H_TR_des1)

#Descriptions EC 2e expression####
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 30)
CT_des2_trigrams <- CT_des2 %>%
  unnest_tokens(trigram, textblock, token = "ngrams", n=3)
CT_des2_trigrams_sep <- CT_des2_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
CT_des2_trigrams_filtered <- CT_des2_trigrams_sep %>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word)
tidy_CT_des2 <- CT_des2_trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")
View(tidy_CT_des2)

O_H_TR_des2 <- tidy_CT_des2
for(unique_value in unique(O_H_TR_des2$trigram)){
  O_H_TR_des2[paste(unique_value)]<-ifelse(O_H_TR_des2$trigram==unique_value, 1,0)
}

View(O_H_TR_des2)

#Rassemblement des données####

OH_dataset <- merge(O_H_TR_des1, O_H_TR_des2, all=T)
OH_dataset_2 <- select(OH_dataset, -trigram)
OH_dataset_2 <- OH_dataset_2%>%
  group_by(NCTid)%>%
  summarise_all(funs(sum))
row.names(OH_dataset_2) <- OH_dataset_2$NCTid
OH_dataset_2 <- select(OH_dataset_2, -NCTid)
View(OH_dataset_2)

#Hierarchical clustering####

d_CTdes <- dist(OH_dataset_2)
hc_CTdes <- hclust(d_CTdes)
plot(hc_CTdes)
