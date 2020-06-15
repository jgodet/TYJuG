#Clintri_tsne_test

library("dplyr")
library("tidyr")
library("keras")

#Préparation des données#####
CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 100)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 100)
CT_des3 <- clintri_descriptions_web("Parkinson", max_rnk = 100)
CT_des4 <- clintri_descriptions_web("Flu", max_rnk = 100)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
CT_des3$Grp <- "Parkinson"
CT_des4$Grp <- "Flu"
BDD <- bind_rows(CT_des1, CT_des2, CT_des3, CT_des4) 
BDD <- BDD %>% 
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
BDD <- distinct(BDD[,-1])
View(BDD)

##Première méthode####
tokenizer <- text_tokenizer(num_words = 9000, filters = "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n1234567890") %>% 
  fit_text_tokenizer(BDD$text)
mat <- texts_to_matrix(tokenizer = tokenizer, texts =  BDD$text, mode = "tfidf")
# length(tokenizer$index_word)
# [1] 8431
res <- apply(mat, MARGIN = 2, FUN = mean)
mat <- mat[,res>0]
# dim(mat)
# [1] 400 8431
colnames(mat) <- names(tokenizer$word_index)
rownames(mat) <- paste0(BDD$NCTid, " ", BDD$Grp)

##Seconde méthode####
data("stop_words")

tidy_BDD <- BDD %>%
  unnest_tokens(word, text, token = "words") %>%
  anti_join(stop_words) %>%
  unite(id, NCTid, Grp, sep = " ") %>%
  group_by(id)
#One hot encoding "manuel"
OH_BDD <- tidy_BDD
for(unique_value in unique(OH_BDD$word)){
    OH_BDD[paste(unique_value)]<-ifelse(OH_BDD$word==unique_value, 1,0)
}
OH_BDD$id <- tidy_BDD$id
OH_BDD <- summarise_all(OH_BDD[,-2], funs(sum))
mat2 <- OH_BDD[,-1]
row.names(mat2) <- OH_BDD$id
# dim(mat2)
# [1] 400 8497

#tSNE####
tsne_out1 <- Rtsne(mat, pca=F, perplexity = 133,theta = 0.0)
plot(tsne_out1$Y,col=as.numeric(as.factor(BDD$Grp)), asp=1)

tsne_out2 <- Rtsne(mat2, pca = F, perplexity = 133, theta = 0.0)
plot(tsne_out2$Y,col=as.numeric(as.factor(BDD$Grp)), asp=1)
#Au delà de 2 critères, le modèle a du mal a discriminer les groupes