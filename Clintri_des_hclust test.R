data("mtcars")
View(mtcars)
as.matrix(mtcars)
d<-dist(as.matrix(mtcars))
d
hc<- hclust(d)
plot(hc)

library(keras)
library(tidyr)
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
plot(hc_CTdes,hang = 1)



#alternative JG
#########################
#J'en prends 10 de plus pour cross valider ensuite
CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 40)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 40)
str(CT_des1)

CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 

daten <- bind_rows(CT_des1, CT_des2) 
daten <- daten %>% mutate_at(c("NCTid", "Grp"), factor)

#regrouper les id identiques
daten <- daten %>% group_by(NCTid) %>% mutate(text = paste0(textblock, collapse = "")) 

daten2 <- distinct(daten[,-1])
dim(daten2)

#je garde que les 30:30 initiaux comme toi
datenVal <- daten2[c(31:40, 71:80),]
daten2 <- daten2[c(-40:-31, -80:-71),]

tokenizer <- text_tokenizer(num_words = 4000, filters = "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n1234567890") %>% 
  fit_text_tokenizer(daten2$text)


# tokenizer$word_counts 
# tokenizer$word_index

mat <- texts_to_matrix(tokenizer = tokenizer, texts =  daten2$text, mode = "tfidf")

dim(mat)
res <- apply(mat, MARGIN = 2, FUN = mean)

mat <- mat[,res>0]
dim(mat)
colnames(mat) <- names(tokenizer$word_index)

distMat <- dist(mat)
hc <- hclust(distMat)
plot(hc,hang = 1)
#de manière évidente ça marche pas (matrice creuse et vect non informatifs?)


require(FactoMineR)
res.pca <- PCA(mat,scale.unit=TRUE, ncp=2, graph = T)

require(Rtsne)
tsne_out <- Rtsne(mat,pca=FALSE,perplexity=15,theta=0.0) # Run TSNE
plot(tsne_out$Y,col=as.numeric(as.factor(daten2$Grp)), asp=1)
#pas assez de lignes ici


#import the package
library(randomForest)
dataRf <- as.data.frame(mat)
grp <- factor(daten2$Grp)
# Perform training:
rf_classifier = randomForest(grp ~ ., data=dataRf, ntree=100, mtry=2, importance=TRUE)
#shitty

require(e1071)
x <- mat
y <-factor(daten2$Grp)
model <- svm(x, y)
print(model)
summary(model)
pred <- predict(model, x)
table(pred, y)

matVal <- texts_to_matrix(tokenizer = tokenizer, texts =  datenVal$text, mode = "tfidf")
dim(matVal)

matVal <- matVal[,res>0]
xVal <- matVal
predVal <- predict(model, xVal)
table(predVal, datenVal$Grp)

#Bon ne c'est pas terrible mais y a rien d'optimisé


