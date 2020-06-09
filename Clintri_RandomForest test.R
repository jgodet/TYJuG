#RandomForest clinical trials
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(randomForest)
library(caret)
library(e1071)

#Préparation db1 ####
db1<-clintri_descriptions_web(expr = "COVID", max_rnk = 30)
word_db1 <- db1 %>%
  unnest_tokens(word, textblock, token = "words")

#bigram_db1 <- db1 %>%
#  unnest_tokens(bigram, textblock, token = "ngrams", n=2)
#bigram_db1_sep <- bigram_db1 %>%
#  separate(bigram, c("mot1", "mot2"), sep = " ")
#bigram_db1_filtered <- bigram_db1_sep %>%
#  filter(!mot1 %in% stop_words$word)%>%
#  filter(!mot2 %in% stop_words$word)
#bigram_db1_united <- bigram_db1_filtered %>%
#  unite(bigram, mot1, mot2, sep = " ")

trigram_db1 <- db1 %>%
  unnest_tokens(trigram, textblock, token = "ngrams", n=3)
trigram_db1_sep <- trigram_db1 %>%
  separate(trigram, c("mot1", "mot2", "mot3"), sep = " ")
trigram_db1_filtered <- trigram_db1_sep %>%
  filter(!mot1 %in% stop_words$word)%>%
  filter(!mot2 %in% stop_words$word)%>%
  filter(!mot3 %in% stop_words$word)
trigram_db1_united <- trigram_db1_filtered %>%
  unite(trigram, mot1, mot2, mot3, sep = " ")

summary(word_db1)
summary(trigram_db1_united)
#tidy_db1 <- merge(word_db1, bigram_db1_united, by = "NCTid", all=T, allow.cartesian = T)
tidy_db1 <- merge(word_db1, trigram_db1_united, by = "NCTid", all = T, allow.cartesian = T)
tidy_db1$subject <- "COVID"
summary(tidy_db1)

#Préparation db2 ####
db2<-clintri_descriptions_web(expr = "Acute+lymphoblastic+leukemia", max_rnk = 30)
word_db2 <- db2 %>%
  unnest_tokens(word, textblock, token = "words")

#bigram_db2 <- db2 %>%
#  unnest_tokens(bigram, textblock, token = "ngrams", n=2)
#bigram_db2_sep <- bigram_db2 %>%
#  separate(bigram, c("mot1", "mot2"), sep = " ")
#bigram_db2_filtered <- bigram_db2_sep %>%
#  filter(!mot1 %in% stop_words$word)%>%
#  filter(!mot2 %in% stop_words$word)
#bigram_db2_united <- bigram_db2_filtered %>%
#  unite(bigram, mot1, mot2, sep = " ")

trigram_db2 <- db2 %>%
  unnest_tokens(trigram, textblock, token = "ngrams", n=3)
trigram_db2_sep <- trigram_db2 %>%
  separate(trigram, c("mot1", "mot2", "mot3"), sep = " ")
trigram_db2_filtered <- trigram_db2_sep %>%
  filter(!mot1 %in% stop_words$word)%>%
  filter(!mot2 %in% stop_words$word)%>%
  filter(!mot3 %in% stop_words$word)
trigram_db2_united <- trigram_db2_filtered %>%
  unite(trigram, mot1, mot2, mot3, sep = " ")

#tidy_db2 <- merge(word_db2, bigram_db2_united, by = "NCTid", all=T, allow.cartesian = T)
tidy_db2 <- merge(word_db2, trigram_db2_united, by = "NCTid", all = T, allow.cartesian = T)
tidy_db2$subject <- "LAL"

ct_des_united <- rbind(tidy_db1, tidy_db2)
ct_des_united2 <- select(ct_des_united,-NCTid)
summary(ct_des_united2)
#Préparation modèle Random Forest ####

set.seed(3148)

train_ctRF <- sample_frac(ct_des_united2, 0.10)
test_ctRF <- anti_join(ct_des_united, train_ctRF)

train_ctRF$subject <- as.character(train_ctRF$subject)
train_ctRF$subject <- as.factor(train_ctRF$subject)

test_ctRF$subject <- as.character(test_ctRF$subject)
test_ctRF$subject <- as.factor(test_ctRF$subject)

levels(train_ctRF$subject)
levels(test_ctRF$subject)

model_ctRF <- randomForest(data = train_ctRF, 
                           subject~., 
                           ntree = 500,
                           na.action = na.omit)
model_ctRF
hist(model_ctRF$oob.times)
model_ctRF$votes[1:10,]

test_ctRF$predicted <- predict(model_ctRF, test_ctRF)

conf_ctRF <- confusionMatrix(data = test_ctRF$predicted, reference = test_ctRF$subject)
conf_ctRF

varImpPlot(model_ctRF)
