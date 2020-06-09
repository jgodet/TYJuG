#RandomForest clinical trials
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(randomForest)
library(caret)
library(e1071)

db1<-clintri_descriptions_web(expr = "COVID", max_rnk = 30)
tidy_db1 <- db1 %>%
  tidytext::unnest_tokens(bigram, textblock, token = "ngrams", n=2)
tidy_db1$subject <- "COVID"

db2<-clintri_descriptions_web(expr = "Acute+lymphoblastic+leukemia", max_rnk = 30)
tidy_db2 <- db2 %>%
  tidytext::unnest_tokens(bigram, textblock, token = "ngrams", n=2)
tidy_db2$subject <- "LAL"

ct_des <- rbind(tidy_db1, tidy_db2)
data("stop_words")
ct_des_sep <- ct_des %>%
  separate(bigram, c("word1", "word2"), sep=" ")
filtered_ct_des_sep <- ct_des_sep %>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)
ct_des_united <- filtered_ct_des_sep %>%
  unite(bigram, word1, word2, sep = " ")

set.seed(3145)

train_ctRF <- sample_frac(ct_des_united, 0.80)
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
