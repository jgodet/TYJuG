library(devtools)
devtools::install_github("rstudio/tensorflow", force = T)
library(tensorflow)
install_tensorflow(version="nightly")
library(keras)
library(tidytext)
library(dplyr)

#Test sur 1 étude
obj1 <- descriptionXML(file = "NCT03478891.xml")

#Encodage avec tidytext

tidyResume<- obj1 %>%
  unnest_tokens(word, textblock)

data("stop_words")

tidyResume <- tidyResume %>%
  anti_join(stop_words)
View(tidyResume)

O_H_TR <- tidyResume
for(unique_value in unique(O_H_TR$word)){
  O_H_TR[paste(unique_value)]<-ifelse(O_H_TR$word==unique_value, 1,0)
}

View(O_H_TR) #conserve infos sur NCTid et mot auquel est associe vecteur binaire

# Avec Keras
words <- tidyResume$word
dataset <- text_line_dataset(words)
summary(dataset)

tokenizer <- text_tokenizer(num_words = 1000)%>%
  fit_text_tokenizer(words)
one_hot_encoding <- texts_to_matrix(tokenizer, tidyResume, mode = "binary")
View(one_hot_encoding)

#Test sur liste d'études
descriptions <- clintri_descriptions_web("COVID", fields = "NCTid", max_rnk=20)

tidyResume<- descriptions %>%
  unnest_tokens(word, textblock)

tidyResume <- tidyResume %>%
  anti_join(stop_words)

View(tidyResume)

O_H_TR <- tidyResume
for(unique_value in unique(O_H_TR$word)){
  O_H_TR[paste(unique_value)]<-ifelse(O_H_TR$word==unique_value, 1,0)
}

View(O_H_TR)

textblock<-descriptions$textblock
tokenizer <- text_tokenizer(num_words = 10000)%>%
  fit_text_tokenizer(textblock)

tokenizer$word_index
tokenizer$word_counts

one_hot_encoding<-texts_to_matrix(tokenizer, textblock, mode = "binary")
View(one_hot_encoding)

text_seq <- texts_to_sequences(tokenizer, textblock)
summary(text_seq)

text_seq_pad <-pad_sequences(text_seq,maxlen = 810, padding = "post")
text_seq_pad

model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = 810, output_dim = 100)
summary(model)

model %>%
  layer_global_average_pooling_1d()
summary(model)

model %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

