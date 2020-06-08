library(devtools)
devtools::install_github("rstudio/tensorflow", force = T)
library(tensorflow)
install_tensorflow(version="nightly")
library(keras)

#Test sur 1 étude
obj1 <- descriptionXML(file = "NCT03478891.xml")
textblock <- obj1$textblock
dataset <- text_line_dataset(textblock)
summary(dataset)

tokenizer <- text_tokenizer(num_words = 1000)%>%
  fit_text_tokenizer(textblock)
one_hot_encoding <- texts_to_matrix(tokenizer, textblock, mode = "binary")
one_hot_encoding

#Test sur liste d'études
descriptions <- clintri_descriptions_web("COVID", fields = "NCTid", max_rnk=20)
textblock<-descriptions$textblock
tokenizer <- text_tokenizer(num_words = 1000)%>%
  fit_text_tokenizer(textblock)
one_hot_encoding<-texts_to_matrix(tokenizer, textblock, mode = "binary")
View(one_hot_encoding)
dataset <- text_line_dataset(textblock)
dataset

