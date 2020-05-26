#Tidy text tests

descriptionXML <- function (file){
  parsedXML <- XML::xmlParse(file = file)
  nodesetDes <- XML::getNodeSet(doc = parsedXML, path= c("//brief_summary","//detailed_description"))
  XMLdfDes <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetDes)
  NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
  id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
  for(i in 1:nrow(XMLdfDes)){
    XMLdfDes$NCTid=id_info_df[1,1]
  }
  return(XMLdfDes)
}

obj<- descriptionXML(file = "NCT04360356.xml")
View(obj)

install.packages("tidytext")
install.packages("stringr")
library("tidytext")
library(dplyr)
library("stringr")

tidyResume<- obj[1,] %>%
  unnest_tokens(word, textblock)
wordTb<-tidyResume %>%
  count(word, sort = T)

data("stop_words")

tidyResume <- tidyResume %>%
  anti_join(stop_words)

library("ggplot2")

mostCommonWords <- tidyResume %>%
  count(word,sort = T) %>%
  filter(n>0)%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

mostCommonWords

#n-grams

resume_bigrams <- obj[1,]%>%
  unnest_tokens(bigram, textblock, token = "ngrams", n=2)
resume_bigrams

resume_bigrams %>%
  count(bigram, sort = T)

library(tidyr)

resume_bigrams_sep <- resume_bigrams %>%
  separate(bigram, c("word1", "word2"), sep=" ")

resume_bigrams_filtered <- resume_bigrams_sep %>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)
resume_bigrams_filtered

resume_bigrams_counts <- resume_bigrams_filtered %>%
  count(word1, word2, sort = T)

resume_bigrams_counts

resume_bigrams_united <- resume_bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

resume_bigrams_united

mostCommonBigrams <- resume_bigrams_united %>%
  count(bigram,sort = T) %>%
  filter(n>0)%>%
  mutate(bigram = reorder(bigram,n))%>%
  ggplot(aes(bigram,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

mostCommonBigrams
