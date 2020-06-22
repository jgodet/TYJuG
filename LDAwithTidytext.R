#Latent Dirichlet Allocation (LDA) with tidytext

CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 10)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 10)
CT_des3 <- clintri_descriptions_web("Parkinson", max_rnk = 10)
CT_des4 <- clintri_descriptions_web("Flu", max_rnk = 10)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
CT_des3$Grp <- "Parkinson"
CT_des4$Grp <- "Flu"
library("dplyr")
BDD <- bind_rows(CT_des1, CT_des2, CT_des3, CT_des4) 
BDD <- BDD %>% 
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
BDD <- distinct(BDD[,-1])
library("tidytext")
library("tidyr")
data("stop_words")

BDD_td <- BDD %>%
  unnest_tokens(word, text, token = "words")%>%
  anti_join(stop_words)

BDD_frq <- BDD_td %>%
  count(NCTid, word)%>%
  group_by(NCTid)
View(BDD_frq)
View(BDD_td)

library(topicmodels)

#data("AssociatedPress")
#AssociatedPress

BDD_dtm <- BDD_frq %>%
  cast_dtm(NCTid, word, n)

BDD_lda <- LDA(BDD_dtm, k = 4, control = list(seed = 1234))
BDD_lda

BDD_topics <- tidy(BDD_lda, matrix = "beta")
BDD_topics

library("ggplot2")

BDD_top_terms <- BDD_topics %>%
  group_by(topic) %>%
  top_n(10, beta)%>%
  ungroup() %>%
  arrange(topic,-beta)

BDD_top_terms %>%
  mutate(term = reorder_within(term, beta, topic))%>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free")+
  coord_flip()+
  scale_x_reordered()

#library("Rtsne")
#tsne_out1 <- Rtsne(BDD_topics, pca=F, perplexity = 30,theta = 0.0)
#plot(tsne_out1$Y,col=as.numeric(as.factor(BDD$Grp)), asp=1)