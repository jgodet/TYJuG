#Latent Dirichlet Allocation (LDA) with tidytext

install.packages("scatterpie")

CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 40)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 40)
CT_des3 <- clintri_descriptions_web("Parkinson", max_rnk = 40)
CT_des4 <- clintri_descriptions_web("Flu", max_rnk = 40)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
CT_des3$Grp <- "Parkinson"
CT_des4$Grp <- "Flu"

library("dplyr")
library("tidytext")
library("tidyr")
library("topicmodels")
library("ggplot2")
library("purrr")

#BDD <- bind_rows(CT_des1, CT_des2)
BDD <- bind_rows(CT_des1, CT_des2, CT_des3, CT_des4) 
BDD <- BDD %>% 
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
BDD <- distinct(BDD[,-1])

data("stop_words")
BDD$text <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", BDD$text))
head(BDD$text)

BDD_td <- BDD %>%
  unnest_tokens(word, text, token = "words")%>%
  anti_join(stop_words)

BDD_frq <- BDD_td %>%
  count(NCTid, word)%>%
  group_by(NCTid)

BDD_dtm <- BDD_frq %>%
  cast_dtm(NCTid, word, n)

BDD_lda <- LDA(BDD_dtm, k = 4)


BDD_topics <- tidy(BDD_lda, matrix = "beta")
BDD_topics

BDD_top_terms <- BDD_topics %>%
  group_by(topic) %>%
  top_n(10, beta)%>%
  ungroup() %>%
  arrange(topic,-beta)

Top_term_plot <- BDD_top_terms %>%
  mutate(term = reorder_within(term, beta, topic))%>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free")+
  coord_flip()+
  scale_x_reordered()

BDD_documents <- tidy(BDD_lda, matrix = "gamma")
BDD_documents


library("Rtsne")
tsne_out1 <- Rtsne(gamma_spread, pca=F, perplexity = 50,theta = 0.0)
tsne_out1
plot(tsne_out1$Y, asp=1)
#tsne avec couleurs selon topic détecté pour chaque étude
plot(tsne_out1$Y,col=as.numeric(as.factor(BDD$Grp)), asp=1)

beta_spread <- BDD_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001)

beta_spread

gamma_spread <- BDD_documents %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, gamma) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001)

gamma_spread

library("tidyr")
library("scatterpie")

gamma_spread$x <- tsne_out1$Y[,1]
gamma_spread$y <- tsne_out1$Y[,2]
BDD_scatterpie <- ggplot() + geom_scatterpie(aes(x=x, y = y, group = document),data = gamma_spread,
                  cols = c("topic1", "topic2", "topic3", "topic4"), color=NA, alpha = 0.3) + coord_equal()

BDD_scatterpie

length(tsne_out1$Y[,1])
gamma_spread
length(tsne_out1$Y[,2])
