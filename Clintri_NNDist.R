#Clintri_NNDist

library("dplyr")
library("tidytext")
library("tidyr")
library("topicmodels")
library("ggplot2")
library("purrr")
library("Rtsne")

#Préparation des données avec LDA et t-sne
CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 50)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 50)
CT_des3 <- clintri_descriptions_web("Parkinson", max_rnk = 50)
CT_des4 <- clintri_descriptions_web("Flu", max_rnk = 50)

BDD <- bind_rows(CT_des1, CT_des2, CT_des3, CT_des4) 
BDD <- BDD %>% 
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
BDD <- distinct(BDD[,-1])
BDD$text <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", BDD$text))
data("stop_words")
BDD_td <- BDD %>%
  unnest_tokens(word, text, token = "words")%>%
  anti_join(stop_words)

BDD_frq <- BDD_td %>%
  count(NCTid, word)%>%
  group_by(NCTid)

BDD_dtm <- BDD_frq %>%
  cast_dtm(NCTid, word, n)

BDD_lda <- LDA(BDD_dtm, k = 4)
BDD_documents <- tidy(BDD_lda, matrix = "gamma")
gamma_spread <- BDD_documents %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, gamma) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001)

tsne_out1 <- Rtsne(gamma_spread, pca=F, perplexity = 10,theta = 0.0)

gamma_spread$x <- tsne_out1$Y[,1]
gamma_spread$y <- tsne_out1$Y[,2]
# data <- NULL
data <- gamma_spread[c(1, 6, 7)]
data

#scatterpie
BDD_scatterpie <- ggplot() + 
  geom_scatterpie(aes(x=x, y = y, group = document),data = gamma_spread,
                  cols = c("topic1", "topic2", "topic3", "topic4"),
                  color=NA,
                  alpha = 0.3) +
  coord_equal()

BDD_scatterpie

#install.packages("spatstat")
library("spatstat")

# NNDist : permet de calculer la distance des plus proches voisins pour chaque point
# k = nombre de voisins 

# test avec nombre de voisins = 5
dist <- nndist(data[c(2,3)], k=5)

# Test avec le document situé sur la 7e ligne
data[7,]
dist[7]
#Boucle pour comparer longueurs des vecteurs
Neighbor <- NULL
for(i in 1:nrow(data)){
  xN <- data[i,]$x
  yN <- data[i,]$y
  xA <- data[7,]$x
  yA <- data[7,]$y
  vec <- sqrt((xN-xA)^2+(yN-yA)^2)
  if(vec <= dist[7]){
  NCTid <- data[i,]$document
  Neighbor$NCTid[i] <- NCTid
  }
  
}

Neighbor$NCTid <- Neighbor$NCTid[!is.na(Neighbor$NCTid)]

NCTid.list <- as.data.frame(Neighbor)

List <- NCTidToTitles_web(NCTid.list)
View(List)
