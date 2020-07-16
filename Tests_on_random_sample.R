#Retrieve subject from random sample
require(XML)

path <- "/Users/taiohy/documents/mes documents/Fac/Projet Professionnel/Stage/Espace de travail/AllPublicXML"

#Liste de l'ensemble des fichiers CT ####
listF <- list.files(path, recursive = T, full.names = T, pattern = ".xml")
listF<-unlist(listF)
length(listF) #338999

#Sélection d'un échantillon ####
set.seed(78952)
Sample <- sample(listF, size = 400)

#Récupération des sommaires et des descriptions détaillées ####
descriptionXML <- function (path){
  parsedXML <- XML::xmlParse(file = path)
  nodesetDes <- XML::getNodeSet(doc = parsedXML, path= c("//brief_summary","//detailed_description"))
  XMLdfDes <- XML::xmlToDataFrame(doc=parsedXML, nodes = nodesetDes)
  if(!is.null(XMLdfDes[1,1])){
    NCTidNodes <- XML::getNodeSet(doc = parsedXML,path = "//id_info/nct_id")
    id_info_df <- XML::xmlToDataFrame(doc = parsedXML, nodes = NCTidNodes)
    XMLdfDes$NCTid=id_info_df[1,1]
    return(XMLdfDes)
  }
  else XMLdfDes <- NULL
}

#Réarrangement des données ####
require(dplyr)
require(parallel)
des.nVec <- mclapply(Sample, FUN = descriptionXML, mc.cores = 10)
View(des.nVec)
require(data.table)
table <- rbindlist(des.nVec, fill=T)
table <- table %>%
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
table <- distinct(table[,-1])

#Tokenization ####
require(tidytext)
data("stop_words")
table$text <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", table$text))

BDD_td <- table %>%
  unnest_tokens(word, text, token = "words")%>%
  anti_join(stop_words)

BDD_frq <- BDD_td %>%
  count(NCTid, word)%>%
  group_by(NCTid)

BDD_dtm <- BDD_frq %>%
  cast_dtm(NCTid, word, n)

#LDA####
require("topicmodels")
BDD_lda <- LDA(BDD_dtm, k = 10)

#préparation des données ####
gamma_spread <- BDD_documents %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, gamma)

BDD_documents <- tidy(BDD_lda, matrix = "gamma")

require("Rtsne")
tsne_out1 <- Rtsne(gamma_spread, pca=F, perplexity = 30,theta = 0.0)
require("scatterpie")
gamma_spread$x <- tsne_out1$Y[,1]
gamma_spread$y <- tsne_out1$Y[,2]

#Scatterpie ####
BDD_scatterpie <- ggplot() + geom_scatterpie(aes(x=x, y = y, group = document),data = gamma_spread,
                                             cols = c(paste0("topic", 1:10, sep="")), color=NA, alpha = 0.3) + coord_equal()
BDD_scatterpie

#Plus proches voisins ####

data <- NULL
data$document <- gamma_spread$document
data$x <- tsne_out1$Y[,1]
data$y <- tsne_out1$Y[,2]
data <- as.data.frame(data)
dim(data)

#Compliqué pour rien ####
# test avec nombre de voisins = 10
#require(spatstat)
#dist <- nndist(data[c(2,3)], k=10)

# Test avec le document situé sur la xe ligne
#set.seed(90776783)
#x <- sample(1:400, size = 1)
#data[x,]
#dist[x]
#Boucle pour comparer longueurs des vecteurs
#Neighbor <- NULL
#for(i in 1:nrow(data)){
#  xN <- data[i,]$x
#  yN <- data[i,]$y
#  xA <- data[x,]$x
#  yA <- data[x,]$y
#  vec <- sqrt((xN-xA)^2+(yN-yA)^2)
#  if(vec <= dist[x]){
#    NCTid <- data[i,]$document
#    Neighbor$NCTid[i] <- NCTid
#  }
#  
#}
#
#Neighbor$NCTid <- Neighbor$NCTid[!is.na(Neighbor$NCTid)]
#
#NCTid.list <- as.data.frame(Neighbor)
#View(NCTid.list)
#List <- NCTidToTitles_web(NCTid.list)
#View(List)
#
#XY <- ppp(x = data$x, y = data$y, owin(xrange = range(data$x), yrange = range(data$y)))
#dist <- nndist(XY, k=5)

# Plus proches voisins avec KNN ####
#install.packages("FNN")
require("FNN")

row.names(data) <- data$document
data <- data[,-1]
#head(data)

neigh <- get.knn(data, k=5)
#head(neigh)

set.seed(90776783)
x <- sample(1:400, size = 1)
#neigh$nn.index[x,]
List <- data[c(x,neigh$nn.index[x,]),]
#rownames(List)

# Titre des études ####
Titles <- NCTidToTitles_web(as.data.frame(rownames(List)))
View(Titles)

#data[x,]
#                   x         y
#NCT04217993 18.41138 -37.01032