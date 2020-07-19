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
#fonction de récupération
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
#récupération
require(dplyr)
require(parallel)
des.nVec <- mclapply(Sample, FUN = descriptionXML, mc.cores = 10)
View(des.nVec)

#Réarrangement des données ####
require(data.table)
table <- rbindlist(des.nVec, fill=T) #regroupe la liste de tables en une table unique
table <- table %>%
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
table <- distinct(table[,-1])
head(table)
#Tokenization Méthode tidyText ####
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

#Tokenization Méthode textTinyR ####
BDD_clust_vec <- textTinyR::tokenize_transform_vec_docs(object = table$text, as_token = T,
                                                        to_lower = T, 
                                                        remove_punctuation_vector = F,
                                                        remove_numbers = T, 
                                                        trim_token = T,
                                                        split_string = T,
                                                        split_separator = " \r\n\t.,;:()?!//", 
                                                        remove_stopwords = T,
                                                        language = "english", 
                                                        min_num_char = 3, 
                                                        max_num_char = 100,
                                                        stemmer = "porter2_stemmer", 
                                                        threads = 4,
                                                        verbose = T)
summary(BDD_clust_vec)
#unq <- unique(unlist(clust_vec$token, recursive = F))
#length(unq)
#[1] 6033

save_dat <- textTinyR::tokenize_transform_vec_docs(object = table$text, as_token = T, 
                                                  to_lower = T, 
                                                  remove_punctuation_vector = F,
                                                  remove_numbers = T, 
                                                  trim_token = T, 
                                                  split_string = T, 
                                                  split_separator = " \r\n\t.,;:()?!//",
                                                  remove_stopwords = T, 
                                                  language = "english", 
                                                  min_num_char = 3, 
                                                  max_num_char = 100, 
                                                  stemmer = "porter2_stemmer", 
                                                  path_2folder = paste(getwd(),"/data1/",sep=""),
                                                  threads = 1,
                                                  verbose = T)

#vectorisation avec fastText
PATH_INPUT <- paste(getwd(),"/data1/output_token_single_file.txt",sep="")
PATH_OUT <- paste(getwd(),"/data1/rt_fst_model",sep="")

list_params <- list(command = 'cbow',
                   lr = 0.1,
                   dim = 200,
                   input = PATH_INPUT,
                   output = PATH_OUT,
                   verbose = 2,
                   thread = 6)

require(fastText)

res <- fastText::fasttext_interface(list_params,
                                   path_output = file.path(PATH_OUT,".txt"),
                                   MilliSecs = 100)

init <- textTinyR::Doc2Vec$new(token_list = BDD_clust_vec$token, 
                              word_vector_FILE = paste(getwd(), "/data/rt_fst_model.vec",sep=""),
                              print_every_rows = 5000, 
                              verbose = TRUE, 
                              copy_data = FALSE)

doc2_sum <- init$doc2vec_methods(method = "sum_sqrt", threads = 6)
doc2_norm <- init$doc2vec_methods(method = "min_max_norm", threads = 6)

#Création de term Matrix
utl <- textTinyR::sparse_term_matrix$new(vector_data = table$text, file_data = NULL,
                                        document_term_matrix = TRUE)

tm <- utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                     remove_numbers = F, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                     threads = 6, verbose = T)

gl_term_w <- utl$global_term_weights()

doc2_idf <- init$doc2vec_methods(method = "idf", global_term_weights = gl_term_w, threads = 6)
doc2_com <- cbind(doc2_sum,doc2_norm, doc2_idf)

#Hierarchical clustering####
hc <- hclust(dist(doc2_idf[,]))
plot(hc, cex=.5)
clust <- cutree(as.hclust(hc), k =8)
rect.hclust(hc, k = 5, border = 2:9)
heatmap(doc2_idf)

#Umap ####
require("uwot")
viz <-umap(doc2_idf,n_neighbors =20,n_threads =6)
rownames(viz) <-paste(1:400)
require(ggplot2)
require(ggrepel)
df  <-data.frame(word =gsub("//.+","",rownames(viz)),
                 upos =gsub(".+//","",rownames(viz)),
                 x =viz[,1],y =viz[,2],
                 stringsAsFactors =FALSE)

p <- ggplot(df,aes(x =x,y =y,label =word))+
  geom_text_repel()+theme_void()+
  labs(title ="word2vec - in 2D using UMAP")

p 

#nearest neighbors
require("FNN")
data <- df[,3:4]
row.names(data) <- table$NCTid
#head(data)

neigh <- get.knn(data, k=5)
#head(neigh)

set.seed(123456)
x <- sample(1:400, size = 1)
#neigh$nn.index[x,]
List <- data[c(343,neigh$nn.index[343,]),]
#rownames(List)

# Titre des études
Titles <- NCTidToTitles_web(as.data.frame(rownames(List)))
View(Titles)
