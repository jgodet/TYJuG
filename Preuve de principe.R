#Preuve de principe

#####
require(tidyverse)
require(textTinyR)
require(fastText)
require(uwot)
require(ggplot2)
require(ggrepel)

#Récupération des données####
covid <- getStudyFields(expr = 'COVID', fields = c("NCTId", "DetailedDescription", "BriefSummary"), max_rnk = 100)
lal <- getStudyFields(expr = 'leukemia+AND+cancer', fields = c("NCTId", "DetailedDescription", "BriefSummary"), max_rnk = 100)
diab <- getStudyFields(expr = 'diabetes', fields = c("NCTId", "DetailedDescription", "BriefSummary"), max_rnk = 100)
park <- getStudyFields(expr = 'parkinson', fields = c("NCTId", "DetailedDescription", "BriefSummary"), max_rnk = 100)
flu <- getStudyFields(expr = 'influenza', fields = c("NCTId", "DetailedDescription", "BriefSummary"), max_rnk = 100)

aids <- getStudyFields(expr = 'AIDS', fields = c("NCTId", "DetailedDescription"), max_rnk = 80)
asth <- getStudyFields(expr = 'asthma', fields = c("NCTId", "DetailedDescription"), max_rnk = 80)
hypt <- getStudyFields(expr = 'hypertension', fields = c("NCTId", "DetailedDescription"), max_rnk = 80)
mala <- getStudyFields(expr = 'malaria', fields = c("NCTId", "DetailedDescription"), max_rnk = 80)
alz <- getStudyFields(expr = 'alzheimer', fields = c("NCTId", "DetailedDescription"), max_rnk = 80)

#Constitution de la dataframe####
DF <- bind_rows(covid, lal, diab, park, flu)
#aids, asth, hypt, mala, alz
DF$exp <- rep(c("COVID", "LAL", "diabetes", "parkinson", "Flu"), each=100)
#, "AIDS", "Asthma", "Hypertension", "Malaria", "alzheimer" 
DF$Description <- paste(DF$DetailedDescription, DF$BriefSummary, sep = " ")
View(head(DF))
DF <- DF[,c(1,4,5)]
DF <- DF %>% filter(Description!="")

#tokenization####
clust_vec <- textTinyR::tokenize_transform_vec_docs(object = DF$Description, as_token = T,
                                                   to_lower = T, 
                                                   remove_punctuation_vector = F,
                                                   remove_numbers = F, 
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

#Vectorisation####
save_dat <- textTinyR::tokenize_transform_vec_docs(object = DF$Description, as_token = T, 
                                                  to_lower = T, 
                                                  remove_punctuation_vector = F,
                                                  remove_numbers = F, trim_token = T, 
                                                  split_string = T, 
                                                  split_separator = " \r\n\t.,;:()?!//",
                                                  remove_stopwords = T, language = "english", 
                                                  min_num_char = 3, max_num_char = 100, 
                                                  stemmer = "porter2_stemmer", 
                                                  path_2folder = paste(getwd(),"/data_pdp1.1/",sep=""),
                                                  threads = 1,
                                                  verbose = T)

PATH_INPUT <- paste(getwd(),"/data_pdp1.1/output_token_single_file.txt",sep="")
PATH_OUT <- paste(getwd(),"/data_pdp1.1/rt_fst_model",sep="")

list_params <- list(command = 'cbow',
                   lr = 0.1,
                   dim = 200,
                   input = PATH_INPUT,
                   output = PATH_OUT,
                   verbose = 2,
                   thread = 6)

res <- fastText::fasttext_interface(list_params,
                                   path_output = file.path(PATH_OUT,".txt"),
                                   MilliSecs = 100)

init <- textTinyR::Doc2Vec$new(token_list = clust_vec$token, 
                              word_vector_FILE = paste(getwd(), "/data_pdp1.1/rt_fst_model.vec",sep=""),
                              print_every_rows = 5000, 
                              verbose = TRUE, 
                              copy_data = FALSE)

doc2_idf <- init$doc2vec_methods(method = "idf", global_term_weights = gl_term_w, threads = 6)

#Visualisation####

heatmap(doc2_idf)
dim(doc2_idf)
tsne_out2 <- Rtsne(doc2_idf, pca=F, perplexity = 30,theta = 0.0)

View(head(doc2_idf))

viz <-umap(doc2_idf,n_neighbors =20,n_threads =6)
as.vector(DF$NCTId)
rownames(viz) <-as.vector(DF$NCTId)

df  <-data.frame(word =gsub("//.+","",rownames(viz)),
                 upos =gsub(".+//","",rownames(viz)),
                 x =viz[,1],y =viz[,2],
                 stringsAsFactors =FALSE)

#p <- ggplot(df,aes(x =x,y =y, label = word))+
 # geom_text_repel()+theme_void()+
  #labs(title ="word2vec - in 2D using UMAP")
Trial <- as.factor(DF$exp)
p <- ggplot(df,aes(x =x,y =y, label = word))+
  geom_label(aes(fill = Trial), alpha=0.3, colour = "white" )+theme_void()+
  labs(title ="word2vec - in 2D using UMAP")+coord_flip()
p


#Plus proches voisins ####

dataW2V <- NULL
dataW2V$document <- DF$NCTId
dataW2V$x <- df$x
dataW2V$y <- df$y
dataW2V <- as.data.frame(dataW2V)
dim(dataW2V)

# Plus proches voisins avec KNN ####
#install.packages("FNN")
require("FNN")

row.names(dataW2V) <- dataW2V$document
dataW2V <- dataW2V[,-1]
neigh <- get.knn(dataW2V, k=10)
List <- dataW2V[c(15,neigh$nn.index[15,]),]

# Titre des études ####
Titles_W2V <- NCTidToTitles_web(as.data.frame(rownames(List)))
View(Titles_W2V)
save <- write.table(Titles_W2V, sep=";", file = paste0(getwd(), "/Titles_Random_Samples_W2V.csv", sep=""))
?write.csv()

# Confusion Matrix ####

mclust_W2V <- kmeans(viz,5)
table(DF$exp, mclust_W2V$cluster)
#'            1  2  3  4  5
#' COVID     67  3  2  1 27
#' diabetes  15 80  0  0  5
#' Flu        6  0 92  0  2
#' LAL        9  0  0 85  6
#' parkinson 42  6  0  0 52

GndTruth_W2V <- factor(DF$exp)
clustUmap_W2V <-  factor(mclust_W2V$cluster )
levels(clustUmap_W2V)  <-  c("COVID", "diabetes", "Flu", "LAL", "parkinson")

ConfMat_W2V <- caret::confusionMatrix(clustUmap_W2V,GndTruth_W2V)
save1 <- write.table(ConfMat_W2V[2], sep = ";",file = paste0(getwd(), "/Statistics_by_Class_W2V.csv", sep=""))
save2 <- write.table(ConfMat_W2V[4], sep = ";",file = paste0(getwd(), "/Statistics_by_Class_W2V.csv", sep=""))


#tokenization methode tidyText####

require(tidytext)
data("stop_words")
DF$Description <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", DF$Description))

BDD_td <- DF %>%
  unnest_tokens(word, Description, token = "words")%>%
  anti_join(stop_words)

BDD_frq <- BDD_td %>%
  count(NCTId, word)%>%
  group_by(NCTId)

BDD_dtm <- BDD_frq %>%
  cast_dtm(NCTId, word, n)

#LDA####
require("topicmodels")
BDD_lda <- LDA(BDD_dtm, k = 5)

#préparation des données ####
require("tidyr")
gamma_spread <- BDD_documents %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, gamma)
View(gamma_spread)
BDD_documents <- tidy(BDD_lda, matrix = "gamma")

require("Rtsne")
tsne_out1 <- Rtsne(gamma_spread, pca=F, perplexity = 30,theta = 0.0)
require("scatterpie")
gamma_spread$x <- tsne_out1$Y[,1]
gamma_spread$y <- tsne_out1$Y[,2]

#Scatterpie ####
BDD_scatterpie <- ggplot() + geom_scatterpie(aes(x=x, y = y, group = document),data = gamma_spread,
                                             cols = c(paste0("topic", 1:5, sep="")), color=NA, alpha = 0.3) + coord_equal()
BDD_scatterpie

#Plus proches voisins ####

dataLDA <- NULL
dataLDA$document <- gamma_spread$document
dataLDA$x <- tsne_out1$Y[,1]
dataLDA$y <- tsne_out1$Y[,2]
dataLDA <- as.data.frame(dataLDA)
dim(dataLDA)

# Plus proches voisins avec KNN ####
#install.packages("FNN")
require("FNN")

row.names(dataLDA) <- dataLDA$document
dataLDA <- dataLDA[,-1]

neigh <- get.knn(dataLDA, k=10)
List_LAL <- dataLDA[c(12,neigh$nn.index[12,]),]
List_PAR <- dataLDA[c(232,neigh$nn.index[232,]),]
List_DIA <- dataLDA[c(244,neigh$nn.index[244,]),]
List_FLU <- dataLDA[c(362,neigh$nn.index[362,]),]
List_COV <- dataLDA[c(450,neigh$nn.index[450,]),]
dataLDA[15,]
# Titre des études ####
Titles_LAL <- NCTidToTitles_web(as.data.frame(rownames(List_LAL)))
Titles_PAR <- NCTidToTitles_web(as.data.frame(rownames(List_PAR)))
Titles_DIA <- NCTidToTitles_web(as.data.frame(rownames(List_DIA)))
Titles_FLU <- NCTidToTitles_web(as.data.frame(rownames(List_FLU)))
Titles_COV <- NCTidToTitles_web(as.data.frame(rownames(List_COV)))
View(Titles)
save_lal <- write.table(Titles_LAL, sep=";", file = paste0(getwd(), "/Titles_LAL_LDA_tsne.csv", sep=""))
save_par <- write.table(Titles_PAR, sep=";", file = paste0(getwd(), "/Titles_PAR_LDA_tsne.csv", sep=""))
save_dia <- write.table(Titles_DIA, sep=";", file = paste0(getwd(), "/Titles_DIA_LDA_tsne.csv", sep=""))
save_flu <- write.table(Titles_FLU, sep=";", file = paste0(getwd(), "/Titles_FLU_LDA_tsne.csv", sep=""))
save_cov <- write.table(Titles_COV, sep=";", file = paste0(getwd(), "/Titles_COV_LDA_tsne.csv", sep=""))
?write.csv()

# Confusion Matrix

dataLDA$NCTID <- as.factor(rownames(dataLDA))
dataLDA_byNCTID <- group_by(dataLDA, NCTID)

DF_byNCTID <- dplyr::arrange(DF, NCTID)
dataLDA_byNCTID <- dplyr::arrange(dataLDA_byNCTID, NCTID)

#View(DF_byNCTID)
#View(dataLDA_byNCTID)

dataLDA_byNCTID <- dataLDA_byNCTID[,-3]

mclust_LDA <- kmeans(dataLDA_byNCTID, 5)
topic_distrib<-table(DF_byNCTID$exp, mclust_LDA$cluster)
write.table(topic_distrib, sep=";", file = paste0(getwd(), "/topic_distrib_LDA_tsne.csv", sep=""))
factor(mclust_LDA$cluster)
factor(DF_byNCTID$exp)
GndTruth <- factor(DF_byNCTID$exp)
clusttsne <-  factor(mclust_LDA$cluster)
levels(clusttsne)  <-  c("Flu", "COVID","LAL", "diabetes", "parkinson")
ConfMat_LDA<-caret::confusionMatrix(clusttsne,GndTruth)
save3 <- write.table(ConfMat_LDA[2], sep = ";",file = paste0(getwd(), "/Confusion_matrix_LDA_tsne.csv", sep=""))
save4 <- write.table(ConfMat_LDA[3], sep = ";",file = paste0(getwd(), "/Overall_stats_LDA_tsne.csv", sep=""))
save5 <- write.table(ConfMat_LDA[4], sep = ";",file = paste0(getwd(), "/Statistics_by_Class_LDA_tsne.csv", sep=""))

ConfMat_LDA
