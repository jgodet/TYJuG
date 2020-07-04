require(tidyverse)


covid <- getStudyFields(expr = 'COVID', fields = c("NCTId", "DetailedDescription"), max_rnk = 180)
lal <- getStudyFields(expr = 'leukemia+AND+cancer', fields = c("NCTId", "DetailedDescription"), max_rnk = 180)

data <- bind_rows(covid, lal)
dim(data)
data$exp <- rep(c("COVID", "lal"), each=180)

data <- data %>% filter(DetailedDescription!="")

#x <-tolower(data$DetailedDescription)
#x <- gsub(pattern = "\\n", replacement = " ", x)
# 
# library(word2vec)
# set.seed(123456789)
# model <-word2vec(x =x,type ="cbow",dim =50,iter =20)
# 
# embedding <-as.matrix(model)
# predict(model, c("covid19", "leukemia"))
# 
# 
# 
# library(uwot)
# viz <-umap(embedding,n_neighbors =10,n_threads =6)
# rownames(viz) <-rownames(embedding)
# head(viz,n =10)
# 
# library(ggplot2)
# library(ggrepel)
# df  <-data.frame(word =gsub("//.+","",rownames(viz)),
#                  upos =gsub(".+//","",rownames(viz)),
#                  x =viz[,1],y =viz[,2],
#                  stringsAsFactors =FALSE)
# df  <-df %>% sample_frac(size = .3)
# ggplot(df,aes(x =x,y =y,label =word))+
#   geom_text_repel()+theme_void()+
#   labs(title ="word2vec - adjectives in 2D using UMAP")
# 
# hc <- hclust(dist(viz))
# plot(hc, cex = 0.6, hang = -1)
# rect.hclust(hc, k = 4, border = 2:5)
# clust <- cutree(as.hclust(hc), k = 4)
#  rownames(embedding)[clust==1]
#  rownames(embedding)[clust==2]
#  rownames(embedding)[clust==3]
#  rownames(embedding)[clust==4]
# 
#  
# 
# xx <- embedding[rownames(embedding) %in%  c(str_split(x[25],pattern = " ")[[1]][1:25]),]
# yy <- embedding[rownames(embedding) %in%  c(str_split(x[255],pattern = " ")[[1]][1:25]),] 
#  word2vec_similarity(xx, yy)
#  

clust_vec = textTinyR::tokenize_transform_vec_docs(object = data$DetailedDescription, as_token = T,
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
unq = unique(unlist(clust_vec$token, recursive = F))
length(unq)

save_dat = textTinyR::tokenize_transform_vec_docs(object = data$DetailedDescription, as_token = T, 
                                                  to_lower = T, 
                                                  remove_punctuation_vector = F,
                                                  remove_numbers = F, trim_token = T, 
                                                  split_string = T, 
                                                  split_separator = " \r\n\t.,;:()?!//",
                                                  remove_stopwords = T, language = "english", 
                                                  min_num_char = 3, max_num_char = 100, 
                                                  stemmer = "porter2_stemmer", 
                                                  path_2folder = paste(getwd(),"/data/",sep=""),
                                                  threads = 1,
                                                  verbose = T)


#devtools::install_github('mlampros/fastText')

PATH_INPUT = paste(getwd(),"/data/output_token_single_file.txt",sep="")
PATH_OUT = paste(getwd(),"/data/rt_fst_model",sep="")

list_params = list(command = 'cbow',
                   lr = 0.1,
                   dim = 200,
                   input = PATH_INPUT,
                   output = PATH_OUT,
                   verbose = 2,
                   thread = 6)

res = fastText::fasttext_interface(list_params,
                                   path_output = file.path(PATH_OUT,".txt"),
                                   MilliSecs = 100)

init = textTinyR::Doc2Vec$new(token_list = clust_vec$token, 
                              word_vector_FILE = paste(getwd(), "/data/rt_fst_model.vec",sep=""),
                              print_every_rows = 5000, 
                              verbose = TRUE, 
                              copy_data = FALSE)

doc2_sum = init$doc2vec_methods(method = "sum_sqrt", threads = 6)
doc2_norm = init$doc2vec_methods(method = "min_max_norm", threads = 6)


utl = textTinyR::sparse_term_matrix$new(vector_data = data$DetailedDescription, file_data = NULL,
                                        document_term_matrix = TRUE)
tm = utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                     remove_numbers = F, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                     threads = 6, verbose = T)

gl_term_w = utl$global_term_weights()
doc2_idf = init$doc2vec_methods(method = "idf", global_term_weights = gl_term_w, threads = 6)
rows_cols = 1:5

doc2_sum[rows_cols, rows_cols]
doc2_norm[rows_cols, rows_cols]
doc2_idf[rows_cols, rows_cols]

doc2_com <- cbind(doc2_sum,doc2_norm, doc2_idf)

hc <- hclust(dist(doc2_idf[,]))
plot(hc, cex=.5)
clust <- cutree(as.hclust(hc), k =8)
rect.hclust(hc, k = 5, border = 2:9)

table(data$exp, clust)

heatmap(doc2_idf)

library(uwot)
viz <-umap(doc2_idf,n_neighbors =20,n_threads =6)
rownames(viz) <-paste(1:301)
head(viz,n =10)
library(ggplot2)
library(ggrepel)
df  <-data.frame(word =gsub("//.+","",rownames(viz)),
                 upos =gsub(".+//","",rownames(viz)),
                 x =viz[,1],y =viz[,2],
                 stringsAsFactors =FALSE)
df  <-df %>% sample_frac(size = .3)

couleurs <- c( "darkblue", "orange")[as.numeric(as.factor(data$exp))]

p <- ggplot(df,aes(x =x,y =y,label =word))+
  geom_text_repel()+theme_void()+
  labs(title ="word2vec - in 2D using UMAP")

Trial <- as.factor(data$exp)
p + theme_void() + geom_label(aes(fill = Trial), colour = "white" ) + 
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))

mclust <- kmeans(viz,2)
table(data$exp, mclust$cluster)

GndTruth <- factor(data$exp)
clustUmap <-  factor(mclust$cluster )
levels(clustUmap)  <-  c("COVID", "lal")
caret::confusionMatrix(clustUmap,GndTruth)

                       