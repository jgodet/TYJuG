require(tidyverse)


covid <- getStudyFields(expr = 'COVID', fields = c("NCTId", "DetailedDescription"), max_rnk = 150)
lal <- getStudyFields(expr = 'leukemia+AND+cancer', fields = c("NCTId", "DetailedDescription"), max_rnk = 150)

data <- bind_rows(covid, lal)
dim(data)
data$exp <- rep(c("COVID", "lal"), each=150)

x <-tolower(data$DetailedDescription)
x <- gsub(pattern = "\\n", replacement = " ", x)

library(word2vec)
set.seed(123456789)
model <-word2vec(x =x,type ="cbow",dim =50,iter =20)

embedding <-as.matrix(model)
predict(model, c("covid19", "leukemia"))



library(uwot)
viz <-umap(embedding,n_neighbors =10,n_threads =6)
rownames(viz) <-rownames(embedding)
head(viz,n =10)

library(ggplot2)
library(ggrepel)
df  <-data.frame(word =gsub("//.+","",rownames(viz)),
                 upos =gsub(".+//","",rownames(viz)),
                 x =viz[,1],y =viz[,2],
                 stringsAsFactors =FALSE)
df  <-df %>% sample_frac(size = .3)
ggplot(df,aes(x =x,y =y,label =word))+
  geom_text_repel()+theme_void()+
  labs(title ="word2vec - adjectives in 2D using UMAP")

hc <- hclust(dist(viz))
plot(hc, cex = 0.6, hang = -1)
rect.hclust(hc, k = 4, border = 2:5)
clust <- cutree(as.hclust(hc), k = 4)
 rownames(embedding)[clust==1]
 rownames(embedding)[clust==2]
 rownames(embedding)[clust==3]
 rownames(embedding)[clust==4]

 

xx <- embedding[rownames(embedding) %in%  c(str_split(x[25],pattern = " ")[[1]][1:25]),]
yy <- embedding[rownames(embedding) %in%  c(str_split(x[255],pattern = " ")[[1]][1:25]),] 
 word2vec_similarity(xx, yy)
 