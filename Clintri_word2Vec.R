require(tidyverse)
require(dplyr)

covid <- getStudyFields(expr = 'COVID', fields = c("NCTId", "DetailedDescription"), max_rnk = 180)
lal <- getStudyFields(expr = 'leukemia+AND+cancer', fields = c("NCTId", "DetailedDescription"), max_rnk = 180)

data <- bind_rows(covid, lal)
dim(data)
data$exp <- rep(c("COVID", "lal"), each=180)

data <- data %>% filter(DetailedDescription!="")
data$DetailedDescription <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", data$DetailedDescription))

library(tidytext)
td_data <- data %>%
  unnest_tokens(word, DetailedDescription, token = "words")

library(word2vec)
set.seed(123456789)
model <- word2vec::word2vec(td_data$word, dim = 50, iter = 20)
View(model)
embedding <-as.matrix(model)
fq_data <- td_data %>%
  count(NCTId, word)%>%
  group_by(NCTId)
fq_data
head(embedding)

typeof(mbddng)
mbddng <- as.data.frame(embedding)
library(data.table)
setDT(mbddng, keep.rownames = T)
mbddng<-mbddng %>%
  rename("word" = rn)
mbdding <- as.matrix(mbddng)
View(mbdding)

DT <- as.matrix(merge(td_data, mbdding, by="word"))
View(DT)

DT1 <- DT[,-c(1, 3)]
DT1<- as.data.frame(DT1) %>% group_by(NCTId)
dim(DT1)
DT1[[2]]
for(i in 3:51){
DT1[[i]] <- as.numeric(DT1[[i]])
}
summary(DT1)

DT2 <- summarize_all(DT1, funs(sum))
DT3 <- summarize_all(DT1, funs(mean))
library(Rtsne)

tsne_out1 <- Rtsne(DT2, pca=F, perplexity = 50,theta = 0.0)
plot(tsne_out1$Y, col = as.numeric(as.factor(data$exp)))

tsne_out2 <- Rtsne(DT3, pca=F, perplexity = 50,theta = 0.0)
plot(tsne_out1$Y, col = as.numeric(as.factor(data$exp)))
DT
library("spatstat")
DT <- ppx(DT)
Dist_DT <- nndist(DT, k = 5)
Dist_DT
