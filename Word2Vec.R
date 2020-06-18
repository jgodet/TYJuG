#Word2vec
#Fait crasher R, marche pas #####
devtools::install_github("mukul13/rword2vec")
library("rword2vec")
ls("package:rword2vec")
library("dplyr")

CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 100)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 100)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
BDD <- bind_rows(CT_des1, CT_des2) 
BDD <- BDD %>% 
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
BDD <- distinct(BDD[,-1])
text <- BDD$text
text1 <- paste0(text, collapse = " ")
View(text1)
model <- word2vec(train_file = text1, output_file = "vec.bin", binary = 1)


# Word embedding tests, fait planter R #####
# inpiré de ce tuto : https://blogs.rstudio.com/ai/posts/2017-12-22-word-embeddings-with-keras/
# Mauvaise préparation initiale des donnée ?

library("dplyr")
library("readr")
library("stringr")

CT_des1 <- clintri_descriptions_web("COVID", max_rnk = 100)
CT_des2 <- clintri_descriptions_web("Acute+lymphoblastic+leukemia", max_rnk = 100)
CT_des1$Grp <- "COVID" 
CT_des2$Grp <- "LAL" 
BDD <- bind_rows(CT_des1, CT_des2) 
BDD <- BDD %>% 
  group_by(NCTid) %>% 
  mutate(text = paste0(textblock, collapse = " "))
BDD <- distinct(BDD[,-1])
descriptions <- BDD$text
unique_words <- length(unique(unlist(str_split(descriptions, " "))))
unique_words

library("tensorflow")
library("keras")

tokenizer <- text_tokenizer(num_word = 20000)
tokenizer %>% fit_text_tokenizer(descriptions)

library("reticulate")
library("purrr")

#Word2Vec

skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}

embedding_size <- 128
skip_window <- 5
num_sampled <- 1

input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1,
  output_dim = embedding_size,
  input_length = 1,
  name = "embedding"
)

target_vector <- input_target %>%
  embedding() %>%
  layer_flatten()

context_vector <- input_context %>%
  embedding()%>%
  layer_flatten()

dot_product <- 
  layer_dot(list(target_vector, context_vector), axes = 1)

output <- 
  layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), outputs = output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam", metrics = "accuracy")

summary(model)

history <- model %>%
  fit_generator(
    skipgrams_generator(descriptions, tokenizer, skip_window, negative_samples),
    steps_per_epoch = 50,
    epochs = 10
  )
