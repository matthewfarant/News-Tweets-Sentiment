########PACKAGES###########
library(tidyverse)
library(igraph)
library(ggraph)
library(tidytext)
######BIGRAMS TOKENIZATION##########
corona_cnn %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(bigram,text,token="ngrams",n=2) %>% 
  count(bigram,sort=TRUE) %>% 
  mutate(account='@CNNbrk')->bigram_cnn
corona_bbc %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(bigram,text,token="ngrams",n=2) %>% 
  count(bigram,sort=TRUE) %>% 
  mutate(account='@BBCBreaking')->bigram_bbc
corona_nbc %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(bigram,text,token="ngrams",n=2) %>% 
  count(bigram,sort=TRUE) %>% 
  mutate(account='@BreakingNews')->bigram_nbc
corona_nyt %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(bigram,text,token="ngrams",n=2) %>% 
  count(bigram,sort=TRUE) %>% 
  mutate(account='@nytimes')->bigram_nyt
corona_bloom %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(bigram,text,token="ngrams",n=2) %>% 
  count(bigram,sort=TRUE) %>% 
  mutate(account='@business')->bigram_bloom
corona_wsj %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(bigram,text,token="ngrams",n=2) %>% 
  count(bigram,sort=TRUE) %>% 
  mutate(account='@business')->bigram_wsj
bigram_full<-rbind(bigram_cnn,bigram_bbc,bigram_nbc,bigram_nyt,bigram_bloom,bigram_wsj)
bigrams_separated <- bigram_full %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
############NEW BIGRAM COUNTS##############
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts
###########BIGRAMS UNITED#################
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
############TRIGRAMS#####################
corona_cnn %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(trigram,text,token="ngrams",n=3) %>% 
  count(trigram,sort=TRUE) %>% 
  mutate(account='@CNNbrk')->trigram_cnn
corona_bbc %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(trigram,text,token="ngrams",n=3) %>% 
  count(trigram,sort=TRUE) %>% 
  mutate(account='@BBCBreaking')->trigram_bbc
corona_nbc %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(trigram,text,token="ngrams",n=3) %>% 
  count(trigram,sort=TRUE) %>% 
  mutate(account='@BreakingNews')->trigram_nbc
corona_nyt %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(trigram,text,token="ngrams",n=3) %>% 
  count(trigram,sort=TRUE) %>% 
  mutate(account='@nytimes')->trigram_nyt
corona_bloom %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(trigram,text,token="ngrams",n=3) %>% 
  count(trigram,sort=TRUE) %>% 
  mutate(account='@business')->trigram_bloom
corona_wsj %>% 
  dplyr::select(-created_at) %>% 
  unnest_tokens(trigram,text,token="ngrams",n=3) %>% 
  count(trigram,sort=TRUE) %>% 
  mutate(account='@business')->trigram_wsj
trigram_full<-rbind(trigram_cnn,trigram_bbc,trigram_nbc,trigram_nyt,trigram_bloom,trigram_wsj)
trigram_full %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)->trigram_counts
######NARRATIVE NETWORKS#############################
bigram_graph <- bigram_counts %>%
  filter(n>4) %>%
  graph_from_data_frame()
#Filtered, coronavirus only
corona_graph<- bigram_counts %>% 
  filter(n>2,word1=='coronavirus'|word2=='coronavirus') %>% 
  graph_from_data_frame()

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(corona_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
