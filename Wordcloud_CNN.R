full_cnn<-read.csv('full_cnn.csv',header=TRUE)
View(full_cnn)
library(tidytext)
library(textclean)
library(wordcloud)
library(tidyverse)
text_cnn<-full_cnn$text
head(text_cnn)

text_cnn %>% 
  str_to_lower() %>% 
  replace_contraction() %>% 
  replace_url() %>% 
  replace_non_ascii() %>% 
  strip() -> text_cnn

head(text_cnn)
enframe(text_cnn,value='tweets',name = NULL) %>% 
  unnest_tokens(tweets,tweets) %>% 
  count(tweets,sort=TRUE) %>% 
  anti_join(stop_words,by=c('tweets'='word')) -> wordz

wordz %>% 
  with(
    wordcloud(
      words=tweets,
      freq=n,
      max.words = 125,
      random.order = FALSE,
      colors = brewer.pal(name='Dark2',12),
      scale = c(3.5,0.25)
    )
  )
