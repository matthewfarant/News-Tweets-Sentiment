########### PACKAGES #############
if (!require('rtweet')) install.packages('rtweet'); library('rtweet')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('tidytext')) install.packages('tidytext'); library('tidytext')
if (!require('textclean')) install.packages('textclean'); library('textclean')
if (!require('sentimentr')) install.packages('sentimentr'); library('sentimentr')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('stringr')) install.packages('stringr'); library('stringr')
if (!require('tseries')) install.packages('tseries'); library('tseries')
if (!require('lmtest')) install.packages('lmtest'); library('lmtest')
if (!require('ggpubr')) install.packages('ggpubr'); library('ggpubr')
if (!require('scales')) install.packages('scales'); library('scales')
########## SCRAPING TWEETS ######################
tweet_cnn<-get_timeline(user='@cnnbrk', n = 3200) 
tweet_nyt<-get_timeline(user='@nytimes', n= 3200)  
tweet_bbc<-get_timeline(user='@BBCBreaking',n=3200)
tweet_bloom<-get_timeline(user='@business',n=3200)
tweet_nbc<-get_timeline(user='@BreakingNews',n=3200) 
tweet_wsj<-get_timeline(user='@wsj',n=3200)
#################################################
keywords<-c('Covid-19','coronavirus','corona','Covid','ncov','2019-ncov','SARS-CoV-2','lockdown')
############ KEYWORDS FILTER ###########################
corona_cnn<-tweet_cnn %>% 
  filter(grepl(paste(keywords, collapse="|"), text, ignore.case = TRUE))
corona_nyt<-tweet_nyt %>% 
  filter(grepl(paste(keywords, collapse="|"), text, ignore.case = TRUE))
corona_bbc<-tweet_bbc %>% 
  filter(grepl(paste(keywords, collapse="|"), text, ignore.case = TRUE))
corona_bloom<-tweet_bloom %>% 
  filter(grepl(paste(keywords, collapse="|"), text, ignore.case = TRUE))
corona_nbc<-tweet_nbc %>% 
  filter(grepl(paste(keywords, collapse="|"), text, ignore.case = TRUE))
corona_wsj<-tweet_wsj %>% 
  filter(grepl(paste(keywords, collapse="|"), text, ignore.case = TRUE))
#############################################
corona_cnn %>% 
  mutate(created_at=as.Date(created_at)) %>% 
  dplyr::select(created_at,text)->corona_cnn
corona_nyt %>% 
  mutate(created_at=as.Date(created_at)) %>% 
  dplyr::select(created_at,text)->corona_nyt
corona_bbc %>% 
  mutate(created_at=as.Date(created_at)) %>% 
  dplyr::select(created_at,text)->corona_bbc
corona_bloom %>% 
  mutate(created_at=as.Date(created_at)) %>% 
  dplyr::select(created_at,text)->corona_bloom
corona_nbc %>% 
  mutate(created_at=as.Date(created_at)) %>% 
  dplyr::select(created_at,text)->corona_nbc
corona_wsj %>% 
  mutate(created_at=as.Date(created_at)) %>% 
  dplyr::select(created_at,text)->corona_wsj
############ TOKEN NORMALIZATION #####################
corona_cnn$text %>% 
  str_to_lower() %>% 
  replace_contraction() %>% 
  replace_symbol() %>% 
  replace_url() %>% 
  strip()->corona_cnn$text
corona_nyt$text %>% 
  str_to_lower() %>% 
  replace_contraction() %>% 
  replace_symbol() %>% 
  replace_url() %>% 
  strip()->corona_nyt$text
corona_bbc$text %>% 
  str_to_lower() %>% 
  replace_contraction() %>% 
  replace_symbol() %>% 
  replace_url() %>% 
  strip()->corona_bbc$text
corona_bloom$text %>% 
  str_to_lower() %>% 
  replace_contraction() %>% 
  replace_symbol() %>% 
  replace_url() %>% 
  strip()->corona_bloom$text
corona_nbc$text %>% 
  str_to_lower() %>% 
  replace_contraction() %>% 
  replace_symbol() %>% 
  replace_url() %>% 
  strip()->corona_nbc$text
corona_wsj$text %>% 
  str_to_lower() %>% 
  replace_contraction() %>% 
  replace_symbol() %>% 
  replace_url() %>% 
  strip()->corona_wsj$text
###############################################
corona_bbc %>% 
  filter(created_at>=as.Date('2020-01-01'))->corona_bbc
corona_nbc %>% 
  filter(created_at>=as.Date('2020-01-01'))->corona_nbc
sentiment_cnn %>% 
  filter(created_at>=as.Date('2020-01-01'))->sentiment_cnn
###############################################
sentiment_cnn<-cbind(corona_cnn,sentiment_by(corona_cnn$text))
sentiment_nyt<-cbind(corona_nyt,sentiment_by(corona_nyt$text))
sentiment_bbc<-cbind(corona_bbc,sentiment_by(corona_bbc$text))
sentiment_bloom<-cbind(corona_bloom,sentiment_by(corona_bloom$text))
sentiment_nbc<-cbind(corona_nbc,sentiment_by(corona_nbc$text))
sentiment_wsj<-cbind(corona_wsj,sentiment_by(corona_wsj$text))
View(sentiment_wsj)
#SUPER CLEANING
keywords2<-c('crisis','positive','highest','top','death','new covid','new coronavirus')
for(i in 1:nrow(sentiment_cnn)){
  if(grepl(paste(keywords2, collapse="|"),
           sentiment_cnn$text[i],
           ignore.case = TRUE)&
     sentiment_cnn$ave_sentiment[i]>=0){
    sentiment_cnn$ave_sentiment[i]<-sentiment_cnn$ave_sentiment[i]*(-1)
  }
}
for(i in 1:nrow(sentiment_bbc)){
  if(grepl(paste(keywords2, collapse="|"),sentiment_bbc$text[i], ignore.case = TRUE)&
     sentiment_bbc$ave_sentiment[i]>=0){
    sentiment_bbc$ave_sentiment[i]<-sentiment_bbc$ave_sentiment[i]*(-1)
  }
}
for(i in 1:nrow(sentiment_nbc)){
  if(grepl(paste(keywords2, collapse="|"),sentiment_nbc$text[i], ignore.case = TRUE)&
     sentiment_nbc$ave_sentiment[i]>=0){
    sentiment_nbc$ave_sentiment[i]<-sentiment_nbc$ave_sentiment[i]*(-1)
  }
}
keyword3<-c('vaccine','low','lowest')
for(i in 1:nrow(sentiment_cnn)){
  if(grepl(paste(keyword3,collapse="|"),sentiment_cnn$text[i],ignore.case = TRUE)&
     sentiment_cnn$ave_sentiment[i]<0){
    sentiment_cnn$ave_sentiment[i]<-sentiment_cnn$ave_sentiment[i]*(-1)
  }
}
for(i in 1:nrow(sentiment_bbc)){
  if(grepl(paste(keyword3,collapse="|"),sentiment_bbc$text[i],ignore.case = TRUE)&
     sentiment_bbc$ave_sentiment[i]<0){
    sentiment_bbc$ave_sentiment[i]<-sentiment_bbc$ave_sentiment[i]*(-1)
  }
}
for(i in 1:nrow(sentiment_nbc)){
  if(grepl(paste(keyword3,collapse="|"),sentiment_nbc$text[i],ignore.case = TRUE)&
     sentiment_nbc$ave_sentiment[i]<0){
    sentiment_nbc$ave_sentiment[i]<-sentiment_nbc$ave_sentiment[i]*(-1)
  }
}
############## SENTIMENT ANALYSIS ####################
sentiment_cnn %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment)) %>% 
  mutate(Status=ifelse(sentiment>0,'Positive','Negative')) %>% 
  ggplot(aes(x=created_at,y=sentiment,fill=Status)) + 
  geom_col()+
  scale_x_date(breaks='1 month',labels=date_format('%B'))+
  labs(x='Month',y='Sentiment',title='Sentiment Analysis of @CNNbrk Tweets',subtitle='Polarity analysis on tweets that contain coronavirus related words',
       caption='Source: Twitter')
sentiment_nyt %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment)) %>% 
  mutate(Status=ifelse(sentiment>0,'Positive','Negative')) %>% 
  ggplot(aes(x=created_at,y=sentiment,fill=Status)) + 
  geom_col()
sentiment_bbc %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment)) %>% 
  mutate(Status=ifelse(sentiment>0,'Positive','Negative')) %>% 
  ggplot(aes(x=created_at,y=sentiment,fill=Status)) + 
  geom_col()+
  scale_x_date(breaks='1 month',labels=date_format('%B'))+
  labs(x='Month',y='Sentiment',title='Sentiment Analysis of @BBCBreaking Tweets',subtitle='Polarity analysis on tweets that contain coronavirus related words',
       caption='Source: Twitter')
sentiment_bloom %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment)) %>% 
  mutate(Status=ifelse(sentiment>0,'Positive','Negative')) %>% 
  ggplot(aes(x=created_at,y=sentiment,fill=Status)) + 
  geom_col()
sentiment_nbc %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment)) %>% 
  mutate(Status=ifelse(sentiment>0,'Positive','Negative')) %>% 
  ggplot(aes(x=created_at,y=sentiment,fill=Status)) + 
  geom_col()+
  scale_x_date(breaks='1 month',labels=date_format('%B'))+
  labs(x='Month',y='Sentiment',title='Sentiment Analysis of @BreakingNews Tweets',subtitle='Polarity analysis on tweets that contain coronavirus related words',
       caption='Source: Twitter')
###########################################################
sentiment_cnn %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment))->sentiment_cnn_sum 
sentiment_bbc %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment))->sentiment_bbc_sum
sentiment_nbc %>% 
  group_by(created_at) %>% 
  summarize(sentiment=mean(ave_sentiment))->sentiment_nbc_sum
################### CNN BBC JOINED #######################
sentiment_cnn_sum %>% 
  dplyr::select(created_at,sentiment) %>% 
  left_join(sentiment_bbc_sum,by='created_at',suffix=c('_cnn','_bbc')) %>% 
  left_join(sentiment_nbc_sum,by='created_at') %>% 
  rename(sentiment_nbc=sentiment)->sentiment_cnn_bbc_joined
sentiment_cnn_bbc_joined<-mutate(sentiment_cnn_bbc_joined,sentiment_mean=rowMeans(
  dplyr::select(sentiment_cnn_bbc_joined,starts_with("sentiment_")),na.rm = TRUE))
View(sentiment_cnn_bbc_joined) # includes nbc
#############EMOTION #############################
emotion_cnn<-emotion_by(corona_cnn$text)
emotion_cnn %>% 
  filter(ave_emotion>0) %>% 
  group_by(emotion_type) %>% 
  summarize(emotion_count=sum(emotion_count)) %>% 
  arrange(desc(emotion_count)) %>% 
  head(8) %>% 
  mutate(emotion_type=fct_reorder(emotion_type,emotion_count)) %>% 
  ggplot(aes(emotion_type,emotion_count))+
  geom_col(fill='#EC2029')+
  coord_flip()+
  labs(y='Word Count',x='Emotion Type',title='CNN Breaking News')->emotion_plot_cnn
emotion_bbc<-emotion_by(corona_bbc$text)
emotion_bbc %>% 
  filter(ave_emotion>0) %>% 
  group_by(emotion_type) %>% 
  summarize(emotion_count=sum(emotion_count)) %>% 
  arrange(desc(emotion_count)) %>% 
  head(8) %>% 
  mutate(emotion_type=fct_reorder(emotion_type,emotion_count)) %>% 
  ggplot(aes(emotion_type,emotion_count))+
  geom_col(fill='black')+
  coord_flip()+
  labs(x='Emotion Type', y= "Word Count", title='BBC Breaking News')->emotion_plot_bbc
emotion_nyt<-emotion_by(corona_nyt$text)
emotion_nyt %>% 
  filter(ave_emotion>0) %>% 
  group_by(emotion_type) %>% 
  summarize(emotion_count=sum(emotion_count)) %>% 
  arrange(desc(emotion_count)) %>% 
  head(8)%>% 
  mutate(emotion_type=fct_reorder(emotion_type,emotion_count)) %>% 
  ggplot(aes(emotion_type,emotion_count))+
  geom_col()+
  coord_flip()+
  labs(x="Emotion Type",y="Word Count",title='New York Times')->emotion_plot_nyt
emotion_bloom<-emotion_by(corona_bloom$text)
emotion_bloom %>% 
  filter(ave_emotion>0) %>% 
  group_by(emotion_type) %>% 
  summarize(emotion_count=sum(emotion_count)) %>% 
  arrange(desc(emotion_count)) %>% 
  head(8) %>% 
  mutate(emotion_type=fct_reorder(emotion_type,emotion_count)) %>% 
  ggplot(aes(emotion_type,emotion_count))+
  geom_col(fill='#0000FF')+
  coord_flip()+
  labs(x='Emotion Type',y="Word Count",title='Bloomberg')->emotion_plot_bloom
emotion_nbc<-emotion_by(corona_nbc$text)
emotion_nbc %>% 
  filter(ave_emotion>0) %>% 
  group_by(emotion_type) %>% 
  summarize(emotion_count=sum(emotion_count)) %>% 
  arrange(desc(emotion_count)) %>% 
  head(8) %>% 
  mutate(emotion_type=fct_reorder(emotion_type,emotion_count)) %>% 
  ggplot(aes(emotion_type,emotion_count))+
  geom_col(fill='#FF9900')+
  coord_flip()+
  labs(x='Emotion Type',y="Word Count",title='NBC Breaking News')->emotion_plot_nbc
emotion_wsj<-emotion_by(corona_wsj$text)
emotion_wsj %>% 
  filter(ave_emotion>0) %>% 
  group_by(emotion_type) %>% 
  summarize(emotion_count=sum(emotion_count)) %>% 
  arrange(desc(emotion_count)) %>% 
  head(8) %>% 
  mutate(emotion_type=fct_reorder(emotion_type,emotion_count)) %>% 
  ggplot(aes(emotion_type,emotion_count))+
  geom_col(fill='#edb879')+
  coord_flip()+
  labs(x='Emotion Type',y="Word Count",title='Wall Street Journal')->emotion_plot_wsj
ggarrange(emotion_plot_bbc,emotion_plot_cnn,emotion_plot_nyt,emotion_plot_bloom,emotion_plot_nbc,emotion_plot_wsj,
          ncol=3,nrow=3)
################## VIX ########################
vix<-read.csv(file.choose())
vix$Date<-as.Date(vix$Date,format = '%m/%d/%Y')
vix<-vix %>% 
  filter(Date>=as.Date('2020-01-01'))
changevix<-c()
for(i in 1:nrow(vix)){
  changevix[i]<-(vix$VIX.Close[i+1]-vix$VIX.Close[i])/vix$VIX.Close[i]
}
vix<-vix %>% mutate(changevix=changevix)
####################################################################
sentiment_cnn_bbc_joined %>% 
  inner_join(vix,by=c('created_at'='Date'))->vix_sent_joined
####################################################################
vix_sent_joined$Status<-ifelse(vix_sent_joined$sentiment_mean>0
                               ,'Positive','Negative')
ggplot(vix_sent_joined)+
  geom_col(aes(created_at,sentiment_mean,fill=Status))+
  geom_line(aes(created_at,changevix),size=1,alpha=0.6)+
  labs(x='Month',y='Mean Polarity & VIX',title='Tweets\' Polarity vs. VIX*',caption='*VIX data downloaded from CBOE website')
#adf test
broom::tidy(adf.test(vix_sent_joined$sentiment_mean))
broom::tidy(adf.test(na.omit(vix_sent_joined$VIX.Close)))
#kpss
broom::tidy(kpss.test(vix_sent_joined$sentiment_mean))
broom::tidy(kpss.test(vix_sent_joined$changevix))
#ccf
ccf(vix_sent_joined$sentiment_mean,na.omit(vix_sent_joined$changevix))
