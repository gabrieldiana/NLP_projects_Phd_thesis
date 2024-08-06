
month <- function(x)format(as.Date(x), '%Y-%m')

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

prep_corp<-function(text){
text<-tm_map(text, tolower)
text<-tm_map(text, removeNumbers)  
#remove Stopwords
text<-tm_map(text,removeWords, stopwords("english"))
for(word in custom_stop_words){
  text<-tm_map(text,removeWords, word)
  
}
text<-tm_map(text, removePunctuation)
text<-tm_map(text,stripWhitespace)
text<-tm_map(text,PlainTextDocument)
text<-tm_map(text,stemDocument)
text<-tm_map(text,removeWords,".{2}?")
text <- tm_map(text, content_transformer(gsub), pattern = "covid", replacement = "coronavirus", fixed=TRUE)

return (text)
}

prep_text<-function(text){
  #handle hashtags , url, etc. 
  {
    text <- tolower(text)
    text <- gsub("<.*?>", "", text) 
    text<-  gsub('http\\S+\\s*','',text)
    text <- gsub("amp", " ", text)  # "&" is "&amp" in HTML, so after punctuation removed 
    text <- gsub("[[:digit:]]", "", text)
    text <- gsub("[[:punct:]]", "", text)
    text <- gsub("[\\$,]", "", text)
  }
  return (text)
}

prep_text_without_stemming <-function(text_temp){
  text_temp<-gsub('http\\S+\\s*','',text_temp)
  text_temp <- gsub("<.*?>", "", text_temp)
  text_temp <- gsub("[\\$,]", "", text_temp)
  text_temp <- gsub("amp", "", text_temp)  # "&" is "&amp" in HTML, so after punctuation removed
  text_temp<-gsub("[\r\n]", "", text_temp)
}

#for each document/article, for the given topic, for each of its 10 top words calculate the frequency and then add the frequencies together
index_calc_fun<-function(topic,freq_terms){
  result<-data.frame()
  i_freq<-1
  for (document in 1:length(corpus_daily))
  {
    result[i_freq,1]<-0
    for (w in 1:length(freq_terms)){
      count<-str_count(corpus_daily[[document]]$content,pattern=freq_terms[w])
      result[i_freq,1]<-result[i_freq,1]+count
    }
    i_freq<-i_freq+1
  }
  return(result)
}




sentiment_function<-function(topic, mapped_results){ 
  expectation<-data.frame()
  i<-1
  foreach(dat=iter(unique(mapped_results$Date),by='row')) %do% {
    mapping_subset_by_date<-subset(mapped_results,Date==as.Date(dat))
    article_num<-mapping_subset_by_date$`Best Article`[topic]
    article<-all_text$V2[article_num]
    i<-i+1
  }
  
  return (expectation)
}

build_sent_index<-function(chosen_topic,mapped_results,term){
  #now build the intensity index
  index_matrix <- index_calc_fun(chosen_topic,term)
  sent_matrix<-sentiment_function(chosen_topic,mapped_results)
  #build the final index = sent*intensity
  final_index<-data.frame(dates=as.Date(daily_text$V1), index=sent_matrix*index_matrix)
  return(final_index)
}



sent<-function(data){
     sent_temp<-analyzeSentiment(data)
     sent_final<-sent_temp$SentimentLM  #values are already normalized
  return(sent_final)
}


#the second version of sentiment fucntion uses lexicon and also valence shifters. read more in bookmarks
sent_valence<-function(data){
  l1 <- sento_lexicons(list_lexicons[c("LM_en")],list_valence_shifters[["en"]])
  sent_final<-compute_sentiment(data, l1, do.sentence = FALSE)  #values are already normalized
   return(sent_final$LM_en)
}


#TOPIC FILTERING
get_topic_time_extraction<-function(lda){ 
  #===========METHOD 1 - BASED ON KEYWORDS - FUTURE, PRESENT, PAST============================
  #get future, past, present as separate files
  lda_top_words <- terms(lda,50) %>% as.data.frame() 
  
  #FUTURE
  text_topics_future<-data.frame()
  future_topics<-c()
  for(top in 1:ncol(lda_top_words)){
    count<-sum(as.numeric(grep(c('\\bnext\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bfutur\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bexpect\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bwill\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bforecast\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bpredict\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bsoon\\b'),lda_top_words[top])))
    if(count>=3 ){
      text_topics_future<-rbind(text_topics_future,text_topics%>%filter(topic==top))
      future_topics<-c(future_topics,top)
    }
  }
  
  #PAST
  text_topics_past<-data.frame()
  past_topics<-c()
  for(top in 1:ncol(lda_top_words)){
    count<-sum(as.numeric(grep(c('\\bpast\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\blast\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bprevious\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bbefore\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bago\\b'),lda_top_words[top])))
    if(count>=3){
      text_topics_past<-rbind(text_topics_past,text_topics%>%filter(topic==top))
      past_topics<-c(past_topics,top)
    }
  }
  
  #PRESENT
  text_topics_present<-data.frame()
  present_topics<-c()
  for(top in 1:ncol(lda_top_words)){
    count<-sum(as.numeric(grep(c('\\btoday\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bnow\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bpresent\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bcurrent\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bthis\\b'),lda_top_words[top])),
               as.numeric(grep(c('\\bcurrently\\b'),lda_top_words[top])))
    if(count>=3 ){
      text_topics_present<-rbind(text_topics_present,text_topics%>%filter(topic==top))
      present_topics<-c(present_topics,top)
    }
  }
  
  #remove from present all those topics that repeat in future topics
  present_topics <- present_topics[!present_topics %in% future_topics]
  
  return(list(past_topics,present_topics, future_topics))
  
}


lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}