rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Libraries.r")
source("functions.r")
require(dplyr)
options(scipen = 999) #disable scientific notation
 

#guardian library is expired, hence needs to be manually installed from tar.gz package and then loaded as below
library(GuardianR)

#define the economic shocks
shocks.df = read.table(textConnection(
  "From, Till
2008-01-01, 2009-01-01
2016-06-20, 2016-12-31 
2020-01-01, 2021-01-01
2022-02-24, 2022-12-31"
  ), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE) %>% mutate(ymin=-Inf, ymax=Inf)

dates_monthly<-seq(as.Date("2000-01-01"),as.Date("2023-12-31"),by="months")
dates_monthly<-as.Date(dates_monthly,"%Y-%m-%d")


#Put Guardian API keys here. 3 are needed due to limitations per key
api_key1=""
api_key2=""
api_key3=""


#choose number of topics for LDA model - use intuition and domain knowledge
lda_topics_num=80

#DATA COLLECTION===========================================================
keywords<-"inflation%20or%20deflation%20or%20price%20or%20cost%20prices%20or%20costs"


#below code needs to be run twice, first time until around 2017 jan and second time from there to jan 2019.
#this is because Guardian has a limit of 5000 calls a day. Keys are changed for each run.
#below loop queries the articles from the guardian for each day and then saves the results in two data frames, daily_text and all_text

#PART 1, DATES 2000-2010
dates_temp<-seq.dates("01/01/2000","09/08/2012",by="days")  
dates_fin_temp<-as.Date(dates_temp,"%Y-%m-%d") #convert dates to acceptabe format for other functions
#daily_text  is the collection of text for every day (grouped per day) and all_text is all the text in separate indexation
daily_text<-data.frame()  
all_text<-data.frame()  
row<-1


for (j in 1:(length(dates_fin_temp))){
  temp<-get_guardian(keywords,
                     section="business",
                     from.date=dates_fin_temp[j], 
                     to.date=dates_fin_temp[j], 
                     api.key=api_key1)
  body_tmp<-temp$body
  
  #some pre-processing to make body more readable
  body<-iconv(body_tmp, "latin1","ASCII", sub="")
  if(dim(temp)[1]==0){ #if the text is empty for the gvien day
    daily_text[j,1]<-dates_fin_temp[j]  #dates
    daily_text[j,2]<-" "  #content
    daily_text[j,3]<-0 # wordcount
    
    all_text[row,1]<-dates_fin_temp[j] #date
    all_text[row,2]<-" "  #content
    row<-row+1
  }else{
    daily_text[j,1]<-dates_fin_temp[j] #dates
    daily_text[j,2]<-paste(body, collapse = "")  #content
    daily_text[j,3]<-sum(as.numeric(as.character(temp[,17]))) #parameter 17 in the results -> wordcount

    for(i in 1:length(temp$id)){
      all_text[row,1]<-dates_fin_temp[j] #date
      all_text[row,2]<-body[i] #content
      row<-row+1
    }
  }

}

all_text1<-all_text
daily_text1<-daily_text

#PART 2, DATES 2017-2020
dates_temp<-seq.dates("09/09/2012","12/31/2019",by="days")
dates_fin_temp<-as.Date(dates_temp,"%Y-%m-%d")
daily_text<-data.frame()
all_text<-data.frame()
row<-1

for (j in 1:(length(dates_fin_temp))){
  temp<-get_guardian(keywords,
                     section="business",
                     from.date=dates_fin_temp[j], 
                     to.date=dates_fin_temp[j], 
                     api.key=api_key2) 

  body_tmp<-temp$body
  #some pre-processing to make body more readable
  body<-iconv(body_tmp, "latin1","ASCII", sub="")
  if(dim(temp)[1]==0){ #if the text is empty for the gvien day
    daily_text[j,1]<-dates_fin_temp[j]  #dates
    daily_text[j,2]<-" "  #content
    daily_text[j,3]<-0 # wordcount
    
    all_text[row,1]<-dates_fin_temp[j] #date
    all_text[row,2]<-" "  #content
    row<-row+1
  }else{
    daily_text[j,1]<-dates_fin_temp[j] #dates
    daily_text[j,2]<-paste(body, collapse = "")  #content
    daily_text[j,3]<-sum(as.numeric(as.character(temp[,17]))) #parameter 17 in the results -> wordcount
    
    for(i in 1:length(temp$id)){
      all_text[row,1]<-dates_fin_temp[j] #date
      all_text[row,2]<-body[i] #content
      row<-row+1
    }
  }
}

all_text2<-all_text
daily_text2<-daily_text


#PART 3, DATES 2019-2023
dates_temp<-seq.dates("01/01/2020","12/31/2023",by="days")
dates_fin_temp<-as.Date(dates_temp,"%Y-%m-%d")
daily_text<-data.frame()
all_text<-data.frame()
row<-1

for (j in 1:(length(dates_fin_temp))){
  temp<-get_guardian(keywords,
                     section="business",
                     from.date=dates_fin_temp[j], 
                     to.date=dates_fin_temp[j], 
                     api.key=api_key3)
  body_tmp<-temp$body
  #some pre-processing to make body more readable
  body<-iconv(body_tmp, "latin1","ASCII", sub="")
  if(dim(temp)[1]==0){ #if the text is empty for the gvien day
    daily_text[j,1]<-dates_fin_temp[j]  #dates
    daily_text[j,2]<-" "  #content
    daily_text[j,3]<-0 # wordcount
    
    all_text[row,1]<-dates_fin_temp[j] #date
    all_text[row,2]<-" "  #content
    row<-row+1
  }else{
    daily_text[j,1]<-dates_fin_temp[j] #dates
    daily_text[j,2]<-paste(body, collapse = "")  #content
    daily_text[j,3]<-sum(as.numeric(as.character(temp[,17]))) #parameter 17 in the results -> wordcount
    
    for(i in 1:length(temp$id)){
      all_text[row,1]<-dates_fin_temp[j] #date
      all_text[row,2]<-body[i] #content
      row<-row+1
    }
  }
}

all_text3<-all_text
daily_text3<-daily_text


#bind all the subsets
all_text<-rbind(all_text1,all_text2, all_text3)
daily_text<-rbind(daily_text1,daily_text2, daily_text3)
#fix the dates to the longer sample for future use
dates<-seq.dates("01/01/2000","12/31/2023",by="days")  #date format mm/dd/yyy
dates_fin<-as.Date(dates,"%Y-%m-%d")

#rename the colimns from V1 and V2 to more understadable names
names(daily_text)[1]='Date'
names(daily_text)[2]='Text'
names(daily_text)[3]='Count'

names(all_text)[1]='Date'
names(all_text)[2]='Text'

#save to files
save_all_text<-all_text
save_daily_text<-daily_text
write.csv(all_text,"save_all_text.csv")
write.csv(daily_text,"save_daily_text.csv")

#DATA PREPARATION====================================================
#Convert all dates in date format
daily_text$Date<-as.Date(daily_text[,1])  
all_text$Date<-as.Date(all_text$Date)

#now delete the nulls from the data before proceeding. Instead of deleting its best to put word empty in the results for easier data pre-processing later on
daily_text$Text[daily_text$Text==" "] <-"br"  #random word which will be later deleted as it is shorter than 2 symbols
all_text$Text[all_text$Text==" "] <-"br"  #random word which will be later deleted as it is shorter than 2 symbols

custom_stop_words<-c("character","description","sinc","id","mon","yday","listauthor", "listsec","isdst","datetimestamp","br","go","mday","meta","wday","listcontent","datetimestamp","description","heading","isdst","listauthor","listsec","yday","make","mon","two","come","day","id","first","one","take","still","character","uk","us","like","new","can","week","say","said","did","also","gmt","pm","am","since","time","just","may")

#Call pre-processing function twice - for daily_text and for all_text separately 
#daily_text is needed to construct the frequency indices later on
initial_prep_daily<-prep_text(daily_text$Text)
guardian_corpus_daily=VCorpus(VectorSource(initial_prep_daily))
corpus_daily<-prep_corp(guardian_corpus_daily)

#same for all_text now to use in LDA
initial_prep_all<-prep_text(all_text$Text)
#now replace any empty documents with word "empty" in the documents otherwise LDA will give an error
initial_prep_all[initial_prep_all==" "]<-"br"
guardian_corpus_all=VCorpus(VectorSource(initial_prep_all))
corpus_all<-prep_corp(guardian_corpus_all)



#MODEL BUILDING===================================================================================================
#DTM, TDM
#First you will have to create a DTM(document term matrix), which is a sparse matrix containing your terms and documents as dimensions.

dtm <- DocumentTermMatrix(corpus_all, control = list(wordLengths = c(3, Inf)))  
dtm_save=dtm
dtm_temp<-removeSparseTerms(dtm, 0.992) #remove sparse items, above this number less is removed, below - some important words are removed
dtm<-dtm_temp
rowTotalsDtm <- apply(dtm , 1, sum)  #Find the sum of words in each Document
dtm_temp<-dtm[rowTotalsDtm>0,] #to choose docs with some actual content
dtm<-dtm_temp

#keep track of rows that are removed in order for dates to match
empty_rows<-as.numeric(which(rowTotalsDtm==0))
dates_filtered<-all_text$Date
dates_filtered<-dates_filtered[!row_number(dates_filtered) %in% empty_rows]  #filter by dates that had empty documents

#build tdm, that is needed for wordcloud and some other plots. 
tdm <- TermDocumentMatrix(corpus_all, control = list(wordLengths = c(3, Inf)))
tdm_save=tdm
tdm_temp<-removeSparseTerms(tdm, 0.992)
tdm<-tdm_temp

#Frequencies and Word Clouds===================================================================================================

#PLotting hsitorgrams with the words and frequencies
# Calculate Frequencies of Words
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 17000) #terms that appear more than 17000 times.
df <- data.frame(term = names(term.freq), freq = term.freq) %>% arrange(desc(freq))

#plot
ggplot(df, aes(x=reorder(term, freq), y=freq)) + geom_bar(stat="identity") +
  xlab("") + ylab("") + coord_flip() +
  ggtitle("Top Frequent Words by Count", )+
theme(axis.text=element_text(size=10), axis.text.y=element_text(size = 12), plot.title=element_text(size=10))+
xlab("Terms") + ylab("Frequency") 

#plotting a wordcloud with top words
m <- as.matrix(tdm)
# Sum rows and frequency data frame
sum_freq <- rowSums(m)
# Sort term_frequency in descending order
word_freq <- sort(sum_freq, decreasing = T)
#View the top 10 most common words
pal <- brewer.pal(9, "BuGn")[-(1:4)] # colors
wordcloud(words = names(word_freq), freq = word_freq, min.freq = 20,
          random.order = F,   max.words = 500, rot.per=0.35, colors = brewer.pal(8, "Dark2"))



##LDA Model========================================================================================================

lda<-LDA(dtm, lda_topics_num , method = "Gibbs", control = list(seed = 1000, burnin = 500, thin = 100, iter = 4000))

#some diagnostics
#print(perplexity(lda, newdata = dtm.new,estimate_theta=FALSE))
#print(lda@loglikelihood)

# get_terms(lda, 20)
write.csv( terms(lda,1000),'lda_results.csv')

#sabe in a new variable just in case
lda_save<-lda


#tidy in the format so that you can see per topic-per word-probabilities
text_topics <- tidytext::tidy(lda , matrix = "beta")
text_topics_80 <- tidytext::tidy(lda , matrix = "beta")

#save in csv
write.csv(text_topics,'text_topics.csv')

#below codes are needed for plotting the top terms per LDA
text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(1000, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


#below codes are needed for plotting the top terms per LDA
text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


#another way to get topic name by creating a separte data tables, will need later
DT <- as.data.table(text_top_terms)
#first term as labell
topic_names<-DT[,list(label = term[which.max(beta)]),by = topic] 
#first 2 words as label
topic_names_2<-DT[,list(label=paste(term[which(beta == max(beta))],'/',term[which(beta == max(beta))+1])),by=topic]
#first 3 words as label
topic_names_3<-DT[,list(label=paste(term[which(beta == max(beta))],'/',term[which(beta == max(beta))+1], '/',term[which(beta == max(beta))+2])),by=topic]

#merge into one and save
write.csv(as.data.frame(cbind(topic=topic_names$topic, label1=topic_names$label,label2=topic_names_2$label,label3=topic_names_3$label)),'topic_labels.csv')



#extract topics that refer to future, past and present
time_extracted_topics_list<-get_topic_time_extraction(lda)
past_topics<-time_extracted_topics_list[[1]]
present_topics<-time_extracted_topics_list[[2]]
future_topics<-time_extracted_topics_list[[3]]

#check the topics
past_topics
present_topics
future_topics

#choose future topics to for printing in the final paper, exclude those that coincide with past and present topics
chosen_topics<-future_topics[!future_topics %in% present_topics & !future_topics %in% past_topics]

#print smaller subset f the tipic histograms
text_top_terms  %>% filter(topic %in% chosen_topics) #%>%  #filter(topic %in% 61:80) %>% 
mutate(term = reorder(term, beta)) %>%
  group_by(topic) %>% 
  mutate(label=paste(term[which(beta == max(beta))],'/',term[which(beta == max(beta))+1])) %>%
  ggplot(aes(reorder(term, beta), beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = " ") +
  facet_wrap(~label, ncol = 4, scales = "free",) +
  coord_flip() +
  theme(axis.text.x = element_blank())



#BUILDING FREQUENCY INDICES=========================================================================

#now lets build a frequency index based on all topics
chosen_topics<-seq(1:k)

#calculate the intensity index
length_topics<-length(chosen_topics)
topic_labels<-topic_names_2 %>% filter(topic %in% chosen_topics)

basic_indices_save<-data.frame(date=as.Date(daily_text$Date))
foreach(topic=iter(chosen_topics,by='row'),.combine = 'cbind') %do% { 
   freq_terms<-terms(lda,25)[,topic]
   new_matrix<-index_calc_fun(topic,freq_terms)
   basic_indices_save<-cbind(basic_indices_save, new_matrix$V1)
}

names(basic_indices_save)[2:(length_topics+1)]= topic_labels$label  # or if simply topic number then use paste0( "topic ", chosen_topics)

#BASIC INDICES WITH 80 topics
#calculate the intensity index
length_topics<-length(chosen_topics)
topic_labels<-topic_names_2 %>% filter(topic %in% chosen_topics)

basic_indices_save<-data.frame(date=as.Date(daily_text$Date))
foreach(topic=iter(chosen_topics,by='row'),.combine = 'cbind') %do% { 
  freq_terms<-terms(lda,20)[,topic]
  new_matrix<-index_calc_fun(topic,freq_terms)
  basic_indices_save<-cbind(basic_indices_save, new_matrix$V1)
}
names(basic_indices_save)[1]= "Date"
names(basic_indices_save)[2:(length_topics+1)]= topic_labels$label  # or if simply topic number then use paste0( "topic ", chosen_topics)

#create monthly series
basic_indices<-zoo(basic_indices_save[colnames(basic_indices_save)!="date"],as.Date(daily_text$Date)) #zoo object
basic_indices_monthly<-aggregate(basic_indices, by=month, FUN=mean, na.rm=TRUE, na.action=NULL)

#save series locally in csv
write.csv(as.data.frame(basic_indices),"basic_index.csv") #as.data.frame is needed, otherwise date column is not saved in csv
write.csv(as.data.frame(basic_indices_monthly),"basic_index_monthly.csv")
write.csv(as.data.frame(basic_indices_save),"basic_indices_daily.csv")

# plot the top ten terms for each topic in order
data_monthly<-cbind(dates=dates_monthly,fortify.zoo(basic_indices_monthly))

data_monthly %>% dplyr::select(-2) %>% dplyr::select(c(1,chosen_topics+1)) %>% 
  gather(key = "variable", value = "value", -dates) %>%
  ggplot( aes(x = dates, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  xlab('')+ylab('Frequency Index') +
  theme_light() +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=8),axis.title.y = element_text(size=8))+
  facet_wrap(~ variable, ncol = 3) #comment this line only to plot topics in one graph



#SENTIMENT ANALYSIS=================================================================================

#gammas matrix is needed to identify per-topic-per-document probabilities and to find the article that describes the given topic the best
gammas_matrix<-data.frame(date=rep(dates_filtered,k),document=reshape2::melt(lda@gamma)$Var1 ,topic=reshape2::melt(lda@gamma)$Var2, gamma=reshape2::melt(lda@gamma)$value)

#with this step, find top 3 probable documents for each topic. this way you can merge them into one big document to analyze sentiment
#by filtering gamma>0.3 we ensur ethat the documents which are not very probable for that topic do not get includes otherwise the statistic changes.
most_probable_document_per_topic<-gammas_matrix %>% select(date,topic, document, gamma) %>% filter(gamma>0.05) %>%
  arrange(date,topic,gamma) %>% group_by(date,topic) %>% 
  top_n(10,gamma) %>% ungroup() %>%   #CHNAGE TO TOP_N(1,gamma)
  select(-gamma)


#create the final documents indices
daily_text_top_documents<-data.frame()
row<-1
#move to top later
all_text_prepped_without_stem_temp<-lapply(all_text$Text,prep_text_without_stemming)
all_text_prepped_without_stem<-do.call(rbind, all_text_prepped_without_stem_temp)

foreach(top=iter(1:k,by='row'),.combine = 'cbind') %do% { 
  
  foreach(dat=iter(daily_text$Date,by='row'),.combine = 'cbind') %do% { 
    
    merged_docs<-data.frame()
    
    daily_text_top_documents[row,1]<-top #topic
    daily_text_top_documents[row,2]<-as.Date(dat) #date
    
    subset<-most_probable_document_per_topic %>% filter(topic==top & date==dat)
    if(nrow(subset)==0){
      daily_text_top_documents[row,3]=''
    }else{
      
      merged_docs<-data.frame()
      for (doc in subset$document){        #thi way we pre-process the documents
        merged_docs<-paste(merged_docs,all_text_prepped_without_stem[doc])   #merged documents, using the non-stemmed corpus
      }
      daily_text_top_documents[row,3]<-merged_docs
      
    }
    row<-row+1
  }
}

names(daily_text_top_documents)[1]='Topic'
names(daily_text_top_documents)[2]='Date'
names(daily_text_top_documents)[3]='Text'

#there are many empty documents/rows in the variable so we change them to word neutral for the dcast function to work 
#Neutral will get a value of zero in sentiment analysis, so it doesn't affect the numbers.
daily_text_top_documents[which(daily_text_top_documents$Text==''),'Text'] = 'Neutral'


#with dcast we will now transform the melted daily_text_top_documents to data.table, so that each column is the topic, each row is the collection of articles for relevant date
#text_as_table<-dcast(setDT(daily_text_top_documents), Date ~ Topic, value.var = 'Text')
#the fun.aggregate below is needed otherwise when castign the data as table duplicates are being removed. 
text_as_table<-dcast(daily_text_top_documents, Date ~ Topic, value.var = 'Text', fun.aggregate =function(x) paste(x[1]))
names(text_as_table)[2:(k+1)]= paste0(seq(1:k))

#run through each date and text and build two versions of sentiment index. one using AnalyzeSentiment, the other using sentometrics
sentiment_indices_1 <- text_as_table %>% as.data.frame() %>% select(c(2:(k+1))) %>% mutate_each(funs(sent))
sentiment_indices_2 <- text_as_table %>% as.data.frame() %>% select(c(2:(k+1))) %>% mutate_each(funs(sent_valence))

#add date to sentiment and rename the column names to our guestimated topic names
sentiment_indices_1<-cbind(Date=text_as_table$Date,sentiment_indices_1)
sentiment_indices_2<-cbind(Date=text_as_table$Date,sentiment_indices_2)
names(sentiment_indices_1)[2:(k+1)]= topic_names_2$label  # or if simply topic number then use paste0( "topic ", chosen_topics)
names(sentiment_indices_2)[2:(k+1)]= topic_names_2$label # or if simply topic number then use paste0( "topic ", chosen_topics)


#plot daily sentiments
sentiment_indices_2 %>% dplyr::select(c(1,chosen_topics+1)) %>%  
  gather(key = "variable", value = "value", -Date) %>%
  ggplot( aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_x_date(breaks = pretty_breaks(12))+
  # geom_rect(data=shocks.df, aes(xmin=From, xmax=Till, ymin=ymin, ymax=ymax),fill="grey", alpha=0.3,inherit.aes = FALSE)+
  xlab('')+ylab('Frequency Index')+
  theme_light()+
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=8),axis.title.y = element_text(size=8))

#save in csv
write.csv(sentiment_indices_2,'sentiment2_daily.csv')

#sentiment_indices_3 for removing all 0s in the sentiment analysis 
sentiment_indices_3=sentiment_indices_2
sentiment_indices_3[sentiment_indices_3==0]=NA


#in order to aggregate data to monthly, zoo library is used.
#change sentiment_indices_1 or 2 depending which one chosen
sent_indices_zoo<-zoo(sentiment_indices_3[colnames(sentiment_indices_3)!="Date"],as.Date(dates_fin))
sent_indices_monthly<-aggregate(sent_indices_zoo, by=month, FUN=mean,na.rm=TRUE) 

#convert back to dataframe, delete first column , which is called INDEX and create Dates, and move it to first position
sentiment_indices_final<- sent_indices_monthly %>% fortify.zoo() %>% select(-1) %>% mutate(Date=dates_monthly) %>% select(Date, everything())

#plot monthly sentiments
sentiment_indices_final %>% dplyr::select(c(1,chosen_topics+1)) %>%  
  gather(key = "variable", value = "value", -Date) %>%
  ggplot( aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1)  + scale_x_date(breaks = pretty_breaks(12))+
  theme_minimal()

#save
write.csv(as.data.frame(sentiment_indices_final),"sentiment_indices_final.csv")


#SENTIMENT AND FREQUENCY INDICES=========
#multiply with basic_indices
frequency_indices_final<-as.data.frame(fortify.zoo(basic_indices_monthly)) %>% select(-1)
names(frequency_indices_final)[1:(k)]= topic_names_2$topic# or if simply topic number then use paste0( "topic ", chosen_topics)

final_indices<-data.frame(dates=as.Date(dates_monthly),
                          as.matrix(frequency_indices_final)*as.matrix(sentiment_indices_final%>%select(-1))) 
names(final_indices)[1]= 'Date'  # or if simply topic number then use paste0( "topic ", chosen_topics)
names(final_indices)[2:(k+1)]= topic_names_2$label  # or if simply topic number then use paste0( "topic ", chosen_topics)


#plot
final_indices %>% dplyr::select(c(1,chosen_topics+1)) %>%   #86,98
  gather(key = "variable", value = "value", -Date) %>%
  ggplot( aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1)  + scale_x_date(breaks = pretty_breaks(12))+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(col="", y='News-Topic Driven Index', x=" ")

#save
write.csv(as.data.frame(final_indices),"final_indices.csv")


#SENTIMENT FOR ALL TEXT(not per topic, used for comparing to consumer confidence indices)==========================================================
#now compare all texts (not topic based) to official cosnumer confidence index
#to do so we need to build a daily consumer confidence index for all topics, so for all news

#some pre-processing for cleaner text
text<-daily_text$Text
text<-gsub('http\\S+\\s*','',text)
text <- gsub("<.*?>", "", text) 
text <- gsub("[\\$,]", "", text)
text <- gsub("amp", "", text)  # "&" is "&amp" in HTML, so after punctuation removed 
text <- gsub("[\r\n]", "", text)
daily_text_prepped<-data.frame(Date=dates_fin)
daily_text_prepped$Text<-text
str(daily_text_prepped)
str(daily_text$Text)
str(text)
rm(text)


sentiment_indices_all <- daily_text_prepped %>% select(Text) %>% mutate_each(funs(sent)) 
sentiment_indices_all<-cbind(Date=dates_fin, Index=sentiment_indices_all$Text) %>% as.data.frame()
sentiment_indices_all$Date <- as.Date(dates_fin,"%Y-%m-%d")
#replace NAN with 0
sentiment_indices_all$Index[is.na(sentiment_indices_all$Index)]<-0

#transform to monthly sentiments
sent_indices_all_zoo<-zoo(sentiment_indices_all[colnames(sentiment_indices_all)!="Date"],as.Date(dates_fin))
sent_indices_all_monthly<-aggregate(sent_indices_all_zoo, by=month, FUN=mean) 
sent_all_final<- sent_indices_all_monthly %>% fortify.zoo() %>% select(-1) %>% mutate(Date=dates_monthly) %>% select(Date, everything())


#same with valence shifter words
sentiment_indices_all_valence<-daily_text_prepped %>% select(Text) %>% mutate_each(funs(sent_valence))
sentiment_indices_all_valence<-cbind(Date=dates_fin,Index=sentiment_indices_all_valence$Text) %>% as.data.frame()
sentiment_indices_all_valence$Date <- as.Date(dates_fin,"%Y-%m-%d")
#replace NAN with 0
sentiment_indices_all_valence$Index[is.na(sentiment_indices_all_valence$Index)]<-0

sent_indices_all_valence_zoo<-zoo(sentiment_indices_all_valence[colnames(sentiment_indices_all_valence)!="Date"],as.Date(dates_fin))
sent_indices_all_valence_monthly<-aggregate(sent_indices_all_valence_zoo, by=month, FUN=mean) 

#sent_indices_all_valence_monthly2<-aggregate(sent_indices_all_valence_zoo, by=month, FUN=mean, na.rm=TRUE) 

sent_all_valence_final<- sent_indices_all_valence_monthly %>% fortify.zoo() %>% select(-1) %>% 
  mutate(Date=seq(as.Date("2000-01-01"),as.Date("2023-12-31"),by="months")) %>% 
  select(Date, everything())


#bind two sentiment indices

sent_all_final_monthly<-cbind(Date=dates_monthly,sent1=sent_all_final[2], sent2=sent_all_valence_final[2])
names(sent_all_final_monthly)[2]='sent1'
names(sent_all_final_monthly)[3]='sent2'

#save
write.csv(cbind(sent1=sentiment_indices_all$Text, sent2=sentiment_indices_all_valence$Index),'sentiment_all_daily.csv')
write.csv(sent_all_final_monthly,'sentiment_all_monthly.csv')
write.csv(sent_indices_all_valence_monthly,'sentiment_all_valence_only.csv')

#two ways to plot, second is commented
ggplot(sent_all_final_monthly, aes(Date)) + 
  geom_line(aes(y = sent2, colour = "SI-2")) +
  scale_x_date(breaks = pretty_breaks(12))+
  xlab('')+ylab('Sentiment Index')+
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=8),axis.title.y = element_text(size=8))


