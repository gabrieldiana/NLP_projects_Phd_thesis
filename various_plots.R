#CHECK CORRELATION
ggcorr(ts.union(CPI_INFL_TS,INFL_ALL_TS, INFL_NDGS_TS,INFL_EXP_TS, INFL_ATT_TS), label=TRUE)
future_topics_subset<-ts(final_indices_quarterly%>%dplyr::select(future_topics),start=c(2000,1), end=c(2020,4), frequency=4)
past_topis_subset<-ts(final_indices_quarterly%>%dplyr::select(past_topics), start=c(2000,1), end=c(2020,4), frequency=4)
ggcorr(ts.union(future_topics_subset,past_topis_subset),label = TRUE,hjust = 0.75, size = 1)
ggcorr(future_topics_subset,label=TRUE)
ggcorr(past_topis_subset,label=TRUE)
ggcorr(final_indices_quarterly%>%dplyr::select(26,57,63,64,74),label=TRUE)
ggcorr(ts.union(CPI_INFL_TS,INFL_ALL_TS, INFL_NDGS_TS,INFL_EXP_TS, INFL_ATT_TS,final_indices_quarterly%>%dplyr::select(11,46,8,28,74)),
       label=TRUE, hjust = 0.75, size = 1, color = "grey50")
cor_table<-round(cor(ts.union(CPI_INFL_TS,INFL_ALL_TS, INFL_NDGS_TS,INFL_EXP_TS, INFL_ATT_TS,future_topics_subset)),2)
print(xtable(cor_table, result='html'))

ggcorr(final_indices_quarterly%>%dplyr::select(10,11,12,3,4,5,6,8,45,46,44,47,48,66),label = TRUE,hjust = 0.75, size = 3)


#PLOT LDA RESULTS 
top_terms <- tidy(lda, matrix = "beta") %>%
  group_by(topic) %>%
  arrange(topic, desc(beta)) %>%
  slice(seq_len(10)) %>%
  arrange(topic, beta) %>%
  dplyr::mutate(row = row_number()) %>%
  ungroup() %>%
  dplyr::mutate(topic_num=topic, topic = paste("Topic", str_pad(topic,width= 2, pad="0"), sep = " "))


top_terms %>% filter(topic_num %in% future_topics & topic_num!=8 & topic_num!=74 & topic_num!=57 ) %>% #filter(topic_num %in% 61:80) %>% 
  group_by(topic_num) %>% 
  mutate(label=paste(term[which(beta == max(beta))],'/',term[which(beta == max(beta))-1])) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = log(beta))) +
  geom_bar(stat = "identity", show.legend = FALSE, color= "grey20", size= 0.2) +
  scale_x_reordered() +
  facet_wrap(~ label, scales = "free", ncol = 4) +
  coord_flip() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text.y = element_text(size= 10),
        axis.text.x = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) 
labs(title= "Guardian News Corpus: Top Words by Topic", y= NULL, x= NULL)
 

#Topics dendogram
bt<-lda %>% tidy(matrix= "beta")  %>% tidyr::spread(term, beta)  %>% select(-topic)# %>% mutate(topic=topic_names_2$label) 
hc<- bt %>% dist() %>% hclust(method= "ward.D")
plot(hc);rect.hclust(hc, h=6, border = "blue")



#SENTIMENT vs CCI

#choose the file that contains sentiment data
exp_data=read_excel('official_data.xlsx', sheet='INFL_EXP') %>% slice(-1) 
cci_1<-exp_data$INFL_EXP %>% round(1)
cci_2<-exp_data$INFL_ATT %>% round(1)


chosen_topics<- c(3,10,15,47,45,14,63) #manually choose relevant topics
sent_sample=sentiment_indices_quarterly %>% dplyr::select(c(chosen_topics+1))
sent_sample=sentiment_indices_quarterly %>% dplyr::select(c(chosen_topics+1))


sent_cci<-data.frame(Date=as.Date(dates_quarterly,"%Y-%m-%d"), NSI=sent_sample, CCI=cci_1, ATT=cci_2)
sent_cci_standardized<-data.frame(Date=as.Date(dates_quarterly,"%Y-%m-%d"), NSI=standardize(sent_sample), CCI=standardize(cci_1),ATT=standardize(cci_2))
  
ggplot(sent_cci_standardized, aes(Date)) + 
  geom_line(aes(y = NSI, colour = "NSI")) +
  geom_line(aes(y = CCI, colour = "CCI")) +
  geom_line(aes(y = ATT, colour = "CCI")) +
  scale_x_date(breaks = pretty_breaks(12))+
  xlab('')+ylab('Index (standardized)')+
  theme_light()+
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=11),axis.title.y = element_text(size=11))


sent_cci_standardized %>% 
  gather(key = "variable", value = "value", -Date) %>%
  ggplot( aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_x_date(breaks = pretty_breaks(12))+
  xlab('')+ylab('Frequency Index')+
  theme_light()+
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=8),axis.title.y = element_text(size=8))



#print all
sent_cci<-data.frame(Date=as.Date(dates_quarterly,"%Y-%m-%d"), NSI=sentiment_all_quarterly$sent2,CCI=cci_1, ATT=cci_2)
sent_cci_standardized<-data.frame(Date=as.Date(dates_quarterly,"%Y-%m-%d"), NSI=standardize(sentiment_all_quarterly$sent2), CCI=standardize(cci_1),ATT=standardize(cci_2))

ggplot(sent_cci_standardized, aes(Date)) + 
  geom_line(aes(y = NSI, colour = "NSI")) +
  geom_line(aes(y = CCI, colour = "CCI")) + 
  scale_x_date(breaks = pretty_breaks(12))+
  xlab('')+ylab('Index (standardized)')+
  theme_light()+
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=11),axis.title.y = element_text(size=11))






#check correlation - out of curiousity, not used in the text
cor(sent_cci_standardized$CCI, sent_cci_standardized$NSI)
var1<-na.omit(unlist(shift(sent_all_final[2],1)))
var2<-sent_cci_standardized$cci
cor(var1, var2)
ggcorr(cbind(var1,var2),hjust = 0.75, size = 2.5,layout.exp = 1, label = TRUE)


# Basic indices (frequency_only), from data_prep
chosen_topics2<-chosen_topics[-c(2,12,15)]  #to exclude topics 9,58,7
data_monthly %>% dplyr::select(-2) %>% dplyr::select(c(1,chosen_topics2+1)) %>% 
  gather(key = "variable", value = "value", -dates) %>%
  ggplot( aes(x = dates, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  # scale_x_date(breaks = pretty_breaks(12))+
  # geom_rect(data=shocks.df, aes(xmin=From, xmax=Till, ymin=ymin, ymax=ymax),fill="grey", alpha=0.3,inherit.aes = FALSE)+
  xlab('')+ylab('Frequency Index') +
  #theme_light() +
  theme(legend.position = "none",axis.title.y = element_text(size=8))+
  #theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=8),axis.title.y = element_text(size=8))+
  facet_wrap(~ variable, ncol = 3) #comment this line only to plot topics in one graph

#plot chosen final indices into one
final_indices %>% dplyr::select(c(1,chosen_topics2+1)) %>%   #86,98
  gather(key = "variable", value = "value", -Date) %>%
  ggplot( aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1)  + scale_x_date(breaks = pretty_breaks(12))+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(col="", y='News-Topic Driven Index', x=" ")

#plot all final indices separately
final_indices %>% dplyr::select(c(1,62:81)) %>% 
  gather(key = "variable", value = "value", -Date) %>%
  ggplot( aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 0.5) +
 # scale_x_date(breaks = pretty_breaks(12))+
  scale_y_continuous(limits=c(-0.75,0.2))+
  # geom_rect(data=shocks.df, aes(xmin=From, xmax=Till, ymin=ymin, ymax=ymax),fill="grey", alpha=0.3,inherit.aes = FALSE)+
  xlab('')+ylab('Frequency Index') +
  #theme_light() +
  theme(legend.position = "none",axis.title.y = element_text(size=8))+
  facet_wrap(~ variable, ncol = 4) #comment this line only to plot topics in one graph




