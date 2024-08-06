#uncomment to install the necessary packages
# install.packages("iterators")
# install.packages("doParallel")
# install.packages("foreach", dependencies=TRUE)
# install.packages("forecast")
# install.packages("rtweet")
# install.packages("dplyr")
# install.packages("tidyverse",dependencies=TRUE)
# install.packages("ggplot2", dependencies=TRUE)
# install.packages("devtools")
# install.packages("maps")
# #devtools::install_github("mkearney/rtweet")
# install.packages("ggmap")
# #devtools::install_github("dkahle/ggmap")
# install.packages("tm")
# install.packages("qdap", dependencies=TRUE, INSTALL_opts = "--no-multiarch")
# install.packages("SnowballC")
# install.packages("GuardianR")
# install.packages("RJSONIO")
# install.packages("rdian")
# install.packages("chron")
# install.packages("topicmodels", dependencies=TRUE)
# install.packages("sentiment")
# install.packages("Rgraphviz")
# install.packages("tidytext")
# install.packages("sna")
# install.packages("quanteda")   #for tokens
# install.packages("SentimentAnalysis")
# install.packages("reshape")
# install.packages("data.table")
# install.packages("TTR")
# install.packages("scorer")
# install.packages("timeDate")
# install.packages("rowr")
# install.packages("readxl")
# install.packages("imputeTS")
# install.packages("robustHD", dependencies=true)
# install.packages("BBmisc")
# install.packages("corrplot")
# install.packages("GGally")
# install.packages("aTSA")
# install.packages("rts")
# devtools::install_github("strengejacke/strengejacke")
# devtools::install_github("strengejacke/sjPlot")
# install.packages("lmtest")
# install.packages("urca")
# install.packages("astsa")
# install.packages("jtools")
# install.packages("huxtable")
# install.packages("xtable")
# install.packages('rJava') # needed for many packages to work, particularly qdap. if error then uninstall then reinstall both
# install.packages("textmineR")  #needed for creatDTM function
# install.packages("Rcpp") #needed for textmineR package
# install.packages("RWeka") #needed for tokenizing
# install.packages("ldatuning")
# install.packages("clValid")
# install.packages("purrr")
# install.packages("furrr")
# install.packages("tictoc")
# install.packages("tosca")
# install.packages('sentometrics')
# install.packages('ngram')
# install.packages('quanteda')
# install.packages('lm.beta')
# install.packages('smooth')  #needed for sma
# if (!require("pacman")) install.packages("pacman")
# install.packages("AER")
# install.packages('Rtsne')
# install.packages("sjlabelled")
# install.packages('Jmisc')
# install.packages('modelsummary')
# install.packages('kableExtra')
# install.packages('gt')
# install.packages("stargazer")
# install.packages('scorer')
# install.packages('xlsx')
# install.packages('seasonal')


#load the libraries
library(imputeTS) # for interpolation
#for geo locations
library(maps)
library(stargazer)  #for nice visuals
# Visualization
library(quanteda)
library(ngram)
library(ggplot2)
library(GGally)
library(corrplot)
library(sjPlot)
library(rts) # needed for apply.quarterly
library(gridExtra) #needed to plot multiple plots 
library(jtools) #for export_summs
library(huxtable)
library(lm.beta) #for goruping of lm regression analysis
# text mining libraries (sentiment analysis, topic modelling etc.)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tm)
library(textmineR)
library(SnowballC)
library(aTSA) #needed for adf.test
library(astsa) #needed for lag2.plot
library(Rcpp) #needed for textmineR package to run
library(forecast) #for ndiffs
library(urca)
library(ldatuning)
library(clValid) #needed for function dunn
library(purrr)
library(furrr)
library(tictoc)
library(topicmodels)
library(wordcloud)
library(quanteda)
library(SentimentAnalysis)
library(reshape) #for acast function
library(reshape2) #for melt function
library(glmnet) #lasso
library(data.table)
library(TTR) #for moving average
library(timeDate)
library(lmtest)
library(xtable) #for printing dataframes
pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr") #needed for sentimentR
library(scales) #needed for pretty_brakes
library(sentometrics) #needed for second sentiment calculation function
library(smooth)
#for working with guardian
#library(GuardianR)  #expired packaged, but the codes still run based on this
library(RJSONIO)
library(rdian)
library(doParallel)
#other - date reated etc
library(chron)
library(zoo)
library(lubridate)
library(readxl)
library(foreach)
library(iterators)
#library(robustHD) #needed for standardize()
library(BBmisc) #needed for normalize
library(tosca) #needed for plotTopic
library(zoo)
library(seasonal)
library(AER)
library(Rtsne)
library(sjlabelled)
library(Jmisc)
library(modelsummary)
library(kableExtra)
library(gt)
library(zoo)
