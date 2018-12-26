# Kaggle Two Sigma News prediction competition
# webpage: https://www.kaggle.com/c/two-sigma-financial-news
# Question: Can we use the content of news analytics to predict stock price performance?

# load required libraries
library(caret) # for nearzerovar()
library(data.table) # for merge()


# load the data
marketdata<- read.csv("ML competitions/Kaggle-TwoSigma/data/marketdata_sample.csv", 
                      sep = ",", stringsAsFactors = TRUE, na.strings=c("","NA"))

str(marketdata)
newsdata<- read.csv("ML competitions/Kaggle-TwoSigma/data/news_sample.csv", 
                    sep = ",", stringsAsFactors = TRUE, na.strings=c("","NA"))

# EDA
dim(marketdata)
dim(newsdata)
names(marketdata)
names(newsdata)
sum(is.na(marketdata)) # 400 missing
sum(is.na(newsdata)) # 64 missing
colSums(is.na(marketdata)) # returnsClosePrevMktres1(100),returnsOpenPrevMktres1(100),returnsClosePrevMktres10(100),returnsOpenPrevMktres10(100)
colSums(is.na(newsdata)) # all missing values in headlineTag only

# check for near zero variance property
badCols<- nearZeroVar(marketdata)
dim(marketdata[, badCols]) # 5 variables with near zero variance property
colnames(marketdata[,badCols])
range(marketdata$returnsClosePrevMktres1) # all blank
range(marketdata$returnsClosePrevMktres10) # all blank
# drop the nzv variables
marketdata.c<- marketdata[,-badCols]

badCols<- nearZeroVar(newsdata)
dim(newsdata[,badCols]) # null

str(marketdata.c)
str(newsdata)
