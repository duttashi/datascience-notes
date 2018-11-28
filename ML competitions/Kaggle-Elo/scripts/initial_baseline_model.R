# Kaggle ELo prediction challenge
# website: https://www.kaggle.com/c/elo-merchant-category-recommendation/data

# minimum required data:  train.csv and test.csv files
# What am I predicting?
# You are predicting a loyalty score for each card_id represented in test.csv and sample_submission.csv

# clean the environment
rm(list = ls())

# load required libraries
library(tidyverse)

# Load the data
train<- read.csv("ML competitions/Kaggle-Elo/data/train.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
test<- read.csv("ML competitions/Kaggle-Elo/data/test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# check data dimension and structure
dim(train)
dim(test)
str(train)
str(test)
summary(train)
summary(test)
# check missing values
sum(is.na(train))
sum(is.na(test))

# Extract the unique card id's and target values. 
userID = train$card_id
target  = train$target
userID.list = unique(sort(userID))
target.list = unique(sort(target))

size.user = length(unique(userID))
size.restaurant = length(unique(target))
# create an index using the above unique card id matched with the train and test data
userID.index = match(train$card_id,userID.list)
restaurantID.index = match(train$target,target.list)


