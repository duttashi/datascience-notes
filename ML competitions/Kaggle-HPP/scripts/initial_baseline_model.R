# Kaggle House Prices: Advanced Regression Techniques
# website: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/

# minimum required data:  train.csv and test.csv files
# What am I predicting?
# With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa, this competition challenges you to predict the final price of each home.

# Evaluation metric: RMSE
# Key idea's to win the competition
## 1. Feature Selection

# clean the environment
rm(list = ls())

# Load the required libraries
library(tidyverse)
library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Matrix)

# read all data files
scriptPath<- getwd()
dataPath<- c("ML competitions/Kaggle-HPP/data/")
setwd(dataPath)
train<- read_csv("train.csv")
test<- read_csv("test.csv")
sample<- read_csv("sample_submission.csv")

# change working directory to scripts folder
setwd(scriptPath)
# EDA
dim(train)
dim(test)
glimpse(train)
head(test)
colnames(train)
colnames(test)

# Plot distributions
hist(train$SalePrice,breaks=100, freq=FALSE, col="lightgray", main="Train_target Distribution")

# check missing data
sum(is.na(train))
sum(is.na(test))

colSums(is.na(train))
colSums(is.na(test))

# Simple fit using linear regression

# create function to plot linear regression results
# adapted from https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
ggplotRegression <- function (fit) {
  lmdf<- data.frame(fitted_values = fit$fitted.values, actual_values = fit$model[, 1])
  print(names(lmdf))
  ggplot(lmdf, aes(x = actual_values, y = fitted_values)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 4),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)
                       ),
         x = 'observed value', y = 'predicted value')
}

fit<- lm(SalePrice ~(`1stFlrSF` + `2ndFlrSF`) * OverallQual + 1, data = train)
ggplotRegression(fit)

# EDA tasks
# misisng value imputation, detect and remove high correlated variables

# Detect and remove highly correlated variables
highCor<- cor(train)
