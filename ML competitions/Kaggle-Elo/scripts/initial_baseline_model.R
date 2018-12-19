# Kaggle ELo prediction challenge
# website: https://www.kaggle.com/c/elo-merchant-category-recommendation/data

# minimum required data:  train.csv and test.csv files
# What am I predicting?
# You are predicting a loyalty score for each card_id represented in test.csv and sample_submission.csv

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
train<- read_csv("ELO/data/train.csv")
test<- read_csv("ELO/data/test.csv")
histdata<- read_csv("ELO/data/historical_transactions.csv")
newdata<- read_csv("ELO/data/new_merchant_transactions.csv")
merchants<-read_csv("ELO/data/merchants.csv")
sample<- read_csv("ELO/data/sample_submission.csv")

# EDA
dim(train)
dim(test)

glimpse(train)
head(test)

dim(histdata)
head(histdata)
glimpse(histdata)

dim(merchants)
head(merchants)

summary(train)

# plot distribution, Histogram
hist(train$target,breaks=100, freq=FALSE, col="lightgray", main="Train_target Distribution")

# train feature
table(train$feature_1)
table(train$feature_2)
table(train$feature_3)

# EDA for historical data
table(histdata$authorized_flag)
prop.table(table(histdata$authorized_flag))
table(histdata$category_1)
prop.table(table(histdata$category_1))
table(histdata$category_2)
prop.table(table(histdata$category_2))
table(histdata$category_3)
prop.table(table(histdata$category_3))
summary(histdata$purchase_amount)
summary(histdata$month_lag)
glimpse(histdata)

# Feature Selection on historical transactional data

## create new variables from the hist_transaction_data
hist_cat1 <- histdata %>% group_by(card_id,category_1) %>% 
  summarize(count=n())%>%
  spread(key=category_1, value=count)

hist_cat2 <- histdata %>% group_by(card_id,category_2) %>% 
  summarize(count=n())%>%
  spread(key=category_2, value=count)

hist_cat3 <- histdata %>% group_by(card_id,category_3) %>% 
  summarize(count=n())%>%
  spread(key=category_3, value=count)

#count unique categories and merchant
hist_summary2 <- histdata %>% group_by(card_id)%>%
  summarise_at(vars(starts_with("merchant_"),starts_with("category")), n_distinct, na.rm = TRUE)
# purchase term, how many days between each transaction.
hist_summary3 <- histdata %>% group_by(card_id)%>%
  summarize(no_trans=n(), 
            pur_term = as.integer(diff(range(purchase_date))),
            avg_term = as.integer(mean(abs(diff(order(purchase_date)))))                    
  )
fn <- funs(sum, mean, min, max, sd, n_distinct, .args = list(na.rm = TRUE))
histdata$authorized_flag <- ifelse(histdata$authorized_flag == "Y",1,0)

hist_summary <- histdata %>%
  group_by(card_id) %>% 
  select(c("card_id","purchase_amount","month_lag","installments","authorized_flag")) %>%
  summarize_all(fn) %>%
  left_join(hist_summary2,by="card_id") %>%
  left_join(hist_summary3,by="card_id") %>%
  left_join(hist_cat1,by="card_id") %>%
  left_join(hist_cat2[,-7],by="card_id") %>%
  left_join(hist_cat3[,-5],by="card_id") 

head(hist_summary)

# new merchants transaction data
# newdata category
new_cat1 <- newdata %>% group_by(card_id,category_1) %>% 
  summarize(count=n())%>%
  spread(key=category_1, value=count)

new_cat2 <- newdata %>% group_by(card_id,category_2) %>% 
  summarize(count=n())%>%
  spread(key=category_2, value=count)

new_cat3 <- newdata %>% group_by(card_id,category_3) %>% 
  summarize(count=n())%>%
  spread(key=category_3, value=count)
# rename columns
colnames(new_cat1) <- c("card_id","new_N", "new_Y")
colnames(new_cat2) <- c("card_id","new1", "new2","new3","new4","new5","na")
colnames(new_cat3) <- c("card_id","new_A", "new_B", "new_C", "na")
# merge new transaction & merchants data
new_merge <- newdata %>%
  left_join(merchants, by="merchant_id",suffix = c("_new", "_m"))
new_summary2 <- new_merge %>% group_by(card_id)%>%
  summarise_at(vars(starts_with("merchant_"),starts_with("category")), n_distinct, na.rm = TRUE)
new_summary3 <- new_merge %>%
  group_by(card_id) %>% 
  summarise(
    new_no_trans=n(), 
    new_pur_term = as.integer(diff(range(purchase_date))),
    new_avg_term = as.integer(mean(abs(diff(order(purchase_date)))))
  )
new_merge$authorized_flag <- ifelse(new_merge$authorized_flag == "Y",1,0)

new_summary <- new_merge %>%
  group_by(card_id) %>% 
  select(c("card_id","purchase_amount","month_lag","installments","authorized_flag","avg_purchases_lag3","avg_purchases_lag6","avg_purchases_lag12")) %>%
  summarize_all(fn) %>%
  left_join(new_summary2,by="card_id") %>%
  left_join(new_summary3,by="card_id") %>%
  left_join(new_cat1, by="card_id") %>%
  left_join(new_cat2[,-7],by="card_id") %>%
  left_join(new_cat3[,-5],by="card_id") 

head(new_summary)

# training data
train_data <- train %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         howlong = as.integer(ymd("2018-02-01") - first_active_month)) %>%
  left_join(hist_summary, by="card_id") %>%
  left_join(new_summary,by="card_id",suffix = c("", "_new"))

head(train_data)

# fill 0 to NA 
train_data[is.na(train_data)] <- 0
head(train_data)
dim(train_data)

# test data
test_data <- test %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         howlong = as.integer(ymd("2018-02-01") - first_active_month)) %>%
  left_join(hist_summary, by="card_id") %>%
  left_join(new_summary,by="card_id",suffix = c("", "_new"))

test_data1 <- test_data[,-c(1,2)]
test_data1[is.na(test_data1)] <- 0
data <- train_data[,-c(1,2)]
test.final <- test_data1

# Model Training

## Light GBM Model
library(caret)
set.seed(1)

inTrain <- createDataPartition(data$target, p=.8)[[1]]
dataTr <- data[inTrain,]
dataTs <- data[-inTrain,]

library(lightgbm)
# lgb dataset
train_s <- as.matrix(dataTr[,-4])
dtrain <- lgb.Dataset(data=train_s, label = dataTr$target)

test_s <- as.matrix(dataTs[,-4])

params <- list(objective="regression",
               metric = "l2",
               #                 min_sum_hessian_in_leaf = 1,
               feature_fraction = 0.7,
               bagging_fraction = 0.7,
               bagging_freq = 5,
               #                 min_data = 100,
               max_bin = 50,
               lambda_l1 = 8,
               lambda_l2 = 1.3
               #                 min_data_in_bin=100,
               #                 min_gain_to_split = 10,
               #                 min_data_in_leaf = 30
               #is_unbalance = TRUE
)

lgb_m <- lgb.train(params=params,
                   data=dtrain,
                   min_data =1, 
                   learning_rate=0.1, 
                   nrounds = 500)
preds_lgb <- predict(lgb_m, test_s)
summary(preds_lgb)
rmse(dataTs$target,preds_lgb)

# on test data
train_lgb1 <- as.matrix(data[,-4])
train_lgb <- lgb.Dataset(data=train_lgb1, label = data$target)

test_lgb <- as.matrix(test.final)

lgb_model <- lgb.train(params=params,
                       data=dtrain,
                       min_data =1, 
                       learning_rate=0.1, 
                       nrounds = 500) 
preds_lgb <- predict(lgb_model, test_lgb)

summary(preds_lgb)

## XGBoost Model
library(xgboost)
params <- list(objective = "reg:linear",
               booster = "gbtree",
               eval_metric = "rmse",
               nthread = 4,
               eta = 0.01,
               max_depth = 8,
               min_child_weight = 5,
               gamma = 1,
               subsample = 0.8,
               colsample_bytree = 0.7,
               colsample_bylevel = 0.6,
               alpha = 0.1,
               lambda = 5)
data_train_final <- as.matrix(data[,-4])
dtrain_final <- xgb.DMatrix(data = data_train_final, label = data$target)

data_test_final <- as.matrix(test.final)
dtest_final <- xgb.DMatrix(data = data_test_final)

xgb_model <- xgboost(params = params, 
                     data=dtrain_final, 
                     nrounds = 1000, 
                     print_every_n = 100, 
                     early_stopping_rounds = 50)

# prediction
preds <- predict(xgb_model, newdata = dtest_final) 
summary(preds)

importance <- xgb.importance(feature_names = colnames(dtrain_final), model = xgb_model)
importance 

importance%>% 
  ggplot()+ geom_col(aes(x=reorder(Feature, Gain), y=Gain), fill ="blue") + coord_flip() + 
  labs(title = "Most important features", x="Feature")+ theme_minimal()

# combine 2 model predictions 
avg_preds <- (preds_lgb+preds)/2
# export result as csv
card_id <- test_data[,2]

# lgb
result2 <- cbind(card_id,preds_lgb)
colnames(result2) <- c("card_id","target") 
write.csv(result2, file = "lgb_submission.csv",row.names=FALSE)

# xgboost
result1 <- cbind(card_id,preds)
colnames(result1) <- c("card_id","target") 
write.csv(result1, file = "xgboost_submission.csv",row.names=FALSE)

# avg
result3 <- cbind(card_id,avg_preds)
colnames(result3) <- c("card_id","target") 
write.csv(result2, file = "avg_submission.csv",row.names=FALSE)


