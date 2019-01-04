# Student performance dataset hosted on Kaggle
# Reference: https://www.kaggle.com/jrosenblum123/visualizations-and-predicting-student-test-scores/code


# Load the required libraries
library(tidyverse)
library(magrittr)
library(caret) 
library(plyr) # for revalue()
library(caTools) # for sample.split()
library(dplyr)
library(randomForest)

# Read in the data
studperf.data<- read.csv("student performance/data/StudentsPerformance.csv", header = TRUE,
                         sep = ",", na.strings = NA, stringsAsFactors = TRUE)
colnames(studperf.data)
str(studperf.data)
table(studperf.data$lunch)
sum(is.na(studperf.data)) # 0 missing values

# Revalue the categorical variables
table(studperf.data$gender)
studperf.data$gender<- revalue(studperf.data$gender, c("female"="1", "male"="2"))
table(studperf.data$race.ethnicity)
studperf.data$race.ethnicity<- revalue(studperf.data$race.ethnicity, 
                                       c("group A"="1","group B"="2","group C"="3",
                                         "group D"="4","group E"="5")
                                       )
table(studperf.data$parental.level.of.education)
studperf.data$parental.level.of.education<- revalue(studperf.data$parental.level.of.education,
                                                    c("some high school"="1","high school"="1",
                                                      "some college"="2",
                                                      "associate's degree"="3", "bachelor's degree"="3",
                                                      "master's degree"="4")
                                                    )
table(studperf.data$lunch)
studperf.data$lunch<- revalue(studperf.data$lunch, c("free/reduced"="1","standard"="2"))
table(studperf.data$test.preparation.course)
studperf.data$test.preparation.course<- revalue(studperf.data$test.preparation.course,
                                                c("completed"="1","none"="2")
                                                )

str(studperf.data)

# Feature Engineering

## combining scores into one aggregate score

studperf.data<- studperf.data %>%
  mutate(composite.score = math.score + reading.score + writing.score)

# split the data into train and test
set.seed(2018)
split = sample.split(studperf.data$composite.score, SplitRatio = 0.75)
training_set = subset(studperf.data, split == TRUE)
test_set = subset(studperf.data, split == FALSE)

# Modelling and predicting with linear regression
regressor1 <- lm(composite.score ~ ., data = training_set)
y_pred1 <- predict(regressor1, newdata = test_set)
# R squared
r2.lin <- rSquared(test_set$composite.score, test_set$composite.score-y_pred1)
print(r2.lin)
# Mean Squared Error
mse.lin <- mean((test_set$composite.score - y_pred1)^2)
print(mse.lin)

## Random Forest Model
set.seed(2018)

regressor <- randomForest(x = training_set[-training_set$composite.score],
                          y = training_set$composite.score,
                          ntree = 500)

y_pred <- predict(regressor, newdata = test_set)

# R-Squared
r2 <- rSquared(test_set$composite.score, test_set$composite.score-y_pred)
print(r2)

# Mean Squared Error
mse <- mean((test_set$composite.score - y_pred)^2)
print(mse)

p <- ggplot(aes(x = test_set$composite.score, y = y_pred),
            data = data.frame(test_set$composite.score, y_pred))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))
