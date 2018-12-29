# Pet Finder Kaggle data hack competition
# Reference: https://www.kaggle.com/c/petfinder-adoption-prediction
# Evaluation metric: "quadratic weighted kappa", which measures the agreement between two ratings. This metric typically varies from 0 (random agreement between raters) to 1 (complete agreement between raters). In the event that there is less agreement between the raters than expected by chance, the metric may go below 0. The quadratic weighted kappa is calculated between the scores which are expected/known and the predicted scores.
# AdoptionSpeed - Categorical speed of adoption. Lower is faster. This is the value to predict. 


# Key idea's to win the competition
## 1. Feature Selection

# clean the workspace
rm(list = ls())
# Load the required libraries
library(tidyverse)
library(magrittr)
library(dplyr)

# read all data files
getwd()
train<- read_csv("ML competitions/kaggle-petfinder/data/train.csv")
test<- read_csv("ML competitions/kaggle-petfinder/data/test.csv")

# EDA
dim(train)
dim(test)
sum(is.na(train)) # 1257
sum(is.na(test)) # 303
colnames(train)
colnames(test)
colSums(is.na(train)) # column name has all missing values
colSums(is.na(test)) # column name has all missing values

glimpse(train)
table(train$Age)
hist(train$AdoptionSpeed)
table(train$Gender)

# save the target variable and drop it from train dataframe
y_target<- train$AdoptionSpeed
train$AdoptionSpeed<- NULL

# fill missing names with -1
train[is.na(train)] <- "-1"
test[is.na(test)]<- "-1"

# coerce categorical vars to factors as per data dictionary
names(train)
train$Type<- as.factor(train$Type)
train$Name<- as.factor(train$Name)
train$Breed1<- as.factor(train$Breed1)
train$Breed2<- as.factor(train$Breed2)
train$Gender<- as.factor(train$Gender)
train$Color1<- as.factor(train$Color1)
train$Color2<- as.factor(train$Color2)
train$Color3<- as.factor(train$Color3)
train$MaturitySize<- as.factor(train$MaturitySize)
train$FurLength<- as.factor(train$FurLength)
train$Vaccinated<- as.factor(train$Vaccinated)
train$Dewormed<- as.factor(train$Dewormed)
train$Sterilized<- as.factor(train$Sterilized)
train$Health<- as.factor(train$Health)
train$State<- as.factor(train$State)
train$Description<- as.character(train$Description)
train$AdoptionSpeed<- as.factor(train$AdoptionSpeed)

# Revalue the categorical variables for visualization purpose
library(plyr) # for revalue()
table(train$State)
train$State<- revalue(train$State, c("41324"="Melaka","41325"="Kedah","41326"="Selangor",
                                     "41327"="Pulau Pinang","41330"="Perak","41332"="Negeri Sembilan",
                                     "41335"="Pahang","41336"="Johor","41342"="Sarawak","41345"="Sabah",
                                     "41361"="Terengganu","41367"="Kelantan","41401"="Kuala Lumpur",
                                     "41415"="Labuan")
                      )

# rearrange the vars- categorical first followed by continuous
str(train)
train.n<- train[,c(1:2,4:15,18:19,21:22,3,16:17,20,23:24)]
str(train.n)

# cols to drop
dropCols<- c(2,16:18)
train.n<- train.n[,-dropCols]
str(train.n)

#### Visualization
# CREATING A MANUAL COLOR PALETTE
library(RColorBrewer) # for brewer.pal()
library(ggplot2)
mycolors = c(brewer.pal(name="Set2", n = 8), 
             brewer.pal(name="Set1", n = 6))

# CREATE A CUSTOM THEME for Journal IEEE Access
mytheme<- function(base_size = 11, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(plot.title = element_text(family="Times", size = 11,
                                    # where t=top margin, r=right margin, b=bottom margin, l=left margin
                                    margin = margin(t = 00, r = 0, b = 10, l = 0)
    ), # end element_text()
    plot.caption = element_text(hjust=0.5, vjust = 0.5, size=rel(1.2)),
    axis.text = element_text(family="Times", size = 8),
    axis.title.x = element_text(family="Times", size = 10,
                                margin = margin(t = 10, r = 0, b = 0, l = 0)
    ),
    axis.title.y = element_text(family="Times", size = 10,
                                angle = 90,
                                margin = margin(t = 0, r = 10, b = 0, l = 0)
    ),
    legend.title = element_text(family="Times", size = 10),
    legend.text = element_text(family="Times", size = 9),
    panel.border=element_rect(fill=NA, size = 0.2),
    legend.background = element_rect(fill="gray90", size=.5, 
                                     linetype="dotted"))
}

# create a ggplot2 object
p<-ggplot(data = train, aes(x = State))

p +geom_bar(stat = "count", color="black",aes(fill = Type))+
  mytheme()+
  ggtitle("(A) States")+
  scale_x_discrete(name="State Name")+
  scale_y_continuous(name = "Count of pets")+
  scale_color_manual(values = mycolors)+
  scale_fill_discrete(name = "Type")

# pick some cols at random for further analysis
str(train)

library(caret)
# Reference: https://topepo.github.io/caret/pre-processing.html#identifying-correlated-predictors

# correlation in categorical vars. Use chi-square test
chi2 = chisq.test(train[,c(4,5)], correct=F)
c(chi2$statistic, chi2$p.value)





descrCor <-  cor(somecols)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .72)
filteredDescr <- somecols[,-highlyCorDescr] # the variable `Vaccinated` is high correlated
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

names(somecols)
names(filteredDescr)
# identifying linear dependencies
comboInfo <- findLinearCombos(somecols)
comboInfo # No linear dependencies found

# preprocessing
pp_hpc <- preProcess(filteredDescr,method = c("center", "scale", "YeoJohnson"))
pp_hpc

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(2018)
names(filteredDescr)

gbmFit1 <- train(AdoptionSpeed ~ ., data = filteredDescr, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1