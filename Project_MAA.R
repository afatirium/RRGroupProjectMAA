#House pricing Model
##Load Dataset
getwd()
#setwd('C:\\Users\\Afat\\Documents\\GitHub\\RRGroupProjectMAA') #Afet add setwd for herself
train <- read.csv("train.csv", h=T)
test <- read.csv("test.csv", h=T)
row.names(train) <- train$Id
train$Id <- NULL
row.names(test) <- test$Id
test$Id <- NULL

### Data Exploration and Preprocessing
