#House pricing Model
##Load Dataset
getwd()
train <- read.csv("train.csv", h=T)
test <- read.csv("test.csv", h=T)
row.names(train) <- train$Id
train$Id <- NULL
row.names(test) <- test$Id
test$Id <- NULL
