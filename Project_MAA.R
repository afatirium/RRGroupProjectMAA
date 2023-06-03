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
## 1. Investigate TARGET value distribution

par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 1)) # Adjusting the margins for better visualization

# Subplot 1
hist(train$SalePrice, breaks = 50, main = "Sales Price Distribution", xlab = "Sale Price",
     ylab = "Counts", col = "lightblue", border = "white")
abline(v = mean(train$SalePrice), lty = 2, col = "red", lwd = 2)
abline(v = median(train$SalePrice), lty = 3, col = "green", lwd = 2)
legend("topright", legend = c("Mean", "Median"), lty = c(2, 3), col = c("red", "green"))

# Subplot 2
plot(train$SalePrice, rnorm(nrow(train), mean = 7, sd = 0.2), main = "Sales Price Distribution",
     xlab = "Sale Price", ylab = "Random Values", pch = 16, col = "blue", cex = 0.6)

# Subplot 3
boxplot(train$SalePrice, main = "Sales Price Distribution", ylab = "Sale Price",
        col = "lightgreen")


# Subplot 4
#install.packages("beanplot")
library(beanplot)
beanplot(train$SalePrice, main = "Sales Price Distribution", ylab = "Sale Price",
         col = "lightpink", what = c(1, 1, 0, 0), bw = 0.2)

##Note: Target is is continuous, and the distribution is skewed to the right. Mostly house price ranges from 100,000 to 200,000

## 2. Investigate Data Types
str(train)
