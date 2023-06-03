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

#- Some variables, such as **YearBuilt, YearRemodAdd, GarageYrBlt, YrSold, MoSold** are numeric variables, which is related to the year.
#- Some variables have a lot of null value, it can be cleaned.

# Explore the categorical variables
cat_vars <- names(train)[sapply(train, is.character)]
cat_vars

## **MSSubClass** is also categorical by definition, despite its numeric values. 
## MSSubClass** should be added to the list of categorical variables

cat_vars <- c(cat_vars, "MSSubClass")
cat_vars

length(cat_vars)
train[cat_vars] <- lapply(train[cat_vars], as.character)
t(sapply(train[cat_vars], summary))

#install.packages("summarytools")
library(summarytools)
## Summary for categorical variables
dfSummary(train[cat_vars]) 

#Explore the continues variables, adding summary statistic for continues variables
stat_cont <- summary(train[, sapply(train, is.numeric)])
stat_cont

tr_cont <- train[, !sapply(train, is.character)]
head(tr_cont)

corr <- cor(tr_cont)
library(ggplot2)
library(tidyr)

melted_corr <- as.data.frame(as.table(corr))
colnames(melted_corr) <- c("Var1", "Var2", "value")

# Increase the plot size
options(repr.plot.width = 30, repr.plot.height = 30)

ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Set the tile border color to white
  scale_fill_gradient(low = "red", high = "yellow") +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
        panel.grid = element_blank())  # Remove grid lines

# Select continuous features to plot
features <- names(tr_cont)[!(names(tr_cont) %in% c("Id", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd",
                                                   "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath",
                                                   "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces",
                                                   "GarageYrBlt", "GarageCars", "MoSold", "YrSold", "SalePrice"))]

for (i in 1:length(features)) {
  hist(tr_cont[[features[i]]], main = paste("Histogram for", features[i]), xlab = features[i], col ="lightblue")
}

######## Missing values
# Check the missing values for train dataset
missing_values <- colSums(is.na(train))
missing_values <- missing_values[order(-missing_values)]


# Display the top 20 variables with the highest number of missing values
head(missing_values, 20)

# Check the missing values for test dataset
missing_values <- colSums(is.na(test))
missing_values <- missing_values[order(-missing_values)]

# Display the top 35 variables with the highest number of missing values
head(missing_values, 35)

##Note: For both dataset some variables have a lot missing values: PoolQC, MiscFeature, Alley, Fence, FireplaceQu, LotFrontage

# Drop the "PoolQC" column from the train dataset
train <- train[, !(colnames(train) %in% "PoolQC")]

# Drop the "PoolQC" column from the test dataset
test <- test[, !(colnames(test) %in% "PoolQC")]


#########Fill the missing variables for the categorical variables:

# Define the features with missing values
obj_NA <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
            "BsmtFinType2", "FireplaceQu", "GarageType", "GarageFinish",
            "GarageQual", "GarageCond", "Fence", "MiscFeature")

# Fill in missing values with "NA" for the specified features in the train dataset
for (i in obj_NA) {
  train[, i][is.na(train[, i])] <- "NA"
}

# Fill in missing values with "NA" for the specified features in the test dataset
for (i in obj_NA) {
  test[, i][is.na(test[, i])] <- "NA"
}

# Fill in missing values for the MSZoning, MasVnrType, Electrical, KitchenQual, Functional and SaleType 
obj_mode <- c("MSZoning", "MasVnrType", "Electrical", "KitchenQual", "Functional", "SaleType")

train$MSZoning[train$MSZoning == "C (all)"] <- "C"
test$MSZoning[test$MSZoning == "C (all)"] <- "C"

for (i in obj_mode) {
  train[[i]][is.na(train[[i]])] <- names(which.max(table(train[[i]])))
  test[[i]][is.na(test[[i]])] <- names(which.max(table(test[[i]])))
}

# Fill in missing values for the Utilities feature
test$Utilities[is.na(test$Utilities)] <- "NoSeWa"

# Fill in missing values for the Exterior1st feature
test$Exterior1st[is.na(test$Exterior1st)] <- "ImStucc"
# Fill in missing values for the Exterior2nd feature
test$Exterior2nd[is.na(test$Exterior2nd)] <- "Other"

##Clean continues variables:
##Fill missing variables

# # Fill in missing values for the LotFrontage and MasVnrArea features
library(zoo)

obj_median <- c("LotFrontage", "MasVnrArea")

for (i in obj_median) {
  train[[i]] <- na.aggregate(train[[i]], FUN = median)
  test[[i]] <- na.aggregate(test[[i]], FUN = median)
}

