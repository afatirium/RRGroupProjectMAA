#House pricing Model
##install libraries
#install.packages("beanplot")
#install.packages("summarytools")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("gridExtra")
#install.packages("scales")
#install.packages("zoo")


##Import libraries
library(beanplot)
library(summarytools)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(zoo)


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


## Summary for categorical variables
dfSummary(train[cat_vars]) 

#Explore the continues variables, adding summary statistic for continues variables
stat_cont <- summary(train[, sapply(train, is.numeric)])
stat_cont

tr_cont <- train[, !sapply(train, is.character)]
head(tr_cont)

corr <- cor(tr_cont)


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


obj_median <- c("LotFrontage", "MasVnrArea")

for (i in obj_median) {
  train[[i]] <- na.aggregate(train[[i]], FUN = median)
  test[[i]] <- na.aggregate(test[[i]], FUN = median)
}

#  Fill in missing values for the BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, BsmtFullBath, BsmtHalfBath, GarageCars and GarageArea features
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

obj_mode <- c("BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", "GarageCars", "GarageArea")

for (i in obj_mode) {
  test[[i]] <- replace(test[[i]], is.na(test[[i]]), Mode(test[[i]]))
}

# Fill in missing values for the GarageYrBlt feature
train$GarageYrBlt <- ifelse(is.na(train$GarageYrBlt), train$YearBuilt, train$GarageYrBlt)
test$GarageYrBlt <- ifelse(is.na(test$GarageYrBlt), test$YearBuilt, test$GarageYrBlt)

# Converting dtype to 'int64'
int_list <- c("LotFrontage", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", "GarageYrBlt", "GarageCars", "GarageArea")

for (i in int_list) {
  train[[i]] <- as.integer(train[[i]])
  test[[i]] <- as.integer(test[[i]])
}


#check the shape of dataset
dim(train)

####Feature Selection

##### 1. Correlation Analysis

# Filter numeric variables
numeric_vars <- sapply(train, is.numeric)

# Calculate correlation matrix
correlations <- cor(train[, numeric_vars])

# Sort correlations with 'SalePrice'
sorted_correlations <- sort(correlations[,"SalePrice"], decreasing = TRUE)

# Display the most positive correlations
cat("Most Positive Correlations:\n")
print(tail(sorted_correlations, 10))

# Display the most negative correlations
cat("\nMost Negative Correlations:\n")
print(head(sorted_correlations, 10))

# Area vs Sale Price
par(mfrow=c(3, 6), mar=c(5, 4, 2, 1)) # Set the layout of subplots

labels <- c("LotArea","MasVnrArea","BsmtFinSF1","BsmtFinSF2",
            "BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF",
            "LowQualFinSF","GrLivArea","GarageArea","WoodDeckSF",
            "OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea")
desc <- c("Lot Size","Masonry Seneer Area","Basement Type 1 Finished",
          "Basement Type 2 Finished","Unfinished Basement Area",
          "Total Basement Area","First Floor","Second Floor",
          "Low Quality Finished","Above Grade Living Area",
          "Size of Garage","Wood Deck Area","Open Porch Area",
          "Enclosed Porch Area","Three Season Porch Area","Screen Porch Area","Pool Area")

for (i in 1:length(labels)) {
  if (i <= 17) {
    plot(train[[labels[i]]], train[["SalePrice"]],
         xlab = desc[i], ylab = "Sale Price",
         main = desc[i], pch = 16)
  }
}

####It is clearly seen that there is positive correlation between price and areas i.e. if the area increases price will also increases expect 3SsnPorch (three season porch) and PoolArea.

#Neighborhood vs Sale Price


ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Sale Price") +
  ggtitle("Neighborhood") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Note: We can see that the prices of the house having NoRidge, NridgHt, StoneBr and Timber neighborhoods are very high.

## Building type, House Style



##Building type, House Style
labels <- c("BldgType", "HouseStyle")

plt <- ggplot(train, aes(x = train[[labels[1]]], y = train[["SalePrice"]])) +
  geom_bar(stat = "identity") +
  labs(x = NULL) +
  ggtitle(labels[1])

plt <- plt + facet_wrap(~ train[[labels[2]]], nrow = 1)

plt <- plt + theme_minimal() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1))

print(plt)

##### House Quality

par(mfrow = c(2, 4))
labels <- c("OverallQual", "ExterQual", "BsmtQual", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual")
col <- 1

for (i in labels) {
  if (col < 9) {
    plot_col <- col %% 4
    if (plot_col == 0) {
      plot_col <- 4
    }
    plot_row <- ceiling(col / 4)
    plot_index <- (plot_row - 1) * 4 + plot_col
    
    barplot(tapply(train$SalePrice, train[, i], mean), xlab = i, ylab = "SalePrice")
    title(main = i)
  }
  
  col <- col + 1
}
####Note: It can be seen that there is positive relation between quality and price. Price increases according to the quality.

###House condition

labels <- c("OverallCond", "ExterCond", "BsmtCond", "GarageCond")

plt <- ggplot(train, aes(x = train[[labels[1]]], y = train[["SalePrice"]])) +
  geom_bar(stat = "identity") +
  labs(x = NULL) +
  ggtitle(labels[1])

plt <- plt + facet_wrap(~ train[[labels[2]]], nrow = 2)

plt <- plt + theme_minimal() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(plt)

#Note: Prices of the house having Ex (excellent) and Gd (good) condition are very high.

# Bathroom condition  


# Custom function to format labels in millions
formatMillions <- function(x) {
  paste0(format(x / 1e6, big.mark = ","), "")
}

# Create individual bar plots
plot1 <- ggplot(train, aes(x = BsmtFullBath, y = SalePrice)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Sale Price (in million)") +
  ggtitle("BsmtFullBath") +
  scale_y_continuous(labels = formatMillions)

plot2 <- ggplot(train, aes(x = BsmtHalfBath, y = SalePrice)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Sale Price (in million)") +
  ggtitle("BsmtHalfBath") +
  scale_y_continuous(labels = formatMillions)

plot3 <- ggplot(train, aes(x = FullBath, y = SalePrice)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Sale Price (in million)") +
  ggtitle("FullBath") +
  scale_y_continuous(labels = formatMillions)

plot4 <- ggplot(train, aes(x = HalfBath, y = SalePrice)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Sale Price (in million)") +
  ggtitle("HalfBath") +
  scale_y_continuous(labels = formatMillions)

# Arrange the plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

######## Roof style and material
par(mfrow = c(1, 2))
labels <- c("RoofStyle", "RoofMatl")

for (i in 1:length(labels)) {
  if (i <= 2) {
    barplot(tapply(train$SalePrice, train[, labels[i]], mean), xlab = labels[i], ylab = "SalePrice")
    title(main = labels[i])
  }
}

# Basement



# Create a list of labels
labels <- c("BsmtExposure", "BsmtFinType1", "BsmtFinType2")

# Create an empty list to store the plots
plots <- list()

# Loop through the labels and create bar plots
for (i in seq_along(labels)) {
  plot <- ggplot(train, aes_string(x = labels[i], y = "SalePrice")) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Sale Price") +
    ggtitle(labels[i]) +
    scale_y_continuous(labels = dollar_format(scale = 1e-6, prefix = "$"))
  
  plots[[i]] <- plot
}


# Arrange the plots in a grid
grid.arrange(grobs = plots, ncol = 3)

#######Garage
par(mfrow = c(1, 3))
labels <- c("GarageType", "GarageFinish", "GarageCars")

for (i in 1:length(labels)) {
  if (i <= 3) {
    barplot(tapply(train$SalePrice, train[, labels[i]], mean), xlab = labels[i], ylab = "SalePrice")
    title(main = labels[i])
  }
}

##Month and Year of Sold - need to correct the pie chart
yearsold <- c(2006:2010)
monthsold <- c("January","February","March","April","May","June","July","August","September","October","November","December")
years <- table(train$YrSold)
months <- table(train$MoSold)

plt <- ggplot() +
  theme_minimal()

plt <- plt +
  geom_bar(data = data.frame(years), aes(x = factor(Var1, levels = yearsold), y = Freq),
           stat = "identity", fill = "steelblue") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, title = "Years Sold") +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plt <- plt +
  geom_bar(data = data.frame(months), aes(x = factor(Var1, levels = monthsold), y = Freq),
           stat = "identity", fill = "steelblue") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, title = "Months Sold") +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plt <- plt +
  facet_wrap(~ NULL, ncol = 3) +
  theme(plot.margin = margin(10, 10, 10, 10))

print(plt)
