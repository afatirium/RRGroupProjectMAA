#House pricing Model
##install libraries
#install.packages("beanplot")
#install.packages("summarytools")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("gridExtra")
#install.packages("scales")
#install.packages("zoo")
#install.packages("dplyr")


#install.packages("gbm")
#install.packages("glmnet")
#install.packages("xgboost")
#install.packages("e1071")






##Import libraries
library(beanplot)
library(summarytools)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(zoo)
library(dplyr)
library(gbm)
library(glmnet)
library(xgboost)
library(e1071)


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

library(scales)

labels <- c("BldgType", "HouseStyle")

plt <- ggplot(train, aes(x = train[[labels[1]]], y = train[["SalePrice"]])) +
  geom_bar(stat = "identity") +
  labs(x = NULL) +
  ggtitle(labels[1])

plt <- plt + facet_wrap(~ train[[labels[2]]], nrow = 1)

plt <- plt + theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
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
cols <- 1

plots <- list()

for (i in labels) {
  if (cols < 5) {
    plt <- ggplot(train, aes(x = train[[i]], y = train[["SalePrice"]])) +
      geom_bar(stat = "identity") +
      labs(x = NULL) +
      ggtitle(i) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14),
            axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
    
    plots[[cols]] <- plt
  }
  cols <- cols + 1
}

grid.arrange(grobs = plots, nrow = 2, ncol = 2, top = "Subplots")



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

##Month and Year of Sold 
par(mfrow = c(1, 3))
yearsold <- c(2006:2010)
monthsold <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
years <- table(train$YrSold)
months <- table(train$MoSold)

pie(years, labels = yearsold, explode = c(0.1, 0.1, 0.1, 0.1, 0.1), col = rainbow(length(yearsold)), main = "Year Sold", cex.main = 1.2)
pie(months, labels = monthsold, explode = rep(0.1, 12), col = rainbow(length(monthsold)), main = "Month Sold", cex.main = 1.2)

#Note: Maximum houses were sold in year 2009 and in month of June.
# We can also see a decline in sales from 2009 to 2010.

### Transform Skewed Features
# Plot histogram for each continuous feature to see if a transformation is necessary

par(mfrow = c(4, 5), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
features <- colnames(train)

col <- 1
for (feature in features) {
  if (class(train[[feature]]) != "character") {
    if (!(feature %in% c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageYrBlt", "GarageCars", "MoSold", "YrSold", "SalePrice"))) {
      if (col < 21) {
        hist(train[[feature]], main = paste("Histogram for", feature), xlab = "", ylab = "", col = "skyblue")
      }
      col <- col + 1
    }
  }
}

#Box-Cox Power Transformation
train$LotFrontage <- train$LotFrontage^(1/3)
train$LotArea <- train$LotArea^(1/6)
train$MasVnrArea <- train$MasVnrArea^(1/1.5)
train$BsmtFinSF1 <- train$BsmtFinSF1^(1/1.4)
train$BsmtUnfSF <- train$BsmtUnfSF^(1/1.5)
train$TotalBsmtSF <- train$TotalBsmtSF^(1/1.4)
train$X1stFlrSF <- train$X1stFlrSF^(1/4)
train$GrLivArea <- train$GrLivArea^(1/4.5)
train$GarageArea <- train$GarageArea^(1/1.1)
train$WoodDeckSF <- train$WoodDeckSF^(1/1.2)
train$OpenPorchSF <- train$OpenPorchSF^(1/2.5)


### Convert Categorical Features To Numeric


features <- colnames(train)

for (feature in features) {
  if (class(train[[feature]]) == "character") {
    train[[feature]] <- as.integer(factor(train[[feature]]))
    test[[feature]] <- as.integer(factor(test[[feature]]))
  }
}

##Split Into Train And Test Set

# Drop unnecessary features
X <- train[, !colnames(train) %in% c("SalePrice")]

# Create the target variable
y <- train$SalePrice

# Set the seed for reproducibility
set.seed(42)

# Split the data into training and test sets
train_indices <- sample(1:nrow(train), size = round(0.8 * nrow(train)), replace = FALSE)
X_train <- X[train_indices, ]
y_train <- y[train_indices]

X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

### Standardize Features
# Fit the scaler on the training data

scaler <- scale(X_train)

# Apply the scaler to the training data
X_train_scaled <- scaler

# Apply the scaler to the test data
X_test_scaled <- scale(X_test, center = attr(scaler, "scaled:center"), scale = attr(scaler, "scaled:scale"))

##
# Calculate the mean and standard deviation from the training set
scale_params <- apply(X_train, 2, mean)
scale_sd <- apply(X_train, 2, sd)

# Scale the training set
X_train_scaled <- scale(X_train, center = scale_params, scale = scale_sd)

# Scale the test set using the scaling parameters from the training set
X_test_scaled <- scale(X_test, center = scale_params, scale = scale_sd)

##MODELs

#Let's build initial functions for the models.
# build finction for loss function
rmse_cv <- function(model) {
  rmse <- sqrt(-mean(cross_val_score(model, X, y, scoring = "neg_mean_squared_error", cv = 5)))
  return(round(rmse, 4))
}

evaluation <- function(y, predictions) {
  mae <- mean(abs(y - predictions))
  mse <- mean((y - predictions)^2)
  rmse <- sqrt(mse)
  r_squared <- 1 - (sum((y - predictions)^2) / sum((y - mean(y))^2))
  
  return(list(mae = round(mae, 4), mse = round(mse, 4), rmse = round(rmse, 4), r_squared = round(r_squared, 4)))
}

models <- data.frame(Model = character(),
                     MAE = numeric(),
                     MSE = numeric(),
                     RMSE = numeric(),
                     R2_Score = numeric(),
                     RMSE_CV = numeric(),
                     stringsAsFactors = FALSE)











# Gradient Boosting Model
library(gbm)

# Fit the Gradient Boosting model
g_boost <- gbm(y_train ~ ., data = X_train, n.trees = 100, interaction.depth = 3)

# Predict on the test set
predictions <- predict(g_boost, newdata = X_test, n.trees = 100)

# Calculate evaluation metrics
metrics <- evaluation(y_test, predictions)
mae <- metrics$mae
mse <- metrics$mse
rmse <- metrics$rmse
r_squared <- metrics$r_squared

cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R2 Score:", r_squared, "\n")
cat("----------------------------------\n")


# Perform cross-validation using cv.glmnet
cv_fit <- cv.glmnet(x = as.matrix(X), y = y, nfolds = 5, alpha = 0.5)

# Calculate the RMSE
rmse_cv <- sqrt(cv_fit$cvm)

# Print the RMSE for each fold
cat("RMSE (Cross-Validation):", rmse_cv, "\n")

# Get the average RMSE
mean_rmse_cv <- mean(rmse_cv)
cat("Mean RMSE (Cross-Validation):", mean_rmse_cv, "\n")

# Append the average RMSE to the models data frame
models$`RMSE (Cross-Validation)`[models$Model == "GradientBoosting"] <- mean_rmse_cv


# Create a new row for the model results
new_row <- data.frame(Model = "GradientBoosting",
                      MAE = mae,
                      MSE = mse,
                      RMSE = rmse,
                      R2_Score = r_squared,
                      RMSE_CV = mean_rmse_cv)

# Append the new row to the models data frame
models <- rbind(models, new_row)



# Extreme Gradient Boosting (XGBoost Model)

# Convert the data to DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test))

# Set the parameters for the XGBoost model
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# Train the XGBoost model
Xg_boost <- xgb.train(params = params, data = dtrain, nrounds = 100)

# Predict on the test set
predictions <- predict(Xg_boost, newdata = dtest)

# Calculate evaluation metrics
mae <- mean(abs(predictions - y_test))
mse <- mean((predictions - y_test)^2)
rmse <- sqrt(mse)
r_squared <- 1 - mse / var(y_test)

cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R2 Score:", r_squared, "\n")
cat("----------------------------------\n")

# Perform cross-validation to calculate RMSE
cv_result <- xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = 100)
rmse_cross_val <- min(cv_result$evaluation_log$test_rmse_mean)

cat("RMSE Cross-Validation:", rmse_cross_val, "\n")

# Create a new row for the model results
new_row <- data.frame(Model = "XGradientBoosting",
                      MAE = mae,
                      MSE = mse,
                      RMSE = rmse,
                      R2_Score = r_squared,
                      RMSE_CV = rmse_cross_val)

# Append the new row to the models data frame
models <- rbind(models, new_row)


### Support Vector Regression (SVR Model)

# Fit the SVR model
svr <- svm(X_train, y_train, kernel = "radial", cost = 100000)

# Predict on the test set
predictions <- predict(svr, X_test)

# Calculate evaluation metrics
mae <- mean(abs(predictions - y_test))
mse <- mean((predictions - y_test)^2)
rmse <- sqrt(mse)
r_squared <- 1 - mse / var(y_test)

cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R2 Score:", r_squared, "\n")
cat("----------------------------------\n")

# Perform cross-validation to calculate RMSE
install.packages("caret", dependencies = TRUE)
update.packages()


library(caret)

# Define the train control with 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform cross-validation using train() function
cv_model <- train(x = X, y = y, method = "svmRadial", trControl = ctrl)

# Get the cross-validated RMSE
rmse_cv <- cv_model$results$RMSE

cat("RMSE (Cross-Validation):", rmse_cv, "\n")

# Create a new row for the model results
new_row <- data.frame(Model = "SVR",
                      MAE = mae,
                      MSE = mse,
                      RMSE = rmse,
                      R2_Score = r_squared,
                      RMSE_CV = rmse_cv)

# Append the new row to the models data frame
models <- rbind(models, new_row)

models
