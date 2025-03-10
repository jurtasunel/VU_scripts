#################################################
### COURSE: Genes, Bioinformatics and Disease ###
### COURSE CODE: 900255SCIY                   ###
### PRACTICAL: Tumor Detection                ###
### AUTHOR: Josemari Urtasun Elizari          ###
### LAST MODIFIED: 10/03/2025                 ###
#################################################

### Documentation (1-ROC plot, 2-plot in r, 3-PCA, 4-KNN with class. 5-KNN with caret, class, etc.):
#https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc
#https://r-coder.com/plot-r/
#https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/
#https://www.geeksforgeeks.org/k-nn-classifier-in-r-programming/
#https://www.datacamp.com/tutorial/k-nearest-neighbors-knn-classification-with-r-tutorial

### Libraries.
library(caret)
library(class)

### Functions.
# Function to split dataset. Input is one dataframe and split value (between 0 and 1), and outputs are train set and test set.
splitdata <- function(dataframe, splitperc){
  
  # Use the input value to get the row number to split the dataframe.
  rowsplit <- round(nrow(dataframe) * splitperc)
  
  # Split train from test set.
  trainset <- dataframe[1:rowsplit,]
  testset <- dataframe[(rowsplit + 1):nrow(dataframe),]
  
  # Return sets.
  return(list(trainset = trainset, testset = testset))  
}
# This can also be done with caret function createDataPartition() from caret (balanced) or sampleSplit() from caTools (random).

# Function to scale with min-max.
minmax <- function(dataframe){
  
  for (column in 1:col(dataframe)){
    
    
  }
  return(scaled_dataframe)
}
# The two functions below can be done simply with base R: scale(x, center = TRUE, scale = TRUE).
# Function to center a dataframe by subtracting the mean of each column.
center <- function(dataframe){
  
  # Make new dataframe to modify.
  center_dataframe <- dataframe
  
  # Loop through columns and get the mean of each column.
  for (column in 1:ncol(center_dataframe)){
    imean <- mean(center_dataframe[,column])
    
    # Loop through rows and substract the mean from each element.
    for (row in 1:nrow(center_dataframe)){
      center_dataframe[row,column] <- center_dataframe[row,column] - imean
    }
  }
  
  # Return centered dataframe.
  return(center_dataframe)
}
# Function to scale the data to z-scores (dividing by the standard deviation on each column).
zscore <- function(dataframe){
  
  # Make new dataframe to modify.
  scaled_datagrame <- dataframe
  
  # Loop through columns and get the mean of each column.
  for (column in 1:ncol(scaled_datagrame)){
    isd <- sd(scaled_datagrame[,column])
    
    # Loop through rows and substract the mean from each element.
    for (row in 1:nrow(scaled_datagrame)){
      scaled_datagrame[row,column] <- scaled_datagrame[row,column] / isd
    }
  }
  
  # Return centered dataframe.
  return(scaled_datagrame)
  
}
# This functions can also be done with preProcess() from caret and set the method = "range"/"scale"/"center".

### Data.
setwd("/home/josemari/Desktop/MasterVU/2024/GBD")
inputfile <- as.data.frame(read.csv("Table_S6.csv"))


###########
### ROC ###
###########

# Sort the data by decreasing order of prediction value.
rocdf <- inputfile[order(inputfile$CancerSEEK.Logistic.Regression.Score, decreasing = TRUE),]
# Get unique prediction values to use as thresholds.
unique_vals <- unique(rocdf$CancerSEEK.Logistic.Regression.Score)

# Make vectors to store TPR and FPR. Add 0 because is not a value on the dataset.
TPR <- c(0)
FPR <- c(0)

# Loop through the unique values, using each of them as a threshold.
for (threshold in unique_vals){
  
  # Calculate TP, FP, TN, FN.
  TP <- sum(rocdf$Tumor.type != "Normal" & rocdf$CancerSEEK.Logistic.Regression.Score >= threshold)
  FP <- sum(rocdf$Tumor.type == "Normal" & rocdf$CancerSEEK.Logistic.Regression.Score >= threshold)
  TN <- sum(rocdf$Tumor.type == "Normal" & rocdf$CancerSEEK.Logistic.Regression.Score < threshold)
  FN <- sum(rocdf$Tumor.type != "Normal" & rocdf$CancerSEEK.Logistic.Regression.Score < threshold)
    
  # Calculate tpr and fpr for current threshold.
  tpr <- TP / (TP + FN)
  fpr <- FP / (FP + TN)
  # Append to the TPR and FPR vectors.
  TPR <- c(TPR, tpr)
  FPR <- c(FPR, fpr)
}

# Plot the ROC plot.
plot(FPR, TPR, type = "b", col = "darkblue", main = "ROC plot")
abline(0,1, col = "darkred", lty = 2) # Diagonal with intercept y = 0 and slope of 1. Lty = 2 for dashed line.


###########
### KNN ###
###########

# Step 1: Preprocessing data.
#############################

# Remove all rows with NA values.
cleandata <- inputfile[complete.cases(inputfile),]
rownames(cleandata) <- paste0("sample", 1:nrow(cleandata))
# Remove metadata
cleandata <- cleandata[,5:ncol(cleandata)]
# Improvement --> remove or limit extreme outliers to not bias the model.

# Step 2: Split dataset.
########################

# Separate train and test with function from caret. This keeps class balance between train and testset. 
set.seed(123)
split <- createDataPartition(cleandata$CancerSEEK.Test.Result, p = 0.8, list = FALSE) # list false makes a vector of rows.
trainset <- cleandata[split,]
testset <- cleandata[-split,]
# Improvements --> modify split ratio depending on data. Use sampleSplit() from caTools for a total random split without balance check. 

# Separate features and labels.
train_features <- trainset[, -ncol(trainset)]
train_labels <- trainset[, ncol(trainset)]
test_features <- testset[, -ncol(testset)]
test_labels <- testset[, ncol(testset)]

# Step 3: Center and scale/normalize.
#####################################

# Get scaling values from training data using preProcess() from caret.
scaling_values <- preProcess(train_features, method = "range")
# Apply to training and test features.
train_features <- predict(scaling_values, newdata = train_features)
test_features <- predict(scaling_values, newdata =  test_features)
# Improvement --> use another scaling method to see how changes results. method can be "center"/"scale"/"pca"/...


# Step 4: Run baseline model.
#############################

# Create variables to store best k and best accuracy.
best_k <- 0
best_acc <- 0
model_predictions <- ""
# Run loop to find best values.
for (i in 1:30){
  
  # Run model with different k.
  model <- knn(train = train_features,
               test = test_features,
               cl = train_labels,
               k = i) 
  
  # Calculate accuracy for each k by dividing correct predictions by total predictions.
  accuracy <- sum(model == test_labels) / length(test_labels)

  # Update values if the accuracy increased.
  if (accuracy > best_acc){
    best_k <- i
    best_acc <- accuracy
    model_predictions <- model
  }
}
print(paste0("Best K = ", best_k, "; Best accuracy = ", best_acc))

# Make confusion matrix comparing model predictions with test labels.
confusionMatrix(as.factor(model_predictions), as.factor(test_labels))


###########
### PCA ###
###########



# ### Feature selection importance.
# rocVarImp <- filterVarImp(features, factor(metadata$CancerSEEK.Test.Result))
# rocVarImp <- rocVarImp[order(rocVarImp$Negative, decreasing = TRUE),]
# 
# # Extract importance values
# feature_importance <- rocVarImp[,1]  
# names(feature_importance) <- rownames(rocVarImp)  # Assign feature names
# 
# # Bar plot
# par(mar = c(10, 4, 4, 2))  # Increase bottom margin for labels
# barplot(sort(feature_importance, decreasing = TRUE), 
#         las = 2, col = "steelblue", 
#         main = "Feature Importance (with caret ROC AUC)", 
#         xlab = "Features", ylab = "Importance Score",
#         margins = c(10, 10))
# 
# 
# ### Step 3: PCA
# # Use the principal component function to calculate the PC values.
# pca_results <- prcomp(features, scale. = FALSE, center = FALSE)
# 
# # Get the importance of each pc.
# pc_importace <- summary(pca_results)$importance
# # Make scree plot to visualize the proportion of variance from each component.
# plot(pc_importace[2,], type = "b", col = "darkblue", ylab = "Proportion of Variance", xlab = "PC components", main = "Scree plot")
# 
# # Get the loadings (the contribution of each feature to each pc).
# pc_loadings <- pca_results$rotation
# 
# # Examine the correlation between variables.
# correl_features <- cor(features)
# heatmap(correl_features, main = "Feature Correlation", col = colorRampPalette(c("blue", "white", "red"))(20), margins = c(10, 10))
# 
# 


### How to decide the centering and scaling method?
### How to decide how many PC to use? Scree plot shows very low importance of each individual pc.
### How to decide which features to remove using the correlation heatmap? Is >4 enought to consider correlated features?


