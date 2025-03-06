#################################################
### COURSE: Genes, Bioinformatics and Disease ###
### COURSE CODE: 900255SCIY                   ###
### PRACTICAL: Tumor Detection                ###
### AUTHOR: Josemari Urtasun Elizari          ###
### LAST MODIFIED: 05/03/2025                 ###
#################################################

### Documentation (1-plot in r, 2-PCA, 3-KNN with class. 4-KNN with caret, class, etc.):
#https://r-coder.com/plot-r/
#https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/
#https://www.geeksforgeeks.org/k-nn-classifier-in-r-programming/
#https://www.datacamp.com/tutorial/k-nearest-neighbors-knn-classification-with-r-tutorial


### Libraries.
library(caret)

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

# The two functions above can be done simply with base R: scale(x, center = TRUE, scale = TRUE).
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
# Funtion to scale the data to z-scores (dividing by the standard deviation on each column).
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

### Data.
setwd("C:/Users/zir826/Desktop/2024-25/AUC/Practicals/TumorDetection")
inputfile <- as.data.frame(read.csv("Table_S6.csv"))

### Step 1: Process data.
# Make new column with binary classification for the result label.
label <- c()
for (i in 1:nrow(inputfile)){
  if (inputfile$CancerSEEK.Test.Result[i] == "Positive"){
    label <- c(label, 1)
  } else {label <- c(label, 0)
}}
inputfile <- cbind(inputfile, label)

# Remove all rows with NA values.
inputfile <- inputfile[complete.cases(inputfile),]
rownames(inputfile) <- paste0("sample", 1:nrow(inputfile))

# Separate train and test.
splits <- splitdata(inputfile, 0.8)
train <- splits$trainset
test <- splits$testset

### Step 2: Center and scaling.
# Get the features and labels separated.
features <- train[,c(5:43)]
metadata <- train[,c(1:4, 44:46)]
labels <- metadata$label 


# Center the data by subtracting the mean on each feature column (so the means of features become 0).
features <- center(features)
# Scale the data to z-scores by dividing by the standard deviation on each column (points above the mean are + and below are -).
features <- zscore(features)


### Feature selection importance.
rocVarImp <- filterVarImp(features, factor(metadata$CancerSEEK.Test.Result))
rocVarImp <- rocVarImp[order(rocVarImp$Negative, decreasing = TRUE),]

# Extract importance values
feature_importance <- rocVarImp[,1]  
names(feature_importance) <- rownames(rocVarImp)  # Assign feature names

# Bar plot
par(mar = c(10, 4, 4, 2))  # Increase bottom margin for labels
barplot(sort(feature_importance, decreasing = TRUE), 
        las = 2, col = "steelblue", 
        main = "Feature Importance (with caret ROC AUC)", 
        xlab = "Features", ylab = "Importance Score",
        margins = c(10, 10))


### Step 3: PCA
# Use the principal component function to calculate the PC values.
pca_results <- prcomp(features, scale. = FALSE, center = FALSE)

# Get the importance of each pc.
pc_importace <- summary(pca_results)$importance
# Make scree plot to visualize the proportion of variance from each component.
plot(pc_importace[2,], type = "b", col = "darkblue", ylab = "Proportion of Variance", xlab = "PC components", main = "Scree plot")

# Get the loadings (the contribution of each feature to each pc).
pc_loadings <- pca_results$rotation

# Examine the correlation between variables.
correl_features <- cor(features)
heatmap(correl_features, main = "Feature Correlation", col = colorRampPalette(c("blue", "white", "red"))(20), margins = c(10, 10))




### How to decide the centering and scaling method?
### How to decide how many PC to use? Scree plot shows very low importance of each individual pc.
### How to decide which features to remove using the correlation heatmap? Is >4 enought to consider correlated features?








