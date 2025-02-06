#################################################
### COURSE: Genes, Bioinformatics and Disease ###
### COURSE CODE: 900255SCIY                   ###
### PRACTICAL: Tumor Detection                ###
### AUTHOR: Josemari Urtasun Elizari          ###
### LAST MODIFIED: 03/05/2024                 ###
#################################################

### Documentation (1-KNN with class. 2-KNN with caret, class, etc. 3-PCA):
#https://www.geeksforgeeks.org/k-nn-classifier-in-r-programming/
#https://www.datacamp.com/tutorial/k-nearest-neighbors-knn-classification-with-r-tutorial
#https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/


### Import libraries.
library(class) # for ML classifier functions.
library(caTools) # for simple train-test set split.
library(ggbiplot) # PCA plot.

# Read input file.
setwd("C:/Users/zir826/Desktop/AUC/TumorExpression")
inputfile = as.data.frame(read.csv("Table_S6.csv", stringsAsFactors = FALSE))


################
### ROC plot ###
################
# Sort file by descending score.
inputfile_srt = inputfile[order(inputfile$CancerSEEK.Logistic.Regression.Score, decreasing = TRUE),]
# Make empty vectors to store TPR and FPR.
TPR <- c()
FPR <- c()
# Loop through the rows.
for (i in 1:nrow(inputfile_srt)){
  
  # Set each i score as a threshold.
  threshold <- inputfile_srt$CancerSEEK.Logistic.Regression.Score[i]
  # Calculate TP, FP, TN and FN.
  TP <- sum(inputfile_srt$Tumor.type != "Normal" & inputfile_srt$CancerSEEK.Logistic.Regression.Score >= threshold)
  FP <- sum(inputfile_srt$Tumor.type == "Normal" & inputfile_srt$CancerSEEK.Logistic.Regression.Score >= threshold)
  TN <- sum(inputfile_srt$Tumor.type == "Normal" & inputfile_srt$CancerSEEK.Logistic.Regression.Score < threshold)
  FN <- sum(inputfile_srt$Tumor.type != "Normal" & inputfile_srt$CancerSEEK.Logistic.Regression.Score < threshold)
  
  # Append TPR and FPR.
  tpr <- TP / (TP + FN)
  fpr <- FP / (FP + TN)
  TPR <- c(TPR, tpr)
  FPR <- c(FPR, fpr)
}

# Plot the ROC plot.
plot(FPR, TPR, type = "b", col = "darkblue", main = "ROC plot")
abline(coef = c(TPR[1],1-TPR[1])) # Diagonal with intercept at lower left corner and slope of ratio of increase between two axis.


###########
### KNN ###
###########

# Split data in train and test sets using split from caTools.
split <- sample.split(inputfile, SplitRatio = 0.8)
train_cl <- subset(inputfile, split == TRUE)
test_cl <- subset(inputfile, split == FALSE)

# Scale dataset with scale (center means a substract the mean, scale means divide by stDev for each feature column).
train_scale <- scale(train_cl[,5:43], center = TRUE, scale = TRUE)
test_scale <- scale(test_cl[,5:43], center = TRUE, scale = TRUE)

# Use KNN model with k of 1.
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$CancerSEEK.Test.Result,
                      k = 1)

# Confusion matrix to asses how many predictions are correct.
cm <- table(test_cl$CancerSEEK.Test.Result, classifier_knn)
# Check accuracy by doing the mean of incorrect predictions and substracting it from 1. If all correct, mean is 0 and acc = 1.
acc <- 1 - mean(classifier_knn != test_cl$CancerSEEK.Test.Result)

# Try different values of k and evaluate accuracy.
# Create variables to store best k and accuracy.
best_k = 0
best_acc = 0
# Loop through values of k.
for (i in 1:50){
    classifier_knn <- knn(train = train_scale,
                        test = test_scale,
                        cl = train_cl$CancerSEEK.Test.Result,
                        k = i)
    acc <- 1 - mean(classifier_knn != test_cl$CancerSEEK.Test.Result)
    
    # Update k and accuracy if they improve.
    if (acc > best_acc){
      best_acc <- acc
      best_k <- i
    }
}
print(paste0("Best K = ", best_k, "; accuracy ", best_acc))


###########
### PCA ###
###########

# Get the numeric variables for PCA, and colour it based on the label. 
pca_results <- prcomp(inputfile[,5:43], center = TRUE, scale = TRUE)

### A: Plot PCA with base R plotting.
pca_color <- c("red", "blue")[(inputfile$Tumor.type == "Normal") + 1]
plot(pca_results$x[,1], pca_results$x[,2], col = pca_color, xlab = "PC1", ylab = "PC2")
legend("topleft", c("Tumor", "Healthy"), fill = c("red", "blue"))

### B: Plot with ggbiplot.
# Add new column with only Healthy and Tumor groups.
pca_color <- ifelse(inputfile$Tumor.type == "Normal", "Healthy", "Tumor")
pca_color <- cbind(inputfile, pca_color)
# Make custom color palette.
group_color <- c(c("Healthy" = "lightblue", "Tumor" = "darkred"))

g <- ggbiplot(pca_results,
              obs.scale = 1,
              var.scale = 1,
              groups = pca_color$pca_color,
              var.axes = 0) +
  scale_color_manual(values = group_color) +
  theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

