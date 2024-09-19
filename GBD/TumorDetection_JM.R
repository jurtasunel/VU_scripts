##############################################
### This script produces a simple ROC plot ###
##############################################

# Define Ground truth and predictions.
GroundTruth <- c("Diseased", "Healthy", "Healthy", "Healthy", "Diseased", "Diseased", "Diseased", "Healthy", "Diseased", "Diseased")
Prediction <- c(0.18, 0.21, 0.41, 0.42, 0.45, 0.57, 0.58, 0.73, 0.82, 0.93)
dataDF <- data.frame(cbind(GroundTruth, Prediction))

# Sort file by descending score.
dataDF = dataDF[order(dataDF$Prediction, decreasing = TRUE),]
# Initialize vectors to store TPR and FPR at coordinates 0,0.
TPR <- c(0)
FPR <- c(0)
# Loop through the rows.
for (i in 1:nrow(dataDF)){
  
  # Set each i score as a threshold.
  threshold <- dataDF$Prediction[i]
  # Calculate TP, FP, TN and FN.
  TP <- sum(dataDF$GroundTruth != "Healthy" & dataDF$Prediction >= threshold)
  FP <- sum(dataDF$GroundTruth == "Healthy" & dataDF$Prediction >= threshold)
  TN <- sum(dataDF$GroundTruth == "Healthy" & dataDF$Prediction < threshold)
  FN <- sum(dataDF$GroundTruth != "Healthy" & dataDF$Prediction < threshold)
  
  # Append TPR and FPR.
  tpr <- TP / (TP + FN)
  fpr <- FP / (FP + TN)
  TPR <- c(TPR, tpr)
  FPR <- c(FPR, fpr)
}

# Plot the ROC plot.
plot(FPR, TPR, type = "b", col = "darkblue", main = "ROC plot")
abline(coef = c(TPR[1],1-TPR[1])) # Diagonal with intercept at lower left corner and slope of ratio of increase between two axis.

