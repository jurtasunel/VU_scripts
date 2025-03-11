#########################################################
### COURSE: Bioinformatics for Translational Medicine ###
### COURSE CODE: X_405092                             ###
### PRACTICAL: Tumor type Predicion                   ###
### AUTHOR: Josemari Urtasun Elizari                  ###
### LAST MODIFIED: 10/03/2024                         ###
#########################################################

### Documentation:

### Libraries.
library(caret)

### Functions.



### Data.
setwd("/home/josemari/Desktop/MasterVU/2024/B4TM")
train_call <- read.delim("Train_call.txt")
train_clinical <- read.delim("Train_clinical.txt")

###################
### Processing. ###
###################

# Remove metadata and transpose the matrix to have features as columns.
features <- train_call[,5:ncol(train_call)]
features <- as.data.frame(t(as.matrix(features)))

# Add row names to the clinical labels to merge it with the features data. 
rownames(train_clinical) <- train_clinical$Sample
train_clinical$Sample <- NULL
combdata <- merge(train_clinical, features, by = "row.names")

# Clean the dataset.
rownames(combdata) <- combdata$Row.names
combdata$Row.names <- NULL



##########################
### Feature selection. ###
##########################

# Separate features and labels.
features <- combdata[,-1]
labels <- factor(combdata$Subgroup)

# Check feature importance for each label with caret function.
rocVarImp <- filterVarImp(features, labels)
# Make the average to get the overal importance of each feature.
averageImp <- apply(rocVarImp, 1, mean)
rocVarImp$average <- averageImp
# Sort in decreasing order to get the most important features.
rocVarImp <- rocVarImp[order(rocVarImp$average, decreasing = TRUE),]

# Keep features with importance => 0.7.
idx_rows <- which(rocVarImp$average >= 0.7) # Alternatively, just keep the top 20 features.
important_features <- rownames(rocVarImp)[idx_rows]
filter_data <- combdata[, c("Subgroup", important_features)]


############
### KNN. ###
############

knn_model <- train(x = filter_data[,2:ncol(filter_data)],
                   y = filter_data$Subgroup,
                   method = "knn",
                   tuneLength = 10,
                   trControl = trainControl(method = "CV", 10),
                   
                   )


print(knn_model)
# 
# ### Check loadings with different method.
# pca_results <- prcomp(features, scale. = TRUE, center = TRUE)
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
# #heatmap(correl_features, main = "Feature Correlation", col = colorRampPalette(c("blue", "white", "red"))(20), margins = c(10, 10))
# 
# 


