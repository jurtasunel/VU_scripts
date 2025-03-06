#########################################################
### COURSE: Bioinformatics for Translational Medicine ###
### COURSE CODE: X_405092                             ###
### PRACTICAL: Tumor type Predicion                   ###
### AUTHOR: Josemari Urtasun Elizari                  ###
### LAST MODIFIED: 06/03/2024                         ###
#########################################################


### Libraries.
import os
import pandas as pd

### Functions.

### Data.
os.chdir("/home/josemari/Desktop/MasterVU/2024/B4TM/")
train_call = pd.read_csv("Train_call.txt", sep = "\t")
train_clinical = pd.read_csv("Train_clinical.txt", sep = "\t")

### Data processing.
# Separate metadata from features, and transpose features to get them as columns.
metadata = train_call[train_call.columns[0:4]]
features = train_call.drop(columns = train_call.columns[0:4])
features = features.T

# Set the samples as index (rownames) of the labels dataset to join them to the features dataframe.
train_clinical = train_clinical.set_index(train_clinical.columns[0])
features = train_clinical.join(features, how = "right")
print(features)
 
# print(features.loc[["Array.10", "Array.101", "Array.102", "Array.104", "Array.105"]].iloc[:,:11])
# print(features.index.tolist())
# print(train_clinical[train_clinical.columns[0]])
