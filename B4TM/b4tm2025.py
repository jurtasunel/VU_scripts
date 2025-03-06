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
os.chdir("/mnt/c/Users/zir826/Desktop/2024-25/B4TM/Practicals/")
train_call = pd.read_csv("Train_call.txt", sep = "\t")
train_clinical = pd.read_csv("Train_clinical.txt", sep = "\t")

print(train_call)
print(train_clinical)