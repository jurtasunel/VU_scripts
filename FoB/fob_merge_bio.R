############################################################################################################
### This script gets the bio grades from two sessions of test vision and adds them to the fob grade book ###
############################################################################################################

setwd("/home/josemari/Desktop")

# Get paths to fob and fob grade books.
bio1_path = "/home/josemari/Desktop/2023 DX 04 P1 X_428565 Biology.csv"
bio2_path = "/home/josemari/Desktop/2023 DX 05 P1 X_405052_biology.csv"
fob_path = "/home/josemari/Desktop/2023-11-22T1112_Grades-X_405052.csv"

# Read csv files.
fob1 = read.csv(bio1_path, check.names = FALSE)
fob2 = read.csv(bio2_path, check.names = FALSE)
fob = read.csv(fob_path, check.names = FALSE)

# Get the grades from the two bio sessions on the same index.
index1  = na.omit(cbind(fob1[,5], fob1[,12])) # Omint NA.
index2 = na.omit(cbind(fob2[,5], fob2[,12])) # Omint NA.
index = rbind(index1, index2)

# Loop through the index.
for (i in 1:nrow(index)){
  
  ID = index[[i,1]]
  grade = index[[i, 2]]
  # Update the fob grade book with the math grade for every student.
  fob[fob[,4] == ID, 10] = grade
}

# Replace dot by comma on values so excel reads them as decimal.
for (i in 1:nrow(fob)){
  
  fob[i,10] = gsub("[.]", "'", fob[i,10])
  
}

# Write out the updated grade book.
write.csv(fob, file = "test.csv", row.names = FALSE)












 
 
 
 
 
 
 
