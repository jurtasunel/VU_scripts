#############################################################################################################
### This script gets the math grades from ISB grade book and transfers the average to the FoB grade book ###
#############################################################################################################

setwd("/home/josemari/Desktop")

# Get paths to isb and fob grade books.
isb_path = "/home/josemari/Desktop/2023-11-22T0605_Grades-X_428565.csv"
fob_path = "/home/josemari/Desktop/2023-11-22T0605_Grades-X_405052.csv"
# Read csv files.
isb = read.csv(isb_path, check.names = FALSE)
fob = read.csv(fob_path, check.names = FALSE)

# Get column index of mathsI and mathII on the isb grade book.
mathI_col = 9
mathII_col = 10
# Get destination column on fob grade book.
fob_destcol = 7

# Make column to store final maths grade.
maths_final = c()

# Loop through the isb grade book
for (i in 1:nrow(isb)){

  # Get mathI grade. Convert all NA values to 0, and all numerical values to numeric.
  if (is.na(isb[i,mathI_col]) || isb[i,mathI_col] == "" || isb[i,mathI_col] == " " || isb[i,mathI_col] == "N/A"){
    mathI = 0} else{mathI = as.numeric(isb[i,mathI_col])}

  # Get mathII grade. Convert all NA values to 0, and all numerical values to numeric.
  if (is.na(isb[i,mathII_col]) || isb[i,mathII_col] == "" || isb[i,mathII_col] == " " || isb[i,mathII_col] == "N/A"){
    mathII = 0} else{mathII = as.numeric(isb[i,mathII_col])}
  
  # Append the average to the final grade vector.
  maths_avr = (mathI + mathII) / 2
  maths_final = c(maths_final, maths_avr)
}

# Make index for vlookup on fob grade book.
index = cbind(asa[,4], maths_final)

# Loop through the index.
for (i in 1:nrow(index)){
  
  ID = index[[i,1]]
  grade = index[[i, 2]]
  # Update the fob grade book with the math grade for every student.
  fob[fob[,4] == ID, fob_destcol] = grade
}

# Replace dot by comma on values so excel reads them as decimal.
for (i in 1:nrow(fob)){
  
  fob[i,fob_destcol] = gsub("[.]", "'", fob[i,fob_destcol])
}

# Write out the updated grade book.
write.csv(fob, file = "test.csv", row.names = FALSE)














 
 
 
 
 
 
 
