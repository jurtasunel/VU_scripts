################################################################################################################################
### This script gets the FOB grade book and produces a csv with the final grades of the conversion classes, exam and project ###
################################################################################################################################

setwd("C:/Users/zir826/Desktop")

# Get paths to fob and fob grade book.
fob_path = "C:/Users/zir826/Desktop/2023-11-27T1445_Grades-X_405052.csv"
fob = read.csv(fob_path, check.names = FALSE)

# Get indices of grade columns.
math_col = 7
prog1_col = 8
prog2_col = 9
bio1_col = 10
bio2_col = 11
final_col = 12
exam_col = 13
project_col = 21

# Initialize vectors to store the numeric grade of each student. First two are 0 for header position
maths <- c(0,0)
bio <- c(0,0)
prog <- c(0,0)
exam <- c(0,0)
project <- c(0,0)

# Loop through the rows starting at 3 (first two rows are headers).
for (i in 3:nrow(fob)){
  maths <- c(maths, as.numeric(fob[i,math_col]))
  bio <- c(bio, (as.numeric(fob[i,bio1_col]) + as.numeric(fob[i,bio2_col]))/2)
  prog <- c(prog, (as.numeric(fob[i,prog1_col]) + as.numeric(fob[i,prog2_col]))/2)
  exam <- c(exam, as.numeric(fob[i,exam_col]))
  project <- c(project, as.numeric(fob[i,project_col]))
}

# Make index data frame with all grades.
index = data.frame(cbind(maths, bio, prog, exam, project))
index = cbind(fob[,4], index)

# Make vector to calculate the final conversion grade.
conv_grade <- c(0,0)
final_grade <- c(0,0)
# Loop through the index rows.
for (i in 3:nrow(index)){
  
  # Get the conversion classes grades.
  conv <- unlist(index[i, c("maths", "bio", "prog")])

  # If student has done programming (prog != NA), get programming grade.
  if (!is.na(conv["prog"])){
    conv_grade <- c(conv_grade, conv["prog"])
  }
  
  # If student has done biology (prog == NA and bio != NA), get bio grade.
  else if(!is.na(conv["bio"])){
    conv_grade <- c(conv_grade, conv["bio"])
  }
  
  # If student hasn't done prog or bio, take maths grade.
  else{conv_grade <- c(conv_grade, conv["maths"])}
  
  # Calculate final grade.
  final_grade <- c(final_grade, ((0.4*unlist(index[i, c("exam")])) + (0.3*unlist(index[i, c("project")])) + (0.3*conv_grade[i])))

}

# Write out csv with final grades.
index = cbind(index, conv_grade, final_grade)
write.csv(index, file = "fob_FinalGrades.csv", row.names = FALSE)
