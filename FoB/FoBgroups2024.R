###################################################################################################
### This script reads in a canvas grades file and makes groups according to students background ###
###################################################################################################

### The sheet is from Canvas>Grades>Export gradebook
### Each year the IDs for the grades columns change, so the script needs update on lines 27, 36 and 43.

setwd("C:/Users/zir826/Downloads")
# Read input file as csv.
GRADEScsv = "2025-09-01T1705_Grades-X_405052.csv"
grades <- as.data.frame(read.csv(GRADEScsv, stringsAsFactors = FALSE))
n_by_group = 4

# Count number of groups of 4 people to make.
n_groups <- nrow(grades)/n_by_group
# If division is not exact, add one extra group.
if (nrow(grades)%%n_by_group != 0) {n_groups <- n_groups + 1}
# Make groups list.
groups_list <- list()
for (i in 1:n_groups){
  group_name <- paste0("group_", i)
  groups_list[[group_name]] <- vector(mode = "character")
}

# Remove first two rows that contain grading scheme and sort the grades descending for programming.
grades <- grades[-c(1,2, nrow(grades)),]
grades <- grades[order(grades$Programming.intake..414920., decreasing = TRUE),]
# Add one student with programming background to each group.
for(i in 1:length(groups_list)){
  groups_list[[i]] <- grades[i,1] # The one is for the first column, which has the student name.
}
# Remove those students from the grades data frame.
grades <- grades[-c(1:n_groups),]

# Repeat with biology background; sort now according to Bio grade, add one to each group and remove those from the df.
grades <- grades[order(grades$Biology.intake..414899., decreasing = TRUE),]
for(i in 1:length(groups_list)){
  groups_list[[i]] <- c(groups_list[[i]], grades[i,1]) # The one is for the first column, which has the student name.
}
grades <- grades[-c(1:n_groups),]

# Repeat with maths background.
grades <- grades[order(grades$Math.intake..414913., decreasing = TRUE),]
for(i in 1:length(groups_list)){
  groups_list[[i]] <- c(groups_list[[i]], grades[i,1]) # The one is for the first column, which has the student name.
}
grades <- grades[-c(1:n_groups),]

# Add the remaining students to groups one by one.
for (i in 1:nrow(grades)){
  groups_list[[i]] <- c(groups_list[[i]], grades$Student[i])
  
}  

# Check how many gruops are missing a student, and add empty string to them.
for (i in (nrow(grades) + 1):length(groups_list)){
  groups_list[[i]] <- c(groups_list[[i]], "NA")
}

# Format the groups to write out a csv.
Group <- names(groups_list)
Student_1 <- c()
Student_2 <- c()
Student_3 <- c()
Student_4 <- c()
for (i in Group){
  Student_1 <- c(Student_1, groups_list[[i]][1])
  Student_2 <- c(Student_2, groups_list[[i]][2])
  Student_3 <- c(Student_3, groups_list[[i]][3])
  Student_4 <- c(Student_4, groups_list[[i]][4])
}
final_groups <- as.data.frame(cbind(Group, Student_1, Student_2, Student_3, Student_4))
write.csv(final_groups, "FoB_Groups_2025.csv", row.names = FALSE)



