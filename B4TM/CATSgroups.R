#########################################################################################################################
### This script reads in a the 'B4TM CATS group search' google form and makes groups according to students background ###
#########################################################################################################################

# Read input file as csv.
CATScsv = "C:/Users/zir826/Downloads/B4TM CATS group search 2024 (Responses) - Form responses 1.csv"
answers <- as.data.frame(read.csv(CATScsv, stringsAsFactors = FALSE))
n_by_group = 4

# Count number of groups of 4 people to make.
n_groups <- nrow(answers)/n_by_group
# If division is not exact, add one extra group.
if (nrow(answers)%%n_by_group != 0) {n_groups <- n_groups + 1}
# Make groups list.
groups_list <- list()
for (i in 1:n_groups){
  group_name <- paste0("group_", i)
  groups_list[[group_name]] <- vector(mode = "character")
}

# Separate students with and without ML background.
MLyes <- answers[answers$ML.exp == "Yes",]
MLno <- answers[answers$ML.exp == "No",]

# Add one student with ML backgroupd to each group.
for(i in 1:length(groups_list)){
  groups_list[[i]] <- MLyes[i,]
}
# Remove those students from the MLyes data frame.
MLyes <- MLyes[-c(1:n_groups),]

# Find the shortest of the two data frames.
shorter_length <- min(nrow(MLyes), nrow(MLno))
# Alternate rows and bind them together one by one.
shuffled_df <- data.frame()
for(i in 1:shorter_length){
  shuffled_df <- rbind(shuffled_df, MLyes[i,], MLno[i,])
}
# Add all the students left on the larger group.
if(nrow(MLyes) > nrow(MLno)){
  shuffled_df <- rbind(shuffled_df, MLyes[(shorter_length+1):nrow(MLyes),])
} else{
  shuffled_df <- rbind(shuffled_df, MLno[(shorter_length+1):nrow(MLno),])
}

# Make counter to update current group.
i_group = 1
# Add the students to the groups in steps of 3.
for(i in seq(1,nrow(shuffled_df), 3)){
  groups_list[[i_group]] <- rbind(groups_list[[i_group]],
                                  shuffled_df[i,],
                                  shuffled_df[i+1,],
                                  shuffled_df[i+2,])
  i_group <- i_group+1
}

# Prepare vectors for final dataframe to print out results.
name <- c()
number <- c()
group <- c()
MLexp <- c()
BIOexp <- c()
comments <- c()

# Loop through the names of each group.
for(group_name in names(groups_list)){
  
  # Get each group df.
  i_group <- groups_list[[group_name]]
  # Loop through each student and fill results vectors.
  for (i in 1:nrow(i_group)){
    
    group <- c(group, group_name)
    name <- c(name, i_group$Name[i])
    number <- c(number, i_group$Student.number[i])
    MLexp <- c(MLexp, i_group$ML.exp[i])
    BIOexp <- c(BIOexp, i_group$Biomedical.exp[i])
    comments <- c(comments, i_group$Comments[i])
  }
    
}
final_groups <- as.data.frame(cbind(name, number, group, MLexp, BIOexp, comments))

# Write out final dataframe.
final_groups <- final_groups[,1:3]
write.csv(final_groups, "B4TM_CATSgroups_2025.csv")

