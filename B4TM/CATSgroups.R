############################################################################################
### This script reads in a google form and makes groups according to students background ###
############################################################################################

# Read input file as csv.
CATScsv = "C:/Users/zir826/Downloads/B4TMCATS.csv"
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

# Prepare final dataframe to print out results.
name <- c()
number <- c()
group <- c()
MLexp <- c()
BIOexp <- c()
comments <- c()

for(group in groups_list){

    
  }
}




