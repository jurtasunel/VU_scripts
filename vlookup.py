############################################################################################################
### This script copies column values from one csv to another using a shared column as index template     ###
### The template column must be present in both files AND have the same index (same as vlookup function) ###
############################################################################################################

### Libraries.
import sys
import pandas as pd

### Argument variables.
file_1 = sys.argv[1] # CSV file with data to transfer.
file_2 = sys.argv[2] # CSV destination file.
data_col = int(sys.argv[3]) # Column index of data column in file 1.
dest_col = int(sys.argv[4]) # Destination column index in file 2.
template_col = int(sys.argv[5]) # Shared column index of template (student vuID).

# Read files as pandas data frame.
df_1 = pd.read_csv(file_1)
df_2 = pd.read_csv(file_2)

# Get template column from dataframe 2 into a list.
df_2_tempvals = df_2.iloc[:,template_col].tolist()
# Filter values to only keep vuIDs strings (NaN are saved as float).
df_2_tempvals = [i for i in df_2_tempvals if type(i) == str]

# Loop through the rows on dataframe 1.
for row in range(len(df_1)):
    
    # Get the vuID.
    vuID = df_1.iloc[row, template_col]
    
    # If the ID exist on dataframe 2:
    if vuID in df_2_tempvals:

        # Get the corresponding value from file 1.
        dataval = float(df_1.iloc[row, data_col])
        # Add the value to the destination column in file 2.
        df_2.loc[df_2.iloc[:, template_col] == vuID, df_2.columns[dest_col]] = dataval  

# Write out the updated file 2.
df_2.to_csv(f"completed_{file_2}", sep = ",")





