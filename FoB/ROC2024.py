# Import library for plotting.
import matplotlib.pyplot as plt

# Make a list of lists with predictions and labels.
data = []
ground_truth = ["Benign", "Benign", "Pathogenic", "Pathogenic", "Benign", "Pathogenic"]
impact_prediction = [0.1, 0.4, 0.35, 0.8, 0.9, 0.7] 
for i in range(len(ground_truth)):
    data.append([impact_prediction[i], ground_truth[i]])

# Sort the list by the impact prediction in descending order.
data.sort(reverse=True, key=lambda x: x[0])
print(data)
# Initialize variables to store tpr and fpr coordinates.
tpr_values, fpr_values = [0], [0]

# Count the total number of Pathogenic and Benign cases.
total_P = ground_truth.count("Pathogenic")
total_N = len(ground_truth) - total_P 

# Initialize variables to store True Positives and False Positives for each threshold.
TP, FP = 0, 0

# Calculate TPR and FPR for each threshold.
for pair in data:

    if pair[1] == "Pathogenic":
        TP += 1
    else:
        FP += 1

    TPR = TP / total_P  
    FPR = FP / total_N  

    tpr_values.append(TPR)
    fpr_values.append(FPR)

# Plot ROC curve.
plt.plot(fpr_values, tpr_values, marker = "o")
plt.xlabel('FPR')
plt.ylabel('TPR')
plt.show()
