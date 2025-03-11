library(randomForest)
library(ggplot2)
library(pheatmap)

# Train the model
# Read the grouping information
design = read.csv("/path/to/your/design_file", header = T, row.names = 1)
# Convert the $Type column to a factor
design$Type = as.factor(design$Type)

# Read the domain matrix
otu_table = read.table("/path/to/your/matrix_file", header = T, row.names = 1)
otu_table = t(otu_table)

# Extract "group1" (training set)
design_sub = subset(design, Group %in% c("group1"))
summary(design_sub)

# Check whether the row names (bacterial IDs) of "design_sub" are in the domain matrix "otu_table"
idx = rownames(design_sub) %in% colnames(otu_table)
# Extract the dataset corresponding to Group1
design_sub = design_sub[idx,]
otu_sub = otu_table[, rownames(design_sub)]
summary(design_sub)

# Train and build the model
# set.seed(1001)
rf = randomForest(t(otu_sub), design_sub$Type, importance=TRUE, proximity=T, ntree = 1000)
print(rf)

# Save the model
save(rf, file = 'model1_group3_1.RData')

# Load the model directly
# load('Gram_model.RData')

# Cross-validation for feature selection
# set.seed(827) # Ensuring repeatability of results with random data is a must
# rfcv is the Random Forest Cross Validation function
result = rfcv(t(otu_sub), design_sub$Type, cv.fold=5)
save(result, file = 'result_rfcv1_group3_1.RData')

# Check the error rate table, where the lowest error rate occurs at 68, indicating the best model
result$error.cv
error_data <- as.data.frame(result$error.cv)
write.table(error_data, file = 'error1_group3_1.txt', sep = '\t', row.names = T,
            quote = F, col.names = NA)

# Plot validation results
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2)) 
# It is recommended to perform cross-validation 5-6 times and combine the results into one figure

# Export training set observations and prediction results
train.p = predict(rf, type = "response")
df = data.frame(observed = design_sub$Type, predict = train.p)  
# Extract a subset of data for prediction
# Save the prediction results and compare them with actual results
write.table(df, file = "train_predict1_group3_1.txt", quote = F, sep = '\t', row.names = T, col.names = T)

# Export feature importance
imp = as.data.frame(rf$importance)
imp = imp[order(imp[,1], decreasing = T),]
head(imp, n=10)
write.table(imp, file = "importance_class1_group3_1.txt", quote = F, sep = '\t', row.names = T, col.names = T)

# Simple visualization
varImpPlot(rf, main = "Top 10 - Feature importance", n.var = 10, bg = par("bg"), color = par("fg"), gcolor = par("fg"), lcolor = "gray")

# Beautify feature importance visualization using ggplot2
# Read all feature importance values
imp = read.table("importance_class1_group3_1.txt", header=T, row.names= 1, sep="\t") 
# Analysis shows that selecting the top 23 groups gives the best performance
imp = head(imp, n=23)

# Reverse order of the X-axis to make the bar chart plot from top to bottom
imp = imp[order(1:23, decreasing = T),]

# Sort imp by the third column in ascending order
imp = imp[order(imp[,3]),]

# Extract column names
imp$Domain = gsub("", "", rownames(imp), perl=TRUE)

imp$Domain = factor(imp$Domain, levels = imp$Domain)

# Figure 1. Feature importance bar chart
library(ggplot2)
p = ggplot(data = imp, mapping = aes(x=Domain, y=MeanDecreaseAccuracy, fill=Domain)) + 
  geom_bar(stat="identity") + coord_flip() + theme_bw()
ggsave(p, filename = "imp_shape1.pdf", width = 16, height = 9)

# Group2 validation
# design = read.table("group_Gr_all.txt", header = T, row.names = 1)
design_test = subset(design, Group %in% c("group2")) 
summary(design_test)

idx = rownames(design_test) %in% colnames(otu_table)
design_test = design_test[idx,]
otu_sub = otu_table[, rownames(design_test)]
summary(design_test)

# Transpose and add grouping information
otutab_t = as.data.frame(t(otu_sub))

# Add the grouping information of Group2 to the domain matrix
# This means adding the "Group" column content from the "design" matrix to the "otutab_t" matrix based on row names, and setting the column name to "Group"
otutab_t$Type = design[rownames(otutab_t),]$Type

# set.seed(13)
otutab.pred = predict(rf, t(otu_sub))  
pre_tab = table(observed=otutab_t[,"Type"], predicted=otutab.pred) 
pre_tab

# Organize original sample grouping and predicted classification
predict = data.frame(Type = otutab_t[,"Type"], predicted=otutab.pred)

# Save the prediction result table
write.table("SampleID\t", file=paste("RF_prediction_binary.txt", sep=""), append = F, quote = F, eol = "", row.names = F, col.names = F)
write.table(predict, file = "RF_prediction_binary.txt", append = T, quote = F, row.names = T, col.names = T, sep = "\t")
