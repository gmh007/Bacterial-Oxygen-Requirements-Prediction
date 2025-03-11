# Load necessary packages
library(e1071)

# Read the matrix
raw_data <- read.csv('pfam_transposed.txt', header = T, sep = '\t', row.names = 1, check.names = F)

# Check the number of columns
ncol(raw_data)
# Check the number of rows
nrow(raw_data)

# Read the experimental design table
design <- read.csv('1814_design3v1_random_extract_raw.csv', header = T, sep = ',', row.names = 1)
head(design)

# Split the data using group1 as the training set and group2 as the test set

# Set an index to check for missing, extra, or duplicate samples
# design_group1 selects rows from data.txt where the Group column value is 'group1', extracting training data
design_group1 <- subset(design, Group %in% c('group1')) 

# Construct the training set
# design_group1 is the index of the training data, raw_data contains rows labeled P001 and columns as domain data
index_group1 <- rownames(design_group1) %in% rownames(raw_data)

# Count TRUE and FALSE in the logical vector "index_group1" and return as a table
# This function is used to check whether all rows in "design_group1" exist in "data.txt".
# If any row is missing, further processing or correction is required.
table(index_group1)

# Check how many samples are in group1
# nrow(design_group1)
# Extract training data from raw_data using row names from design_group1
train_data <- raw_data[rownames(design_group1),]

# Check the dimensions and row names of the training set
# dim(train_data)
class(train_data)

# Convert training set data format
train_data[,1:10351] <- as.data.frame(lapply(train_data[,1:10351], as.numeric))

# Convert oxygen type to factor
# Convert categorical variables to factor type to facilitate grouping, statistics, and modeling
design_group1$Type <- factor(design_group1$Type)

# Construct the test set
design_group2 <- subset(design, Group %in% c('group2'))
test_data <- raw_data[rownames(design_group2),]
# dim(test_data)
test_data[,1:10351] <- as.data.frame(lapply(test_data[,1:10351], as.numeric))
# The last column in design.txt is Type, so it is not visible in data.txt
test_data$Type <- factor(design_group2$Type)

############################## Training and test sets are ready ###################################

library(e1071)
print("--------------svm start--------------")
# Support Vector Machine (SVM)
svm_model <- svm(design_group1$Type ~ ., train_data)
train_result <- predict(svm_model, train_data)
# View training set prediction results
table(true=design_group1$Type, predict=train_result)
conf_matrix <- table(true=design_group1$Type, predict=train_result)
svm_train_result <- sum(diag(conf_matrix)) / sum(conf_matrix)
svm_train_result <- paste(round(svm_train_result * 100, 2), "%", sep = '')
write.table(svm_train_result, file="accuracy_svm_train.txt")
write.table(conf_matrix, file="svm_train_result.txt", quote = F, sep = '\t', row.names = T, col.names = T)

# View test set prediction results
print("-----Test Set Data-----")
test_result <- predict(svm_model, test_data)
table(true=design_group2$Type, predict=test_result)
conf_matrix <- table(true=design_group2$Type, predict=test_result)
svm_test_result <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy_svm_test = paste(round(svm_test_result * 100, 2), "%", sep = '')
write.table(accuracy_svm_test, file="accuracy_svm_test.txt")
write.table(conf_matrix, file="svm_test_result.txt", quote = F, sep = '\t', row.names = T, col.names = T)
print("--------------svm end--------------")

print("--------------naiveBayes start--------------")

# Naive Bayes Classification
nb_model <- naiveBayes(design_group1$Type ~ ., train_data)
train_result <- predict(nb_model, train_data, type='class')
table(true=design_group1$Type, predict=train_result)
conf_matrix <- table(true=design_group1$Type, predict=train_result)
nb_train_result <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy_naiveBayes_train = paste(round(nb_train_result * 100, 2), "%", sep = '')
write.table(accuracy_naiveBayes_train, file="accuracy_naiveBayes_train.txt")
write.table(conf_matrix, file="nb_train_result.txt", quote = F, sep = '\t', row.names = T, col.names = T)

print("-----Test Set Data-----")
test_result <- predict(nb_model, test_data, type='class')
table(true=design_group2$Type, predict=test_result)
conf_matrix <- table(true=design_group2$Type, predict=test_result)
nb_test_result <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy_naiveBayes_test = paste(round(nb_test_result * 100, 2), "%", sep = '')
write.table(accuracy_naiveBayes_test, file="accuracy_naiveBayes_test.txt")
write.table(conf_matrix, file="nb_train_test.txt", quote = F, sep = '\t', row.names = T, col.names = T)
print("--------------naiveBayes end--------------")

print("--------------part Conditional Inference Tree start--------------")

# Conditional Inference Tree Algorithm
library(party)
ctree_model <- ctree(design_group1$Type ~ ., train_data)
train_result <- predict(ctree_model, train_data)
table(true=design_group1$Type, predict=train_result)
conf_matrix <- table(true=design_group1$Type, predict=train_result)
ctree_train_result <- sum(diag(conf_matrix)) / sum(conf_matrix)

print("-----Test Set Data-----")
accuracy_ctree_train = paste(round(ctree_train_result * 100, 2), "%", sep = '')
write.table(accuracy_ctree_train, file="accuracy_ctree_train.txt")
write.table(conf_matrix, file="ctree_train_result.txt", quote = F, sep = '\t', row.names = T, col.names = T)
test_result <- predict(ctree_model, test_data)
table(true=design_group2$Type, predict=test_result)
conf_matrix <- table(true=design_group2$Type, predict=test_result)
ctree_test_result <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy_ctree_test = paste(round(ctree_test_result * 100, 2), "%", sep = '')
write.table(accuracy_ctree_test, file="accuracy_ctree_test.txt")
write.table(conf_matrix, file="ctree_test_result.txt", quote = F, sep = '\t', row.names = T, col.names = T)
print("--------------part Conditional Inference Tree end--------------")
#De
print("--------------part Decision Tree start--------------")
library(rpart)
dtree_model <- rpart(design_group1$Type~.,train_data,method = 'class')
dtree_model$cptable
# rpart.plot::rpart.plot(dtree_model)
dtree_model <- prune(dtree_model,cp=0.01)
train_result <- predict(dtree_model,train_data,type='class')
table(true=design_group1$Type,predict=train_result)
conf_matrix <- table(true=design_group1$Type,predict=train_result)
dtree_train_result <-sum(diag(conf_matrix))/sum(conf_matrix)

accuracy_rpart_train = paste(round(dtree_train_result*100,2),"%",sep = '')
write.table(accuracy_rpart_train, file="accuracy_dtree_train.txt")
write.table(conf_matrix,file="dtree_train_result.txt",quote = F,sep = '\t', row.names = T, col.names = T)

print("-----Test Set Data-----")
test_result <- predict(dtree_model,test_data,type='class')
table(true=design_group2$Type,predict=test_result)
conf_matrix <- table(true=design_group2$Type,predict=test_result)
dtree_test_result <-sum(diag(conf_matrix))/sum(conf_matrix)
accuracy_rpart_test = paste(round(dtree_test_result*100,2),"%",sep = '')
write.table(accuracy_rpart_test, file="accuracy_dtree_test.txt")
write.table(conf_matrix,file="dtree_test_result.txt",quote = F,sep = '\t', row.names = T, col.names = T)
print("--------------part Decision Tree end--------------")


print("--------------GBDT start--------------")

library(xgboost)

design_group1$Type <- as.factor(design_group1$Type)
design_group2$Type <- as.factor(design_group2$Type)

# Convert training and test sets to matrix formattrain_matrix <- as.matrix(train_data[,1:10351])
train_matrix <- as.matrix(train_data[,1:10351])
test_matrix <- as.matrix(test_data[,1:10351])
train_label <- as.numeric(design_group1$Type) - 1  # xgboost 需要数值型标签
test_label <- as.numeric(design_group2$Type) - 1

# Create xgboost datasets
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Train the GBDT model
params <- list(
  objective = "multi:softmax",
  num_class = length(levels(design_group1$Type)),
  eval_metric = "merror",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)
gbdt_model <- xgboost(params = params, data = dtrain, nrounds = 50, verbose = 0)

# Prediction on the training set
train_result <- predict(gbdt_model, dtrain)
conf_matrix <- table(true = train_label, predict = train_result)
gbdt_train_result <- sum(diag(conf_matrix)) / sum(conf_matrix)

accuracy_gbdt_train = paste(round(gbdt_train_result * 100, 2), "%", sep = '')
write.table(accuracy_gbdt_train, file = "accuracy_gbdt_train.txt")
write.table(conf_matrix, file = "gbdt_train_result.txt", quote = F, sep = '\t', row.names = T, col.names = T)

# Prediction on the test set
print("-----测试集数据-----")
test_result <- predict(gbdt_model, dtest)
conf_matrix <- table(true = test_label, predict = test_result)
gbdt_test_result <- sum(diag(conf_matrix)) / sum(conf_matrix)

accuracy_gbdt_test = paste(round(gbdt_test_result * 100, 2), "%", sep = '')
write.table(accuracy_gbdt_test, file = "accuracy_gbdt_test.txt")
write.table(conf_matrix, file = "gbdt_test_result.txt", quote = F, sep = '\t', row.names = T, col.names = T)

print("--------------GBDT end--------------")




