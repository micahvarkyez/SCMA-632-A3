# Install necessary packages if not already installed
if (!require(caTools)) install.packages('caTools', dependencies=TRUE)
if (!require(ROCR)) install.packages('ROCR', dependencies=TRUE)
if (!require(caret)) install.packages('caret', dependencies=TRUE)
if (!require(pROC)) install.packages('pROC', dependencies=TRUE)
if (!require(rpart)) install.packages('rpart', dependencies=TRUE)
if (!require(rpart.plot)) install.packages('rpart.plot', dependencies=TRUE)

# Load the libraries
library(caTools)
library(ROCR)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)

# Load the dataset
setwd("C:\\Users\\Dell\\Desktop\\MICAH")
getwd()
data = read.csv("Car Evaluation.csv")
head(data)

# Convert decision column
data$decision <- ifelse(data$decision == "acceptable", 1, 0)
data$decision

set.seed(123) # for reproducibility
sample_size <- floor(0.8 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = sample_size)

trainData <- data[train_indices, ]
testData <- data[-train_indices, ]

# Fit logistic regression model
logistic_model <- glm(decision ~ ., data = trainData, family = binomial)

# Summary of the model
summary(logistic_model)

# Predict on test data
logistic_pred <- predict(logistic_model, testData, type = "response")
logistic_pred_class <- ifelse(logistic_pred > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = logistic_pred_class, Actual = testData$decision)
print(conf_matrix)

# Calculate precision, recall, and F1 score
precision <- conf_matrix[2,2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2,2] / sum(conf_matrix[, 2])
f1_score <- 2 * precision * recall / (precision + recall)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1 Score: ", f1_score, "\n")

# ROC and AUC
roc_curve <- roc(testData$decision, logistic_pred)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC = ", round(auc_value, 2), ")", sep = ""))

# Fit decision tree model
tree_model <- rpart(decision ~ ., data = trainData, method = "class")

# Plot the decision tree
rpart.plot(tree_model, type = 3, extra = 101)

# Predict on test data
tree_pred <- predict(tree_model, testData, type = "class")

# Confusion matrix
tree_conf_matrix <- table(Predicted = tree_pred, Actual = testData$decision)
print(tree_conf_matrix)

# Calculate precision, recall, and F1 score for decision tree
tree_precision <- tree_conf_matrix[2,2] / sum(tree_conf_matrix[2, ])
tree_recall <- tree_conf_matrix[2,2] / sum(tree_conf_matrix[, 2])
tree_f1_score <- 2 * tree_precision * tree_recall / (tree_precision + tree_recall)
tree_accuracy <- sum(diag(tree_conf_matrix)) / sum(tree_conf_matrix)

cat("Decision Tree Accuracy: ", tree_accuracy, "\n")
cat("Decision Tree Precision: ", tree_precision, "\n")
cat("Decision Tree Recall: ", tree_recall, "\n")
cat("Decision Tree F1 Score: ", tree_f1_score, "\n")
