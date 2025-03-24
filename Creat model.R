library(randomForest)
library(pROC)
library(caret)
library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)

args <- commandArgs(trailingOnly = TRUE)

group_file <- args[which(args == "-group") + 1]
data <- read.table("lefse_input_form_group.xls", header = TRUE, sep = "\t", check.names = FALSE)   ##lefse input file

# Replace the characters in the first column
data[, 1] <- gsub("\\.", "_", data[, 1])
data[, 1] <- gsub("-", "_", data[, 1])
data[, 1] <- gsub("\\|", ".", data[, 1])
data[, 1] <- gsub("\\[", "_", data[, 1])
data[, 1] <- gsub("\\]", "_", data[, 1]) 
data[, 1] <- gsub("\\(", "_", data[, 1])
data[, 1] <- gsub("\\)", "_", data[, 1])


# Write the modified data back to the file
#write.table(data, "newname_lefse_input_form_group.xls", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
rownames(data)<-data$Taxonomy

import_info <- read.table("hmp_aerobiosis_small.res", header = F, sep = "\t", check.names = FALSE)   ##lefse output file

import_info[, 5] <- as.numeric(import_info[, 5])

# Filter the fifth column that is less than 0.05 and not NA
filtered_data <- import_info[import_info[, 5] < 0.05 & !is.na(import_info[, 5]), ]
# Extract 1st column
first_column <- filtered_data[, 1]
selected_rows <- first_column[grepl("s__", first_column)]
importance<-data[selected_rows,]
importance$Taxonomy<-NULL

otu_table<-importance
otu_table_t<-t(otu_table)
otu_table_t <- as.data.frame(otu_table_t)
group_type<-read.table(group_file,sep = "\t",col.names = c("SampleID", "Group"),row.names = 1)

print(group_type)

data_train <- read.table("Training Cohort.txt",sep = "\t",header = T,check.names = F)  
sample_train<-data_train$Sample
select_sample<-intersect(rownames(group_type), sample_train)
info_train<-otu_table_t[select_sample,]
info_train$Group<-group_type[select_sample,]
info_train$Group <- as.factor(info_train$Group)

inter_test_train <- read.table("Inter_test.txt",sep = "\t",header = T,check.names = F)
sample_inter_test<-inter_test_train$Sample
select_sample_in<-intersect(rownames(group_type), sample_inter_test)
info_inter_test<-otu_table_t[select_sample_in,]
info_inter_test$Group<-group_type[select_sample_in,]
info_inter_test$Group <- as.factor(info_inter_test$Group)

exter_test_train <- read.table("Exter_test.txt",sep = "\t",header = T,check.names = F)
sample_exter_test<-exter_test_train$Sample
select_sample_ex<-intersect(rownames(group_type), sample_exter_test)
info_exter_test<-otu_table_t[select_sample_ex,]
info_exter_test$Group<-group_type[select_sample_ex,]
info_exter_test$Group <- as.factor(info_exter_test$Group)

X_train <- info_train[, -ncol(info_train)]
Y_train <- info_train$Group
X_test_in <- info_inter_test[, -ncol(info_inter_test)]
Y_test_in <- info_inter_test$Group
X_test_ex <- info_exter_test[, -ncol(info_exter_test)]
Y_test_ex <- info_exter_test$Group

model_info<-"rf"         ###you can be replaced with other models
roc_list <- list()
model_list<-list()

cl<-makePSOCKcluster(20)

registerDoParallel(cl)

#xgb_grid <- expand.grid(
#  nrounds = c(50, 100),
#  max_depth = c(3, 5),
#  eta = c(0.1, 0.3),
#  gamma = c(0, 0.1),
#  colsample_bytree = c(0.8),
#  min_child_weight = c(1),
#  subsample = c(0.8)
#)

for (i in 1:100) {
  set.seed(123 + i)
  gm_model <- train(
    x = X_train, y = Y_train,
    method = model_info,   
    metric = "ROC",
    trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions="final"),
    tuneLength = 5,    # Automatically adjusts the hyperparameters
    #tuneGrid = xgb_grid
  )
  
  rf_probs <- predict(gm_model, X_test_in, type = "prob")[,1]
  roc_L <- roc(Y_test_in, rf_probs)
  #Save ROC results
  model_list[[i]]<-gm_model
  roc_list[[i]] <-roc_L
}

best_index <- which.max(sapply(roc_list, function(x) x$auc))
roc_rf_in <- roc_list[[best_index]]
best_model_rf<-model_list[[best_index]]

rf_probs <- predict(best_model_rf, X_test_ex, type = "prob")[,1]
roc_rf_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_rf$pred
roc_rf_train<-roc(predictions[,3],predictions[,5])

pdf("rf_model_all_v2.pdf")
plot(roc_rf_train, col = "blue", main = "ROC Curve Comparison", print.auc = F, lwd = 2)
plot(roc_rf_in, col = "red", add = TRUE, print.auc = F, lwd = 2)
plot(roc_rf_ex, col = "green", add = TRUE, print.auc = F, lwd = 2)

ci_auc_train <- ci.auc(roc_rf_train)
ci_auc_in <- ci.auc(roc_rf_in)
ci_auc_ex <- ci.auc(roc_rf_ex)

auc_text_rf <- paste0("Training Cohort AUC: ", round(auc(roc_rf_train), 3), 
                      " (95% CI: ", round(ci_auc_train[1], 3), "-", round(ci_auc_train[3], 3), ")")


auc_text_in <- paste0("Inter Cohort AUC: ", round(auc(roc_rf_in), 3), 
                      " (95% CI: ", round(ci_auc_in[1], 3), "-", round(ci_auc_in[3], 3), ")")


auc_text_ex <- paste0("Exter Cohort AUC: ", round(auc(roc_rf_ex), 3), 
                      " (95% CI: ", round(ci_auc_ex[1], 3), "-", round(ci_auc_ex[3], 3), ")")
legend("bottomright", 
       legend = c(auc_text_rf, auc_text_in, auc_text_ex),
       col = c("blue", "red", "green"), lwd = 2, cex = 0.8)
dev.off()

model_info<-"rf"
rf_probs <- predict(best_model_rf, X_test_in, type = "prob")[, 1]
y_pred <- predict(best_model_rf, X_test_in)

roc_L <- roc(Y_test_in, rf_probs)
cm <- confusionMatrix(y_pred, Y_test_in,positive = "Malignant")
print(cm)
tp <- cm$table[1,1]  
tn <- cm$table[2,2]  
fp <- cm$table[1,2] 
fn <- cm$table[2,1]  

# Sensitivity, Specificity, Accuracy
sensitivity_value <- cm$byClass["Sensitivity"]  
specificity_value <- cm$byClass["Specificity"] 
accuracy_value <- cm$overall["Accuracy"]        

# 95% confidence intervals for sensitivity, specificity, and accuracy were calculated
sens_ci <- binom.test(tp, tp + fn)$conf.int * 100  
spec_ci <- binom.test(tn, tn + fp)$conf.int * 100  
acc_ci <- binom.test(tp + tn, tp + tn + fp + fn)$conf.int * 100 
auc_ci <- ci.auc(roc_L)
results_df <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Accuracy", "AUC"),
  Value = c(
    sprintf("%.1f%%", sensitivity_value * 100),
    sprintf("%.1f%%", specificity_value * 100),
    sprintf("%.1f%%", accuracy_value * 100),
    sprintf("%.3f", auc(roc_L))
  ),
  `95% CI` = c(
    sprintf("(%.1f%% - %.1f%%)", sens_ci[1], sens_ci[2]),
    sprintf("(%.1f%% - %.1f%%)", spec_ci[1], spec_ci[2]),
    sprintf("(%.1f%% - %.1f%%)", acc_ci[1], acc_ci[2]),
    sprintf("(%.3f - %.3f)", auc_ci[1], auc_ci[3])
  )
)
files_names<-paste(model_info, "_Inter_metrics_2calss_v2.csv", sep="")
write.csv(results_df, file = files_names, row.names = FALSE)

rf_probs <- predict(best_model_rf, X_test_ex, type = "prob")[, 2]
y_pred <- predict(best_model_rf, X_test_ex)
roc_L <- roc(Y_test_ex, rf_probs)
cm <- confusionMatrix(y_pred, Y_test_ex,positive = "Malignant")
print(cm)
tp <- cm$table[1,1]  
tn <- cm$table[2,2]  
fp <- cm$table[1,2] 
fn <- cm$table[2,1]  
# Sensitivity, Specificity, Accuracy
sensitivity_value <- cm$byClass["Sensitivity"] 
specificity_value <- cm$byClass["Specificity"] 
accuracy_value <- cm$overall["Accuracy"]       

sens_ci <- binom.test(tp, tp + fn)$conf.int * 100  
spec_ci <- binom.test(tn, tn + fp)$conf.int * 100  
acc_ci <- binom.test(tp + tn, tp + tn + fp + fn)$conf.int * 100  
auc_ci <- ci.auc(roc_L)
results_df <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Accuracy", "AUC"),
  Value = c(
    sprintf("%.1f%%", sensitivity_value * 100),
    sprintf("%.1f%%", specificity_value * 100),
    sprintf("%.1f%%", accuracy_value * 100),
    sprintf("%.3f", auc(roc_L))
  ),
  `95% CI` = c(
    sprintf("(%.1f%% - %.1f%%)", sens_ci[1], sens_ci[2]),
    sprintf("(%.1f%% - %.1f%%)", spec_ci[1], spec_ci[2]),
    sprintf("(%.1f%% - %.1f%%)", acc_ci[1], acc_ci[2]),
    sprintf("(%.3f - %.3f)", auc_ci[1], auc_ci[3])
  )
)
files_names<-paste(model_info, "_Exter_metrics_2calss_v2.csv", sep="")
write.csv(results_df, file = files_names, row.names = FALSE)

###If you want to extract important features, use the following code
#rfModel <- randomForest(
#  x = X_train, 
#  y = Y_train, 
#  importance = TRUE
#)
#featureImportance <- importance(rfModel, type = 1)  # Mean Decrease Accuracy
#importantFeatures <- featureImportance[featureImportance[, "MeanDecreaseAccuracy"] > 1, ]
#selectedFeatures <- rownames(importantFeatures)
# Screening important features
#X_train_reduced <- X_train[, selectedFeatures]