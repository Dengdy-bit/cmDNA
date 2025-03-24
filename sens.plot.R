library(pROC)
library(ggplot2)
library(dplyr)
group_file <- "group.txt"
data <- read.table("lefse_input_form_group.xls", header = TRUE, sep = "\t", check.names = FALSE)

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

import_info <- read.table("hmp_aerobiosis_small.res", header = F, sep = "\t", check.names = FALSE)

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


##gender
Title="Gender"
pos<-"Male"
nes<-"Female"

Male_sample <- data_all %>% 
  filter(Gender == pos) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Gender == nes) %>% 
  select(Sample) %>% 
  pull()

disease_group_male <- data_all %>%
  filter(Sample %in% Male_sample) %>%
  select(Disease_Group) %>%
  pull()

disease_group_female <- data_all %>%
  filter(Sample %in% Female_sample) %>%
  select(Disease_Group) %>%
  pull()

info_male<-otu_table_t[Male_sample,]
info_male$Group<-disease_group_male
info_male$Group <- as.factor(info_male$Group)

info_female<-otu_table_t[Female_sample,]
info_female$Group<-disease_group_female
info_female$Group <- as.factor(info_female$Group)
X_male <- info_male[, -ncol(info_male)]
Y_male <- info_male$Group
X_female <- info_female[, -ncol(info_female)]
Y_female <- info_female$Group

male_pred <- predict(best_model_rf, X_male)

true_benign_m <- sum(Y_male == "Malignant" & male_pred == "Malignant")
#false_benign =sum(Y_male == "Benign" & male_pred == "Malignant")
#true_malignant =sum(Y_male == "Benign" & male_pred == "Benign")  # TP
false_malignant_m <-  sum(Y_male == "Malignant" & male_pred == "Benign")  # FP  # FN

male_sens<-true_benign_m/(true_benign_m+false_malignant_m)

female_pred <- predict(best_model_rf, X_female)
true_benign_f = sum(Y_female == "Malignant" & female_pred == "Malignant")  # TP
false_malignant_f = sum(Y_female == "Malignant" & female_pred == "Benign")

female_sens<-true_benign_f/(true_benign_f+false_malignant_f)

sensitivity_data <- data.frame(
  Gender = c(pos, nes),
  Sensitivity = c(female_sens, male_sens),
  true_benign =c(true_benign_f,true_benign_m),
  false_malignant=c(false_malignant_f,false_malignant_m)
)

rf_model_impor <- randomForest(
  Group ~ .,  # Suppose 'Group' is the target variable
  data = info_train,  
  importance = TRUE  
)
importance_scores <- importance(rf_model_impor, type = 1)
important_features <- importance_scores[importance_scores[, "MeanDecreaseAccuracy"] > 1, ]


###Change the parameters
Title<="Drinking"
pos<-"Drinking"
nes<-"No-drinking"

Male_sample <- data_all %>% 
  filter(Drinking_History %in% c("Drinking","Quit-drinking")) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Drinking_History == "Non-drinking") %>% 
  select(Sample) %>% 
  pull()


pos<-"Male"
nes<-"Female"

Male_sample <- data_all %>% 
  filter(Gender == pos) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Gender == nes) %>% 
  select(Sample) %>% 
  pull()


Title="HER"
pos<-"HER2+"
nes<-"HER2-"

Male_sample <- data_all %>% 
  filter(MSS_Group == pos) %>% 
  select(Sample) %>% 
  pull()

# Use the filter() function to filter out samples whose Age is less than 60
Female_sample <- data_all %>% 
  filter(HER2_Status ==nes) %>% 
  select(Sample) %>% 
  pull()

###### If there's ">" symbol
Title="CA724"
pos<-"High"
nes<-"Low"
yuzhi<-6.9

Male_sample <- data_all %>% 
  filter(`CA724(<6.90)`>yuzhi) %>% 
  select(Sample) %>% 
  pull()


Female_sample <- data_all %>% 
  filter(`CA724(<6.90)`<=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Title="CEA"
pos<-"High"
nes<-"Low"
yuzhi<-5

Male_sample <- data_all %>% 
  filter(`CEA(<5.00)`>=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(`CEA(<5.00)`<yuzhi) %>% 
  select(Sample) %>% 
  pull()

####stop!!!#####

disease_group_male <- data_all %>%
   filter(Sample %in% Male_sample) %>%
  select(Disease_Group) %>%
  pull()
disease_group_female <- data_all %>%
  filter(Sample %in% Female_sample) %>%
  select(Disease_Group) %>%
  pull()
info_male<-otu_table_t[Male_sample,]
info_male$Group<-disease_group_male
info_male$Group <- as.factor(info_male$Group)

info_female<-otu_table_t[Female_sample,]
info_female$Group<-disease_group_female
info_female$Group <- as.factor(info_female$Group)
X_male <- info_male[, -ncol(info_male)]
Y_male <- info_male$Group
X_female <- info_female[, -ncol(info_female)]
Y_female <- info_female$Group

male_pred <- predict(best_model_rf, X_male)

true_benign_m = sum(Y_male == "Malignant" & male_pred == "Malignant") # TP

false_malignant_m =  sum(Y_male == "Malignant" & male_pred == "Benign")

male_sens<-true_benign_m/(true_benign_m+false_malignant_m)

female_pred <- predict(best_model_rf, X_female)
true_benign_f = sum(Y_female == "Malignant" & female_pred == "Malignant")
false_malignant_f = sum(Y_female == "Malignant" & female_pred == "Benign")

female_sens<-true_benign_f/(true_benign_f+false_malignant_f)

sensitivity_data <- data.frame(
  Gender = c(pos, nes),
  Sensitivity = c(male_sens,female_sens),
  true_benign =c(true_benign_m,true_benign_f),
  false_malignant=c(false_malignant_m,false_malignant_f)
)
sensitivity_data$Gender <- factor(sensitivity_data$Gender, levels = c(pos, nes))
pdf_filename <- paste0(Title, "_sensitivity.pdf")

pdf(pdf_filename)
p1 <- ggplot(sensitivity_data, aes(x = Gender, y = Sensitivity * 100, fill = Gender)) + 
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = Sensitivity * 50, label = sprintf("%.1f%%", Sensitivity * 100)),  
            vjust = 0.5, hjust = 0.5, color = "white", size = 5) +  
  geom_text(aes(label = paste0(Gender, " (", true_benign, "/", true_benign + false_malignant, ")")), vjust = -0.5) +
  labs(title = "Sensitivity", x = Title, y = "Sensitivity (%)") +  
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#00a381", "#824880"))  

print(p1)
dev.off()
