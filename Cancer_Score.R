library(ggplot2)
library(ggsignif)

best_roc_ex<-roc_exter
best_roc_in<-roc_inter
X_test <- info_inter_test[, -ncol(info_inter_test)]
Y_test <- info_inter_test$Stage

rf_probs_train<- predict(best_model_rf, X_train, type = "prob")
stage_info <- read.table("group_stage_II.txt",sep='\t',header = FALSE, col.names = c("SampleID", "Stage"))
rf_probs_train$Sample<-rownames(rf_probs_train)
merged_df <- merge(rf_probs_train, stage_info, by.x = "Sample", by.y = "SampleID")


pdf("Cancer_Scores_train.pdf")
ggplot(merged_df, aes(x = Stage, y = Malignant, fill = Stage)) +
  geom_boxplot(aes(fill=Stage), position = position_dodge(width = 0.6),  # 调整箱线图之间的空隙
               outlier.shape = NA, size = 0.9, width = 0.5, alpha = 0.8) +  # 调整箱线图边框颜色、大小、宽度和透明度
  geom_jitter(position = position_jitterdodge(jitter.width = 0.25, # 散点抖动宽度
                                              dodge.width = 0.7))+  # 减小散点抖动范围的宽度，避免散点溢出
  geom_signif(
    comparisons = list(c("Benign", "Early"), c("Benign", "Late")),
    step_increase = 0.3,
    map_signif_level = TRUE,
    test = wilcox.test
  )+  # 添加显著性标注
  scale_fill_manual(values = c("#0072B5", "#E18727",
                               "#de1c31")) +  # 蓝色和橙色配色方案
  scale_color_manual(values = c("#0072B5", "#E18727","#de1c31")) +  # 蓝色和橙色的散点颜色
  theme_minimal(base_size = 14) +  # 简洁的主题和更大字号
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(size = 1, color = "black"),  # 添加坐标轴线并加粗
    panel.grid = element_blank(),  # 移除网格线
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Training cohort", y = "Cancer Score", x = "") +
  theme_minimal() +
  theme(legend.position = "none") # 隐藏图例
dev.off()
external