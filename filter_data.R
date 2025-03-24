args <- commandArgs(trailingOnly = TRUE)

group_file <- args[which(args == "-group") + 1]
input_file <- args[which(args == "-in") + 1]
output_file <- args[which(args == "-out") + 1]

# 读取 group 文件中的样本名
group <- read.table(group_file, header = F, stringsAsFactors = FALSE)
sample_names <- group[[1]]  # 提取第一列样本名

# 读取 filter_otu_table_even.txt 文件
otu_table <- read.table(input_file,  sep='\t', header = TRUE, stringsAsFactors = FALSE,check.names = FALSE)

# 保留第一列、最后一列以及根据样本名筛选的列
selected_columns <- c(names(otu_table)[1], sample_names, names(otu_table)[ncol(otu_table)])
filtered_otu_table <- otu_table[, selected_columns, drop = FALSE]
# 保存筛选后的数据到参数指定的输出文件
write.table(filtered_otu_table, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
