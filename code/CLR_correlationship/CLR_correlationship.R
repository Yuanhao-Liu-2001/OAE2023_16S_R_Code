# 加载必要的包
library(compositions)  # 用于CLR变换
library(ggplot2)      # 绘图
library(ggpubr)       # 添加统计信息
library(tidyverse)    # 数据整理
library(patchwork)

setwd("D:/Files/CMEE/Mesocosms/论文用图/蓝藻与叶绿素的相关性")

# 假设数据格式：
# 1. rel_abundance.csv: 行为样本，列为微生物类群（含Cyanobacteria列），第一列为SampleID
# 2. env_data.csv: 包含SampleID和Chla列（叶绿素a浓度）

# --------------------- 1. 数据准备 ---------------------
rel_abund_Wet <- read.delim('feature-table_10142_L2_DFE.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
env_data_Wet <- read.delim('Chlorophyll_DFE.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

# --------------------- 2. CLR变换 ---------------------
# CLR变换要求数据严格为正数，建议将0替换为一个小值（如1e-6）
rel_abund_no_zero_Wet <- rel_abund_Wet %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, 1e-6, .)))
         
# 执行CLR变换
clr_transformed_Wet <- clr(rel_abund_no_zero_Wet)
         
# 提取蓝藻的CLR值
cyanobacteria_clr_Wet <- data.frame(
  SampleID = rownames(clr_transformed_Wet),
  Cyanobacteria_CLR = as.numeric(clr_transformed_Wet[, "Cyanobacteria"])
)
         
# 合并环境数据
merged_data_Wet <- merge(cyanobacteria_clr_Wet, env_data_Wet, by = "SampleID")
         
# --------------------- 3. 相关性分析与绘图 ---------------------
# 计算Pearson相关系数
cor_test_Wet <- cor.test(merged_data_Wet$Cyanobacteria_CLR, merged_data_Wet$Chlorophyll, method = "pearson")
cor_label_Wet <- paste0("r = ", round(cor_test_Wet$estimate, 4), 
                             ",   p-value = ", signif(cor_test_Wet$p.value, 4))
         
# 绘制散点图
p_Wet <- ggplot(merged_data_Wet, aes(x = Cyanobacteria_CLR, y = Chlorophyll)) +
  geom_point(size = 1.5, color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "solid") +
  labs(
    title = "Wet season mesocosm",
    x = "CLR-transformed Cyanobacteria Relative Abundance",
    y = "Chlorophyll-a (μg/L)",
    tag = "a"
  ) +
  annotate("text", x = min(merged_data_Wet$Cyanobacteria_CLR), 
           y = max(merged_data_Wet$Chlorophyll),
           label = cor_label_Wet, hjust = -0.01, vjust = 1, size = 3) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.tag = element_text(size = 10.5, face = "bold")
  )



# --------------------- 1. 数据准备 ---------------------
rel_abund_Dry <- read.delim('feature-table_10142_L2_GJH.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
env_data_Dry <- read.delim('Chlorophyll_GJH.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

# --------------------- 2. CLR变换 ---------------------
# CLR变换要求数据严格为正数，建议将0替换为一个小值（如1e-6）
rel_abund_no_zero_Dry <- rel_abund_Dry %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, 1e-6, .)))

# 执行CLR变换
clr_transformed_Dry <- clr(rel_abund_no_zero_Dry)

# 提取蓝藻的CLR值
cyanobacteria_clr_Dry <- data.frame(
  SampleID = rownames(clr_transformed_Dry),
  Cyanobacteria_CLR = as.numeric(clr_transformed_Dry[, "Cyanobacteria"])
)

# 合并环境数据
merged_data_Dry <- merge(cyanobacteria_clr_Dry, env_data_Dry, by = "SampleID")

# --------------------- 3. 相关性分析与绘图 ---------------------
# 计算Pearson相关系数
cor_test_Dry <- cor.test(merged_data_Dry$Cyanobacteria_CLR, merged_data_Dry$Chlorophyll, method = "pearson")
cor_label_Dry <- paste0("r = ", round(cor_test_Dry$estimate, 4), 
                        ",   p-value = ", signif(cor_test_Dry$p.value, 4))

# 绘制散点图
p_Dry <- ggplot(merged_data_Dry, aes(x = Cyanobacteria_CLR, y = Chlorophyll)) +
  geom_point(size = 1.5, color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "solid") +
  labs(
    title = "Dry season mesocosm",
    x = "CLR-transformed Cyanobacteria Relative Abundance",
    y = "Chlorophyll-a (μg/L)",
    tag = "b"
  ) +
  annotate("text", x = min(merged_data_Dry$Cyanobacteria_CLR), 
           y = max(merged_data_Dry$Chlorophyll),
           label = cor_label_Dry, hjust = -0.01, vjust = 1, size = 3) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.tag = element_text(size = 10.5, face = "bold")
  )



# --------------------- 1. 数据准备 ---------------------
rel_abund_55L <- read.delim('feature-table_10142_L2_55L.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
env_data_55L <- read.delim('Chlorophyll_55L.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

# --------------------- 2. CLR变换 ---------------------
# CLR变换要求数据严格为正数，建议将0替换为一个小值（如1e-6）
rel_abund_no_zero_55L <- rel_abund_55L %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, 1e-6, .)))

# 执行CLR变换
clr_transformed_55L <- clr(rel_abund_no_zero_55L)

# 提取蓝藻的CLR值
cyanobacteria_clr_55L <- data.frame(
  SampleID = rownames(clr_transformed_55L),
  Cyanobacteria_CLR = as.numeric(clr_transformed_55L[, "Cyanobacteria"])
)

# 合并环境数据
merged_data_55L <- merge(cyanobacteria_clr_55L, env_data_55L, by = "SampleID")

# --------------------- 3. 相关性分析与绘图 ---------------------
# 计算Pearson相关系数
cor_test_55L <- cor.test(merged_data_55L$Cyanobacteria_CLR, merged_data_55L$Chlorophyll, method = "pearson")
cor_label_55L <- paste0("r = ", round(cor_test_55L$estimate, 4), 
                        ",   p-value = ", signif(cor_test_55L$p.value, 4))

# 绘制散点图
p_55L <- ggplot(merged_data_55L, aes(x = Cyanobacteria_CLR, y = Chlorophyll)) +
  geom_point(size = 1.5, color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "solid") +
  labs(
    title = "55L microcosm",
    x = "CLR-transformed Cyanobacteria Relative Abundance",
    y = "Chlorophyll-a (μg/L)",
    tag = "c"
  ) +
  annotate("text", x = min(merged_data_55L$Cyanobacteria_CLR), 
           y = max(merged_data_55L$Chlorophyll),
           label = cor_label_55L, hjust = -0.01, vjust = 1, size = 3) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.tag = element_text(size = 10.5, face = "bold")
  )

#合成总图
p <- p_Wet|p_Dry|p_55L
