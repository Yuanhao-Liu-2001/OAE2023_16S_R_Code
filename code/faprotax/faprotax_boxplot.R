#faprotax的结果是在服务器上跑出来的，即feature-table_10142.faprotax

library(vegan)
library(ggplot2)
library(data.table)
library(reshape2)
wd="D:/Files/CMEE/Mesocosms/论文用图/FAPROTAX"
setwd(wd)

#直接导入表格开始画
#只使用加入氢氧化镁之后的样品
plot_data <- read.delim("feature-table_10142.faprotax", header=T, sep="\t", row.names=1, stringsAsFactors = FALSE,check.names=F)

#将Group的元素从字符串的转化为factor，通过指定level对横轴坐标进行排序
#教程：https://blog.csdn.net/neweastsun/article/details/121801992
plot_data$Group <- factor(plot_data$Group,levels = c("Wet_Meso_Control", "Wet_Meso_Low", "Wet_Meso_Medium", "Dry_Meso_Control", "Dry_Meso_Low", "Dry_Meso_Medium", "Dry_Micro_Control", "Dry_Micro_Low", "Dry_Micro_Medium", "Dry_Micro_High"))

####################
#可视化
library(patchwork)
library(ggpubr)

#设置要两两比对的组合
my_all = list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"), c("Wet_Meso_Control", "Wet_Meso_Low"), c("Wet_Meso_Control", "Wet_Meso_Medium"), c("Wet_Meso_Low", "Wet_Meso_Medium"), c("Dry_Meso_Control", "Dry_Meso_Low"), c("Dry_Meso_Control", "Dry_Meso_Medium"), c("Dry_Meso_Low", "Dry_Meso_Medium"), c("Dry_Micro_Control", "Dry_Micro_Low"), c("Dry_Micro_Control", "Dry_Micro_Medium"),c("Dry_Micro_Control", "Dry_Micro_High"), c("Dry_Micro_Low", "Dry_Micro_Medium"), c("Dry_Micro_Low", "Dry_Micro_High"), c("Dry_Micro_Medium", "Dry_Micro_High"))

group_colors <- c(
  "Wet_Meso_Control" = "#00ff00", # 浅红
  "Wet_Meso_Low" = "#1eb31e", # 浅绿
  "Wet_Meso_Medium" = "#3c673c",  # 浅蓝
  "Dry_Meso_Control" = "#00ffff", # 浅红
  "Dry_Meso_Low" = "#02bbff", # 浅绿
  "Dry_Meso_Medium" = "#0532ff",  # 浅蓝
  "Dry_Micro_Control" = "#ffc60a",
  "Dry_Micro_Low" = "#ff9107",
  "Dry_Micro_Medium" = "#ff2700",
  "Dry_Micro_High" = "#9d4d3e"
)
  
#绘图，使用wilcoxon检验
p1 <- ggplot(data = plot_data, aes(x = Group, y = methylotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'methylotrophy', tags = "a")+
  theme(axis.text.x=element_blank(), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p2 <- ggplot(data = plot_data, aes(x = Group, y = cellulolysis))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'cellulolysis', tags = "b")+
  theme(axis.text.x=element_blank(), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p3 <- ggplot(data = plot_data, aes(x = Group, y = aerobic_chemoheterotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'aerobic_chemoheterotrophy', tags = "c")+
  theme(axis.text.x=element_blank(), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p4 <- ggplot(data = plot_data, aes(x = Group, y = hydrocarbon_degradation))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'hydrocarbon_degradation', tags = "d")+
  theme(axis.text.x=element_blank(), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p5 <- ggplot(data = plot_data, aes(x = Group, y = nitrate_reduction))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'nitrate_reduction', tags = "e")+
  theme(axis.text.x=element_blank(), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p6 <- ggplot(data = plot_data, aes(x = Group, y = intracellular_parasites))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'intracellular_parasites', tags = "f")+
  theme(axis.text.x=element_blank(), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"),c("Wet_Meso_Control","Wet_Meso_Low"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p7 <- ggplot(data = plot_data, aes(x = Group, y = photoautotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'photoautotrophy', tags = "g")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"),c("Dry_Meso_Low","Dry_Meso_Medium"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p8 <- ggplot(data = plot_data, aes(x = Group, y = chemoheterotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'chemoheterotrophy', tags = "h")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"),c("Dry_Meso_Control","Dry_Meso_Medium"))
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)

#总图
p_all <- (p1/p3/p5/p7)|(p2/p4/p6/p8)
p_all

