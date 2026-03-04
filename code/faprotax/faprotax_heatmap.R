library(vegan)
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)

wd="C:/Users/l1065/Documents/Voyage/github/code/faprotax"
setwd(wd)

#直接导入表格开始画，由于是热图，对原始的相对丰度进行了对数lg运算
plot_data <- read.delim("feature-table_10142_BG_log.faprotax", header=T, sep="\t", row.names=1, stringsAsFactors = FALSE,check.names=F)
plot_data <- plot_data%>% mutate(plot_data=row.names(.)) %>% melt()#转化为ggplot画图需要的长列表
colnames(plot_data)[1] <- "Pathways"
colnames(plot_data)[2] <- "Sample"
colnames(plot_data)[3] <- "Number"
plot_data$Pathways <- factor(plot_data$Pathways,levels = c("*chemoheterotrophy","**photoautotrophy","*intracellular_parasites","**nitrate_reduction","hydrocarbon_degradation","**aerobic_chemoheterotrophy","cellulolysis","*methylotrophy"))

#将Group的元素从字符串的转化为factor，通过指定level对横轴坐标进行排序
#教程：https://blog.csdn.net/neweastsun/article/details/121801992
metadata <- read.delim('sample-metadata_BG.tsv', sep = '\t',header = T, stringsAsFactors = FALSE, check.names = FALSE)
metadata$Group <- factor(metadata$Group,levels = c("Wet_Background", "Dry_Background"))
metadata$sample_id <- factor(metadata$sample_id, levels = c("19/06/2023","23/06/2023","27/06/2023","01/07/2023","05/07/2023","10/07/2023","13/07/2023","17/07/2023","21/07/2023","25/07/2023","30/07/2023","01/01/2024","09/01/2024","17/01/2024","29/01/2024"))

#颜色分组
group_colors <- c(
  "Wet_Background" = "#9400ff",
  "Dry_Background" = "#ffc0ca"
)

#顶部的分组色块
group <- metadata %>%
  mutate(p="Group") %>%
  ggplot(aes(sample_id,y=p,fill=Group))+
  geom_tile() + 
  scale_y_discrete(labels = NULL) +
  theme_minimal()+xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values = group_colors) +
  labs(fill = "Group")
group

####################
#可视化
library(patchwork)
library(ggpubr)

p1 <- ggplot(plot_data,aes(x=Sample,y=Pathways,fill=Number)) + #热图绘制
  geom_raster() + scale_fill_gradient2(low="#003366", high="#990033", mid="white") +
  labs(fill = "lg(Number of ASVs)")+
  theme(axis.title.x = element_blank(),
    axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1))
p1

总图
p_all <- group/p1 + plot_layout(nrow  = 2, heights = c(1, 9))
p_all

###################
# 显著性检验
###################

#!!!!!!!!!!!!!!以下内容只是用来确定某一群落功能是否具有显著差异（即图片中行的名称是否带有*以及有几颗*）
plot_data_test <- read.delim("feature-table_10142_BG.faprotax", header=T, sep="\t", row.names=1, stringsAsFactors = FALSE,check.names=F)

#将Group的元素从字符串的转化为factor，通过指定level对横轴坐标进行排序
#教程：https://blog.csdn.net/neweastsun/article/details/121801992
plot_data_test$Group <- factor(plot_data_test$Group,levels = c("Wet_Background", "Dry_Background"))

my_all = list(c("Wet_Background", "Dry_Background"))

p1 <- ggplot(data = plot_data_test, aes(x = Group, y = methylotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'methylotrophy', tags = "A")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p2 <- ggplot(data = plot_data_test, aes(x = Group, y = cellulolysis))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'cellulolysis', tags = "B")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p3 <- ggplot(data = plot_data_test, aes(x = Group, y = aerobic_chemoheterotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'aerobic_chemoheterotrophy', tags = "C")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p4 <- ggplot(data = plot_data_test, aes(x = Group, y = hydrocarbon_degradation))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'hydrocarbon_degradation', tags = "D")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p5 <- ggplot(data = plot_data_test, aes(x = Group, y = nitrate_reduction))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'nitrate_reduction', tags = "E")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p6 <- ggplot(data = plot_data_test, aes(x = Group, y = intracellular_parasites))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'intracellular_parasites', tags = "F")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p7 <- ggplot(data = plot_data_test, aes(x = Group, y = photoautotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'photoautotrophy', tags = "G")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)
p8 <- ggplot(data = plot_data_test, aes(x = Group, y = chemoheterotrophy))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = "Number of ASVs", subtitle = 'chemoheterotrophy', tags = "H")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1), axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = group_colors) +
  # ylim(100, 1150) +
  stat_compare_means(comparisons=my_all
                     , text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)

p1
p2
p3
p4
p5
p6
p7
p8
