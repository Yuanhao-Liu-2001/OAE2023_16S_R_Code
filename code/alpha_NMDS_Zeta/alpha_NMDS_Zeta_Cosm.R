#教程：https://www.jianshu.com/p/6c28a596e280
####################
#Alpha多样性指数计算
library(vegan)
library(ggplot2)
library(data.table)
library(reshape2)
wd="D:/Files/CMEE/Mesocosms/论文用图/alpha_beta"
setwd(wd)

###########################
# Alpha diversity
###########################

#直接导入多样性表格开始画
#只使用加入氢氧化镁之后的样品
plot_data_55L <- read.delim("feature-table_10142_alpha_55L.tsv", header=T, sep="\t", row.names=1, stringsAsFactors = FALSE,check.names=F)
plot_data_50000L <- read.delim("feature-table_10142_alpha_50000L.tsv", header=T, sep="\t", row.names=1, stringsAsFactors = FALSE,check.names=F)

#将Group的元素从字符串的转化为factor，通过指定level对横轴坐标进行排序
#教程：https://blog.csdn.net/neweastsun/article/details/121801992
plot_data_55L$Group <- factor(plot_data_55L$Group,levels = c("Dry_Micro_Control", "Dry_Micro_Low", "Dry_Micro_Medium", "Dry_Micro_High"))
plot_data_50000L$Group <- factor(plot_data_50000L$Group,levels = c("Wet_Meso_Control", "Dry_Meso_Control", "Wet_Meso_Low", "Dry_Meso_Low", "Wet_Meso_Medium", "Dry_Meso_Medium"))

####################
#可视化
library(patchwork)
library(ggpubr)

#设定要两两之间比较的组合
my_55L = list(c("Dry_Micro_Control", "Dry_Micro_Low"), c("Dry_Micro_Control", "Dry_Micro_Medium"),c("Dry_Micro_Control", "Dry_Micro_High"), c("Dry_Micro_Low", "Dry_Micro_Medium"), c("Dry_Micro_Low", "Dry_Micro_High"), c("Dry_Micro_Medium", "Dry_Micro_High"))
my_50000L = list(c("Wet_Meso_Control", "Dry_Meso_Control"), c("Wet_Meso_Low", "Dry_Meso_Low"),c("Wet_Meso_Medium", "Dry_Meso_Medium"), c("Wet_Meso_Control", "Wet_Meso_Low"), c("Wet_Meso_Control", "Wet_Meso_Medium"), c("Wet_Meso_Low", "Wet_Meso_Medium"), c("Dry_Meso_Control", "Dry_Meso_Low"), c("Dry_Meso_Control", "Dry_Meso_Medium"), c("Dry_Meso_Low", "Dry_Meso_Medium"))

#规定颜色
group_colors_mesocosm <- c(
  "Wet_Meso_Control" = "#00ff00", # 浅红
  "Wet_Meso_Low" = "#1eb31e", # 浅绿
  "Wet_Meso_Medium" = "#3c673c",  # 浅蓝
  "Dry_Meso_Control" = "#00ffff", # 浅红
  "Dry_Meso_Low" = "#02bbff", # 浅绿
  "Dry_Meso_Medium" = "#0532ff"
)
group_colors_microcosm <- c(
  "Dry_Micro_Control" = "#ffc60a",
  "Dry_Micro_Low" = "#ff9107",
  "Dry_Micro_Medium" = "#ff2700",
  "Dry_Micro_High" = "#9d4d3e"
)

#使用Wilxocon检验，并绘图
p1 <- ggplot(data = plot_data_50000L, aes(x = Group, y = Simpson))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = 'Simpson', subtitle = '50000L Mesocosm', tags = 'a')+
  theme(axis.text.x=element_text(angle = 45,hjust = 1),axis.title.x = element_blank(), legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors_mesocosm) +
  # ylim(0.7, 1.0) +
  stat_compare_means(comparisons=my_50000L, text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)


p2 <- ggplot(data = plot_data_55L, aes(x = Group, y = Simpson))+ 
  geom_boxplot(aes(fill = Group), alpha = 1)+
  labs(y = 'Simpson', subtitle = '55L Microcosm', tags = 'b')+
  theme(axis.text.x=element_text(angle = 45,hjust = 1),axis.title.x = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5), plot.tag = element_text(face = "bold")) +
  scale_fill_manual(values = group_colors_microcosm) +
  # ylim(0.7, 1.0) +
  stat_compare_means(comparisons=my_55L, text=element_text(family="A",face="bold"), size=4, label ="p.signif", method = "wilcox.test", paired = FALSE)

p1+p2

###########################
# NMDS
###########################

# 读取NMDS数据文件
#只使用加入氢氧化镁之后的样品
df_50000L = read.delim("feature-table_10142_diversity_50000L.tsv", header = T, row.names = 1, check.names = FALSE)
df_55L = read.delim("feature-table_10142_diversity_55L.tsv", header = T, row.names = 1, check.names = FALSE)
df_50000L=t(df_50000L) # 对数据进行转置，如果想对基因分组则不用转置
df_55L=t(df_55L)

# 读取样本分组数据文件
dfGroup_50000L = read.delim("metadata_group_50000L.tsv",row.names = 1)
dfGroup_55L = read.delim("metadata_group_55L.tsv",row.names = 1)

set.seed(123)

# NMDS计算
dfNmds_50000L<-metaMDS(df_50000L,distance="bray",k = 2)
dfNmds_55L<-metaMDS(df_55L,distance="bray",k = 2)
#stress小于0.2才有解释意义，参考https://zhuanlan.zhihu.com/p/612688854

#PCoA和NMDS建议用这个，结果解读看https://zhuanlan.zhihu.com/p/111985434
otu.distance_50000L <- vegdist(df_50000L, method = 'bray')
otu.distance_55L <- vegdist(df_55L, method = 'bray')
anosim_result_50000L <- anosim(otu.distance_50000L, dfGroup_50000L$Group, permutations = 999)
anosim_result_55L <- anosim(otu.distance_55L, dfGroup_55L$Group, permutations = 999)
summary(anosim_result_50000L)
summary(anosim_result_55L)
anosim_result_subtitle_50000L <- paste0("ANOSIM statistic R: ",round(anosim_result_50000L$statistic[1],4),"     Significance: ",round(anosim_result_50000L$signif[1],4))  
anosim_result_subtitle_55L <- paste0("ANOSIM statistic R: ",round(anosim_result_55L$statistic[1],4),"     Significance: ",round(anosim_result_55L$signif[1],4)) 

# 绘图前的数据整理
data_50000L = data.frame(dfNmds_50000L$points)
data_55L = data.frame(dfNmds_55L$points)
data_50000L$Group = dfGroup_50000L$Group
data_55L$Group = dfGroup_55L$Group

#修改列名
names(data_50000L)[1:3] <- c('NMDS1', 'NMDS2', 'Group')
names(data_55L)[1:3] <- c('NMDS1', 'NMDS2', 'Group')
data_50000L$Group<-factor(data_50000L$Group, levels = c('Wet_Meso_Control', 'Wet_Meso_Low', 'Wet_Meso_Medium', 'Dry_Meso_Control', 'Dry_Meso_Low', 'Dry_Meso_Medium'))
data_55L$Group<-factor(data_55L$Group, levels = c('Dry_Micro_Control', 'Dry_Micro_Low', 'Dry_Micro_Medium', 'Dry_Micro_High'))
#stress小于0.2说明降维的轴有解释意义,R>0说明组内距离小于组间距离(即分组是有效的)

# 绘图
p3 <- ggplot(data_50000L,aes(x = NMDS1,
                          y = NMDS2,
                          color = Group,
                          group = Group,
                          fill = Group)
)+
  geom_point(size=1)+
  theme_classic()+
  stat_ellipse(             # 添加置信区间
    geom = "polygon",
    level = 0.95,
    alpha=0.3)+
  # geom_text(                # 添加文本标签
  #   aes(label=rownames(data)),
  #   vjust=1.5,
  #   size=3.5,
  #   color = "black"
  # )+
  labs(                     # 在副标题处添加stress
  #  title = "NMDS Analysis",
    #subtitle = paste("Stress=",round(dfNmds_50000L$stress,4),sep="","     ",anosim_result_subtitle_50000L),
    subtitle = paste(anosim_result_subtitle_50000L),
    x = "NMDS1",
    y = "NMDS2",
    tags = "c"
  )+
  scale_fill_manual(values = group_colors_mesocosm) +
  scale_color_manual(values = group_colors_mesocosm) +
  theme(plot.subtitle = element_text(size = 8.5), plot.tag = element_text(face = "bold"),
        legend.key.size = unit(9,"pt"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))     # 标题居中
#scale_fill_discrete(name="处理",
#                    breaks=c("Control", "Low", "Medium"),
#                    labels=c("对照组", "弱碱度增强组", "中等碱度增强组"))

p4 <- ggplot(data_55L,aes(x = NMDS1,
                            y = NMDS2,
                            color = Group,
                            group = Group,
                            fill = Group)
)+
  geom_point(size=1)+
  theme_classic()+
  stat_ellipse(             # 添加置信区间
    geom = "polygon",
    level = 0.95,
    alpha=0.2)+
  # geom_text(                # 添加文本标签
  #   aes(label=rownames(data)),
  #   vjust=1.5,
  #   size=3.5,
  #   color = "black"
  # )+
  labs(                     # 在副标题处添加stress
  #  title = "NMDS Analysis",
    #subtitle = paste("Stress=",round(dfNmds_55L$stress,4),sep="","     ",anosim_result_subtitle_55L),
    subtitle = paste(anosim_result_subtitle_55L),
    x = "NMDS1",
    y = "NMDS2",
    tags = "d"
  )+
  scale_fill_manual(values = group_colors_microcosm) +
  scale_color_manual(values = group_colors_microcosm) +
  theme(plot.subtitle = element_text(size = 8.5), plot.tag = element_text(face = "bold"),
        legend.key.size = unit(9,"pt"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))     # 标题居中
#scale_fill_discrete(name="处理",
#                    breaks=c("Control", "Low", "Medium"),
#                    labels=c("对照组", "弱碱度增强组", "中等碱度增强组"))

p3+p4

(p1/p3)|(p2/p4)
###########################
# Zeta
###########################

library(readxl)
library(tidyverse) # for data handling and manipulation
library(lubridate)
library(codyn) # a range of community dynamics analyses
library(vegan) # for a range of community analyses
library(zetadiv) # Zeta diversity
library(synchrony) # synchrony

#载入并预处理数据
#只使用加入氢氧化镁之后的样品
#setwd("D:/Files/CMEE/Mesocosms/论文用图/zeta_diversity_and_synchrony")
Marine_OTU_table_50000L <- read.delim("feature-table_10142_diversity_50000L.tsv", header=T, sep="\t", stringsAsFactors = FALSE,check.names=F)
Marine_wide_50000L <- t(read.delim("feature-table_10142_diversity_50000L.tsv", header=T, row.names = 1, sep="\t", stringsAsFactors = FALSE,check.names=F)) %>%
  as.data.frame(.)
Marine_group_50000L <- read.delim("metadata_group_50000L.tsv", header=T, sep="\t", stringsAsFactors = FALSE,check.names=F)

Marine_long_50000L <- gather(Marine_OTU_table_50000L, key = Sample, value = Abundance, -"ASV")
Marine_gg_50000L <- Marine_group_50000L

Seasons_50000L <- unique(Marine_gg_50000L$Group)

#开始计算zeta多样性
for(i in 1:6) { # loop through the seasons
  
  Marine_pa_50000L <- Marine_wide_50000L %>% mutate(Group = Marine_gg_50000L$Group)
  
  if(i == 1) { Marine_pa_50000L <- Marine_pa_50000L %>% filter(Group == Seasons_50000L[i]) %>% dplyr::select(-Group) }
  if(i == 2) { Marine_pa_50000L <- Marine_pa_50000L %>% filter(Group == Seasons_50000L[i]) %>% dplyr::select(-Group) }
  if(i == 3) { Marine_pa_50000L <- Marine_pa_50000L %>% filter(Group == Seasons_50000L[i]) %>% dplyr::select(-Group) }
  if(i == 4) { Marine_pa_50000L <- Marine_pa_50000L %>% filter(Group == Seasons_50000L[i]) %>% dplyr::select(-Group) }
  if(i == 5) { Marine_pa_50000L <- Marine_pa_50000L %>% filter(Group == Seasons_50000L[i]) %>% dplyr::select(-Group) }
  if(i == 6) { Marine_pa_50000L <- Marine_pa_50000L %>% filter(Group == Seasons_50000L[i]) %>% dplyr::select(-Group) }
  
  Marine_pa_50000L[Marine_pa_50000L > 0] <- 1 
  
  Marine_zeta_50000L <- data.frame(Group = NA, Order = (1:(dim(Marine_pa_50000L)[[1]]-1)), Zeta_diversity = NA, Zeta_diversity_SD = NA)
  
  for(z in 1:(dim(Marine_pa_50000L)[[1]]) - 1) {
    zeta.out.50000L <- Zeta.order.mc(Marine_pa_50000L, order = z, normalize = "Simpson")
    Marine_zeta_50000L$Group[z] <- Seasons_50000L[i]
    Marine_zeta_50000L$Order[z] <- zeta.out.50000L$zeta.order
    Marine_zeta_50000L$Zeta_diversity[z] <- zeta.out.50000L$zeta.val
    Marine_zeta_50000L$Zeta_diversity_SD[z] <- zeta.out.50000L$zeta.val.sd
    print(z)
  } # end z loop through zeta orders
  
  if(i == 1) { Marine_zeta_out_50000L <- Marine_zeta_50000L }
  if(i > 1) { Marine_zeta_out_50000L <- rbind(Marine_zeta_out_50000L, Marine_zeta_50000L) }
  
} # end i loop through the seasons

#计算完之后，保存为中间文件，手动添加分组（示例的中间文件已经给出）
write.table(Marine_zeta_out_50000L, file ="Marine_zeta_out_50000L.txt", sep ="\t", row.names =FALSE, col.names =TRUE, quote =FALSE)

summary(Marine_aov_50000L <- aov(Marine_zeta_out_50000L$Zeta_diversity ~ Marine_zeta_out_50000L$Group))
TukeyHSD(Marine_aov_50000L)

#加载处理好的中间文件，开始画图
Marine_zeta_out_50000L <- read.delim('Marine_zeta_out_50000L.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
Marine_zeta_out_50000L$Group <- factor(Marine_zeta_out_50000L$Group, levels = c('Wet_Meso_Control', 'Wet_Meso_Low', 'Wet_Meso_Medium', 'Dry_Meso_Control', 'Dry_Meso_Low', 'Dry_Meso_Medium'))

p5 <- Marine_zeta_out_50000L %>%
  dplyr::filter(Order > 1) %>%
  ggplot(aes(x = Order, y = Zeta_diversity)) +
  geom_ribbon(aes(ymin = (Zeta_diversity - Zeta_diversity_SD), ymax = (Zeta_diversity + Zeta_diversity_SD), fill = Group), alpha = 0.2) +
  geom_line(aes(colour = Group), size = 1.5, alpha = 0.8) +
  geom_point(aes(fill = Group), size = 2, pch = 21, colour = "black", alpha = 0.8) +
  xlab("Zeta order") +
  ylab("Zeta diversity") +
  labs(tag = "e", subtitle = "p-value > 0.05") +
  #theme_bw(base_size = 18) +
  scale_fill_manual(values = group_colors_mesocosm) +
  scale_color_manual(values = group_colors_mesocosm) +
  theme(plot.tag = element_text(face = "bold"),
        legend.key.size = unit(9,"pt"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))
p5


#开始检验另一组实验
Marine_OTU_table_55L <- read.delim("feature-table_10142_diversity_55L.tsv", header=T, sep="\t", stringsAsFactors = FALSE,check.names=F)
Marine_wide_55L <- t(read.delim("feature-table_10142_diversity_55L.tsv", header=T, row.names = 1, sep="\t", stringsAsFactors = FALSE,check.names=F)) %>%
  as.data.frame(.)
Marine_group_55L <- read.delim("metadata_group_55L.tsv", header=T, sep="\t", stringsAsFactors = FALSE,check.names=F)

Marine_long_55L <- gather(Marine_OTU_table_55L, key = Sample, value = Abundance, -"ASV")
Marine_gg_55L <- Marine_group_55L

Seasons_55L <- unique(Marine_gg_55L$Group)

for(i in 1:4) { # loop through the seasons
  
  Marine_pa_55L <- Marine_wide_55L %>% mutate(Group = Marine_gg_55L$Group)
  
  if(i == 1) { Marine_pa_55L <- Marine_pa_55L %>% filter(Group == Seasons_55L[i]) %>% dplyr::select(-Group) }
  if(i == 2) { Marine_pa_55L <- Marine_pa_55L %>% filter(Group == Seasons_55L[i]) %>% dplyr::select(-Group) }
  if(i == 3) { Marine_pa_55L <- Marine_pa_55L %>% filter(Group == Seasons_55L[i]) %>% dplyr::select(-Group) }
  if(i == 4) { Marine_pa_55L <- Marine_pa_55L %>% filter(Group == Seasons_55L[i]) %>% dplyr::select(-Group) }
  
  Marine_pa_55L[Marine_pa_55L > 0] <- 1 
  
  Marine_zeta_55L <- data.frame(Group = NA, Order = (1:(dim(Marine_pa_55L)[[1]]-1)), Zeta_diversity = NA, Zeta_diversity_SD = NA)
  
  for(z in 1:(dim(Marine_pa_55L)[[1]]) - 1) {
    zeta.out.55L <- Zeta.order.mc(Marine_pa_55L, order = z, normalize = "Simpson")
    Marine_zeta_55L$Group[z] <- Seasons_55L[i]
    Marine_zeta_55L$Order[z] <- zeta.out.55L$zeta.order
    Marine_zeta_55L$Zeta_diversity[z] <- zeta.out.55L$zeta.val
    Marine_zeta_55L$Zeta_diversity_SD[z] <- zeta.out.55L$zeta.val.sd
    print(z)
  } # end z loop through zeta orders
  
  if(i == 1) { Marine_zeta_out_55L <- Marine_zeta_55L }
  if(i > 1) { Marine_zeta_out_55L <- rbind(Marine_zeta_out_55L, Marine_zeta_55L) }
  
} # end i loop through the seasons

#计算完之后，保存为中间文件，手动添加分组（示例的中间文件已经给出）
write.table(Marine_zeta_out_55L, file ="Marine_zeta_out_55L.txt", sep ="\t", row.names =FALSE, col.names =TRUE, quote =FALSE)

#加载处理好的中间文件，开始画图
Marine_zeta_out_55L <- read.delim('Marine_zeta_out_55L.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
Marine_zeta_out_55L$Group <- factor(Marine_zeta_out_55L$Group, levels = c('Dry_Micro_Control', 'Dry_Micro_Low', 'Dry_Micro_Medium', 'Dry_Micro_High'))

summary(Marine_aov_55L <- aov(Marine_zeta_out_55L$Zeta_diversity ~ Marine_zeta_out_55L$Group))
TukeyHSD(Marine_aov_55L)

Marine_zeta_out_55L$Group<-factor(Marine_zeta_out_55L$Group, levels = c('Dry_Micro_Control', 'Dry_Micro_Low', 'Dry_Micro_Medium', 'Dry_Micro_High'))

p6 <- Marine_zeta_out_55L %>%
  dplyr::filter(Order > 1) %>%
  ggplot(aes(x = Order, y = Zeta_diversity)) +
  geom_ribbon(aes(ymin = (Zeta_diversity - Zeta_diversity_SD), ymax = (Zeta_diversity + Zeta_diversity_SD), fill = Group), alpha = 0.3) +
  geom_line(aes(colour = Group), size = 1.5, alpha = 0.8) +
  geom_point(aes(fill = Group), size = 2, pch = 21, colour = "black", alpha = 0.8) +
  xlab("Zeta order") +
  ylab("Zeta diversity") +
  labs(tag = "f", subtitle = "p-value > 0.05") +
  #theme_bw(base_size = 18) +
  scale_fill_manual(values = group_colors_microcosm) +
  scale_color_manual(values = group_colors_microcosm) +
  theme(plot.tag = element_text(face = "bold"),
        legend.key.size = unit(9,"pt"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))
p6

#绘制总图
#p_all <- ((p1/p3/p5)|(p2/p4/p6))+plot_layout(ncol  = 2, widths = c(1, 1))
p_all <- (p1|p2)/(p3|p4)/(p5|p6)
p_all
#保存为pdf，参考readme进行格式转换
