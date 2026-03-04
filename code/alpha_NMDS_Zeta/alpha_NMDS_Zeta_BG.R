################################
# Alpha diversity
################################

#Alpha多样性指数计算
library(vegan)
library(ggplot2)
library(data.table)
library(reshape2)

#设定工作目录
wd="D:/Files/CMEE/Mesocosms/论文用图/alpha_beta"
setwd(wd)

#载入数据
plot_data_BG <- read.delim("feature-table_10142_alpha_BG.tsv", header=T, sep="\t", row.names=1, stringsAsFactors = FALSE,check.names=F)
plot_data_BG$Group <- factor(plot_data_BG$Group,levels = c("Wet_Background", "Dry_Background"))

#可视化
library(patchwork)
library(ggpubr)

#设定要两两之间比较的组合
my_BG = list(c("Wet_Background", "Dry_Background"))

#设置颜色
group_colors_BG <- c(
  "Wet_Background" = "#9400ff",
  "Dry_Background" = "#ffc0ca"
)

#绘图
p1 <- ggplot(data = plot_data_BG, aes(x = Group, y = Simpson))+ 
  geom_point(aes(colour = Group), alpha = 1)+
  labs(y = 'Simpson', tag = 'a') +
  scale_colour_manual(values = group_colors_BG) +
  # ylim(0.7, 1.0) +
  stat_compare_means(comparisons=my_BG, text=element_text(family="A",face="bold"), size=3, label ="p.signif", method = "wilcox.test", paired = FALSE)+
  theme_bw(base_size = 9)+
  theme(axis.title.x = element_blank(), legend.position = "none", plot.tag = element_text(face = "bold"))
p1



################################
# Beta diversity
################################
set.seed(123)

# 读取NMDS数据文件
df_BG = read.delim("feature-table_10142_diversity_BG.tsv", header = T, row.names = 1, check.names = FALSE)
df_BG=t(df_BG)

# 读取样本分组数据文件
dfGroup_BG = read.delim("metadata_group_BG.tsv",row.names = 1)

# NMDS计算（二选一）为了复现本科毕设，选择这个
dfNmds_BG<-metaMDS(df_BG,distance="bray",k = 2)
#stress小于0.2才有解释意义，参考https://zhuanlan.zhihu.com/p/612688854

#PCoA和NMDS建议用这个，结果解读看https://zhuanlan.zhihu.com/p/111985434
otu.distance_BG <- vegdist(df_BG, method = 'bray')
anosim_result_BG <- anosim(otu.distance_BG, dfGroup_BG$Mesocosm, permutations = 999)
summary(anosim_result_BG)
anosim_result_subtitle_BG <- paste0("ANOSIM statistic R: ",round(anosim_result_BG$statistic[1],4),"     Significance: ",round(anosim_result_BG$signif[1],4)) 

# 绘图前的数据整理
data_BG = data.frame(dfNmds_BG$points)
data_BG$Microcosm = dfGroup_BG$Mesocosm

#修改列名
names(data_BG)[1:3] <- c('NMDS1', 'NMDS2', 'Group')
data_BG$Group<-factor(data_BG$Group, levels = c('Wet_Background', 'Dry_Background'))
#stress小于0.2说明降维的轴有解释意义,R>0说明组内距离小于组间距离(即分组是有效的)

# 绘图
p2 <- ggplot(data_BG,aes(x = NMDS1,
                          y = NMDS2,
                          color = Group,
                          group = Group,
                          fill = Group)
)+
  geom_point(size=2)+
  theme_classic()+
  # geom_text(                # 添加文本标签
  #   aes(label=rownames(data_BG)),
  #   vjust=1.5,
  #   size=3.5,
  #   color = "black"
  # )+
  labs(fill = "Group", tag = 'b',                     # 在副标题处添加stress
    #  title = "NMDS Analysis",
    #subtitle = paste("Stress=",round(dfNmds_BG$stress,4),sep="","     ",anosim_result_subtitle_BG),
    subtitle = paste(anosim_result_subtitle_BG),
    x = "NMDS1",
    y = "NMDS2"
  )+
  scale_fill_manual(values = group_colors_BG) +
  scale_color_manual(values = group_colors_BG) +# 标题居中
  theme_bw(base_size = 9)     +
  theme(plot.title = element_text(hjust = 0.5), plot.tag = element_text(face = "bold"))
#scale_fill_discrete(name="处理",
#                    breaks=c("Control", "Low", "Medium"),
#                    labels=c("对照组", "弱碱度增强组", "中等碱度增强组"))
p2



################################
# Zeta diversity
################################
library(readxl)
library(tidyverse) # for data handling and manipulation
library(lubridate)
library(codyn) # a range of community dynamics analyses
library(vegan) # for a range of community analyses
library(zetadiv) # Zeta diversity
library(synchrony) # synchrony

# setwd("D:/Files/CMEE/Mesocosms/论文用图/zeta_diversity_and_synchrony")

#载入并预处理数据
Marine_OTU_table_BG <- read.delim("feature-table_10142_diversity_BG.tsv", header=T, sep="\t", stringsAsFactors = FALSE,check.names=F)
Marine_wide_BG <- t(read.delim("feature-table_10142_diversity_BG.tsv", header=T, row.names = 1, sep="\t", stringsAsFactors = FALSE,check.names=F)) %>%
  as.data.frame(.)
Marine_group_BG <- read.delim("metadata_group_BG.tsv", header=T, sep="\t", stringsAsFactors = FALSE,check.names=F)

Marine_long_BG <- gather(Marine_OTU_table_BG, key = Sample, value = Abundance, -"ASV")
Marine_gg_BG <- Marine_group_BG

Seasons_BG <- unique(Marine_gg_BG$Group)


#开始计算zeta多样性
for(i in 1:2) { # loop through the seasons
  
  Marine_pa_BG <- Marine_wide_BG %>% mutate(Group = Marine_gg_BG$Group)
  
  if(i == 1) { Marine_pa_BG <- Marine_pa_BG %>% filter(Group == Seasons_BG[i]) %>% dplyr::select(-Group) }
  if(i == 2) { Marine_pa_BG <- Marine_pa_BG %>% filter(Group == Seasons_BG[i]) %>% dplyr::select(-Group) }
  
  Marine_pa_BG[Marine_pa_BG > 0] <- 1 
  
  Marine_zeta_BG <- data.frame(Group = NA, Order = (1:(dim(Marine_pa_BG)[[1]]-1)), Zeta_diversity = NA, Zeta_diversity_SD = NA)
  
  for(z in 1:(dim(Marine_pa_BG)[[1]]) - 1) {
    zeta.out.BG <- Zeta.order.mc(Marine_pa_BG, order = z, normalize = "Simpson")
    Marine_zeta_BG$Group[z] <- Seasons_BG[i]
    Marine_zeta_BG$Order[z] <- zeta.out.BG$zeta.order
    Marine_zeta_BG$Zeta_diversity[z] <- zeta.out.BG$zeta.val
    Marine_zeta_BG$Zeta_diversity_SD[z] <- zeta.out.BG$zeta.val.sd
    print(z)
  } # end z loop through zeta orders
  
  if(i == 1) { Marine_zeta_out_BG <- Marine_zeta_BG }
  if(i > 1) { Marine_zeta_out_BG <- rbind(Marine_zeta_out_BG, Marine_zeta_BG) }
  
} # end i loop through the seasons

#计算完之后，保存为中间文件，手动添加分组（示例的中间文件已经给出）
write.table(Marine_zeta_out_BG, file ="Marine_zeta_out_BG.txt", sep ="\t", row.names =FALSE, col.names =TRUE, quote =FALSE)

summary(Marine_aov_BG <- aov(Marine_zeta_out_BG$Zeta_diversity ~ Marine_zeta_out_BG$Group))
TukeyHSD(Marine_aov_BG)

#加载处理好的中间文件，开始画图
Marine_zeta_out_BG <- read.delim('Marine_zeta_out_BG.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
Marine_zeta_out_BG$Group <- factor(Marine_zeta_out_BG$Group, levels = c('Wet_Background', 'Dry_Background'))

p3 <- Marine_zeta_out_BG %>%
  dplyr::filter(Order > 1) %>%
  ggplot(aes(x = Order, y = Zeta_diversity)) +
  geom_ribbon(aes(ymin = (Zeta_diversity - Zeta_diversity_SD), ymax = (Zeta_diversity + Zeta_diversity_SD), fill = Group), alpha = 0.3) +
  geom_line(aes(colour = Group), size = 1.5, alpha = 0.8) +
  geom_point(aes(fill = Group), size = 2, pch = 21, colour = "black", alpha = 0.8) +
  xlab("Zeta order") +
  ylab("Zeta diversity") +
  labs(tag = 'c',subtitle = 'p-value > 0.05') +
  scale_colour_manual(values = group_colors_BG) +
  scale_fill_manual(values = group_colors_BG) +
  theme_bw(base_size = 12) +
  theme(plot.tag = element_text(face = "bold"))
p3

#绘制总图
p_all <- (p1|p2)/p3
p_all
#保存为pdf，参考readme进行格式转换
