#https://www.jianshu.com/p/f3a71b12647e
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(reshape)

library(patchwork)
library(ggpubr)

wd="D:/Files/CMEE/Mesocosms/论文用图/barplot"
setwd(wd)

#读取数据
stat_BG_Summer <- read.delim('feature-table_10142_L6_BG_Summer.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
stat_BG_Winter <- read.delim('feature-table_10142_L6_BG_Winter.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
stat_DFE <- read.delim('feature-table_10142_L6_DFE.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
stat_GJH <- read.delim('feature-table_10142_L6_GJH.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
stat_55L <- read.delim('feature-table_10142_L6_55L.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

stat_long_BG_Summer <- melt(stat_BG_Summer,id=c("Taxonomy"))
stat_long_BG_Winter <- melt(stat_BG_Winter,id=c("Taxonomy"))
stat_long_DFE <- melt(stat_DFE,id=c("Taxonomy"))
stat_long_GJH <- melt(stat_GJH,id=c("Taxonomy"))
stat_long_55L <- melt(stat_55L,id=c("Taxonomy"))

names(stat_long_BG_Summer)[2:3] <- c('Sample', 'Relative_Abundance')
names(stat_long_BG_Winter)[2:3] <- c('Sample', 'Relative_Abundance')
names(stat_long_DFE)[2:3] <- c('Sample', 'Relative_Abundance')
names(stat_long_GJH)[2:3] <- c('Sample', 'Relative_Abundance')
names(stat_long_55L)[2:3] <- c('Sample', 'Relative_Abundance')

#将小数类型的相对丰度乘以 100 方便以百分比展示
stat_long_BG_Summer$Relative_Abundance <- stat_long_BG_Summer$Relative_Abundance * 100
stat_long_BG_Winter$Relative_Abundance <- stat_long_BG_Winter$Relative_Abundance * 100
stat_long_DFE$Relative_Abundance <- stat_long_DFE$Relative_Abundance * 100
stat_long_GJH$Relative_Abundance <- stat_long_GJH$Relative_Abundance * 100
stat_long_55L$Relative_Abundance <- stat_long_55L$Relative_Abundance * 100

stat_long_BG_Summer$Taxonomy <- factor(stat_long_BG_Summer$Taxonomy,levels = c("Candidatus_Actinomarina","PeM15","Unclassified_Saprospiraceae","Unclassified_Cryomorphaceae","Flavicella","Formosa","NS3a_marine_group","NS4_marine_group","NS5_marine_group","Robertkochia","Winogradskyella","Unclassified_Flavobacteriaceae","NS11-12_marine_group","Cyanobium_PCC-6307","Synechococcus_CC9902","Aestuariicoccus","HIMB11","Unclassified_Rhodobacteraceae","AEGEAN-169_marine_group","SAR11_clade_Ia","OM43_clade","Pseudoalteromonas","Vibrio","Alcanivorax","OM60(NOR5)_clade","Pseudohongiella","SAR86_clade","Others"))
stat_long_BG_Winter$Taxonomy <- factor(stat_long_BG_Winter$Taxonomy,levels = c("Candidatus_Actinomarina","PeM15","Unclassified_Saprospiraceae","Unclassified_Cryomorphaceae","Flavicella","Formosa","NS3a_marine_group","NS4_marine_group","NS5_marine_group","Robertkochia","Winogradskyella","Unclassified_Flavobacteriaceae","NS11-12_marine_group","Cyanobium_PCC-6307","Synechococcus_CC9902","Aestuariicoccus","HIMB11","Unclassified_Rhodobacteraceae","AEGEAN-169_marine_group","SAR11_clade_Ia","OM43_clade","Pseudoalteromonas","Vibrio","Alcanivorax","OM60(NOR5)_clade","Pseudohongiella","SAR86_clade","Others"))
stat_long_DFE$Taxonomy <- factor(stat_long_DFE$Taxonomy,levels = c("Candidatus_Actinomarina","PeM15","Unclassified_Saprospiraceae","Unclassified_Cryomorphaceae","Flavicella","Formosa","NS3a_marine_group","NS4_marine_group","NS5_marine_group","Robertkochia","Winogradskyella","Unclassified_Flavobacteriaceae","NS11-12_marine_group","Cyanobium_PCC-6307","Synechococcus_CC9902","Aestuariicoccus","HIMB11","Unclassified_Rhodobacteraceae","AEGEAN-169_marine_group","SAR11_clade_Ia","OM43_clade","Pseudoalteromonas","Vibrio","Alcanivorax","OM60(NOR5)_clade","Pseudohongiella","SAR86_clade","Others"))
stat_long_GJH$Taxonomy <- factor(stat_long_GJH$Taxonomy,levels = c("Candidatus_Actinomarina","PeM15","Unclassified_Saprospiraceae","Unclassified_Cryomorphaceae","Flavicella","Formosa","NS3a_marine_group","NS4_marine_group","NS5_marine_group","Robertkochia","Winogradskyella","Unclassified_Flavobacteriaceae","NS11-12_marine_group","Cyanobium_PCC-6307","Synechococcus_CC9902","Aestuariicoccus","HIMB11","Unclassified_Rhodobacteraceae","AEGEAN-169_marine_group","SAR11_clade_Ia","OM43_clade","Pseudoalteromonas","Vibrio","Alcanivorax","OM60(NOR5)_clade","Pseudohongiella","SAR86_clade","Others"))
stat_long_55L$Taxonomy <- factor(stat_long_55L$Taxonomy,levels = c("Candidatus_Actinomarina","PeM15","Unclassified_Saprospiraceae","Unclassified_Cryomorphaceae","Flavicella","Formosa","NS3a_marine_group","NS4_marine_group","NS5_marine_group","Robertkochia","Winogradskyella","Unclassified_Flavobacteriaceae","NS11-12_marine_group","Cyanobium_PCC-6307","Synechococcus_CC9902","Aestuariicoccus","HIMB11","Unclassified_Rhodobacteraceae","AEGEAN-169_marine_group","SAR11_clade_Ia","OM43_clade","Pseudoalteromonas","Vibrio","Alcanivorax","OM60(NOR5)_clade","Pseudohongiella","SAR86_clade","Others"))

colourCount = length(unique(stat_55L$Taxonomy))
getPalette = colorRampPalette(c("#880015","#7092BE","#B5E61D","#FF7F27",
                                "#FFF200","#ff2700","#3F48CC","#E3A5E3",
                                "#C3C3C3","#B97A57","#C8BFE7","#FFAEC9",
                                "#99D9EA","#00FF90","#22B14C","#FF00FF",
                                "#004080","#008080","#807000","#13A5E3",
                                "#7F00FF","#00FFFF","#FFBA70","#d2d329",
                                "#408000","#EFE4B0","#7F7F7F","#E6E6FA"))

                                
# "#408000","#880015","#ED1C24","#B5E61D","#FF7F27","#FFF200",
# "#00A2E8","#3F48CC","#A349A4","#C3C3C3","#B97A57","#22B14C",
# "#FFAEC9","#FFC90E","#EFE4B0","#99D9EA","#7092BE","#C8BFE7",
# "#00FF00","#FF00FF","#00FFFF","#807000","#008080","#400080",
# "#E6E6FA","#7F7F7F","#7F00FF","#F08080","#004080","#800080",
# "#FFBA70","#d2d329","#4CAF50","#907c7c","#13A5E3","#E3A5E3"

#利用geom_bar()绘制堆栈式条形图——'stack'
s1 <- ggplot(stat_long_BG_Summer[1:308,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  #scale_fill_brewer()+
  labs(y = 'Relative Abundance(%)', subtitle = "Wet season",tags = "a") +
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.position = 'none',plot.subtitle = element_text(hjust = 0.5),  axis.text.x=element_text(angle = 45,hjust = 1), plot.tag = element_text(face = "bold"))
#theme(axis.text.x=element_text(angle = 45,hjust = 1))
s2 <- ggplot(stat_long_BG_Winter[1:112,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(y = 'Relative Abundance(%)', subtitle = "Dry season",tags = "b") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+
  theme(axis.title.x = element_blank(),plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_text(angle = 45,hjust = 1), plot.tag = element_text(face = "bold"), legend.key.size = unit(8,"pt"))
#theme(axis.text.x=element_text(angle = 45,hjust = 1))
s_all <- (s1+s2)+plot_layout(ncol = 2, widths = c(11, 4))

p1 <- ggplot(stat_long_DFE[1:336,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(subtitle = "Control",tags = "a") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),legend.position = 'none', axis.title.y = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.tag = element_text(face = "bold"))
#theme(axis.text.x=element_text(angle = 45,hjust = 1), , plot.subtitle = element_text(hjust = 0.5))
p2 <- ggplot(stat_long_DFE[337:672,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(subtitle = "Low", y = "Relative Abundance(%)",tags = "b")+
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),legend.position = 'none', axis.title.x = element_blank(), axis.text.x = element_blank(), plot.tag = element_text(face = "bold"))
#theme(axis.text.x=element_text(angle = 45,hjust = 1))
p3 <- ggplot(stat_long_DFE[673:1008,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(subtitle = "Medium", x = "Day\n\nWet season mesocosm",tags = "c") +
  #scale_fill_brewer()+
  scale_fill_manual('Taxonomy', values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),legend.position = 'none', axis.title.y = element_blank(), plot.tag = element_text(face = "bold"))
p4 <- ggplot(stat_long_GJH[1:420,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(subtitle = "Control",tags = "d") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),legend.position = 'none', axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(), plot.tag = element_text(face = "bold"))
p5 <- ggplot(stat_long_GJH[421:840,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs( x = 'Day', subtitle = "Low",tags = "e") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), plot.tag = element_text(face = "bold"))
p6 <- ggplot(stat_long_GJH[841:1260,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs( x = 'Day\n\nDry season mesocosm', subtitle = "Medium",tags = "f") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),legend.position = 'none', axis.title.y = element_blank(), plot.tag = element_text(face = "bold"))
p7 <- ggplot(stat_long_55L[1:168,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(subtitle = "Control",tags = "g") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = 'none', axis.title.y = element_blank(), plot.tag = element_text(face = "bold"))
p8 <- ggplot(stat_long_55L[169:336,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(subtitle = "Low",tags = "h") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = 'none', axis.title.y = element_blank(), plot.tag = element_text(face = "bold"))
p9 <- ggplot(stat_long_55L[337:504,], aes
             (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs(subtitle = "Medium",tags = "i") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = 'none', axis.title.y = element_blank(), plot.tag = element_text(face = "bold"))
p10 <- ggplot(stat_long_55L[505:672,], aes
              (Sample, weight = Relative_Abundance, fill = Taxonomy)) +
  #geom_hline(yintercept = seq(25, 100, 25), color = 'gray') +
  geom_bar(color = "black", width = 0.8, position = 'stack') +
  labs( x = 'Day\n\n55L microcosm', subtitle = "High",tags = "j") +
  #scale_fill_brewer()+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()+theme_bw(base_size = 8) +
  theme(plot.subtitle = element_text(hjust = 0.5),axis.title.y = element_blank(), plot.tag = element_text(face = "bold"), legend.position = 'none')
#blank_plot <- ggplot(data.frame(), aes(x = 0, y = 0)) +theme_void() # 留白
#p1+((p2|blank_plot)/(p4|p3)/(p6|p5)/(p8|p7))+plot_layout(widths = c(1, 2))

#绘制总图
p_all <- ((p1/p2/p3)|(p4/p5/p6)|(p7/p8/p9/p10))+plot_layout(ncol = 3, widths = c(4, 5, 3))
