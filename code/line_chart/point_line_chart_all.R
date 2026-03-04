#https://www.jianshu.com/p/f3a71b12647e
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(reshape)
library(patchwork)
library(ggpubr)

wd="C:/Users/l1065/Documents/Voyage/Mesocosms/论文用图/environment"
setwd(wd)

#读取数据
#因为一部分数据有残缺，直接用一张表格画的话会导致有些点连不成线，所以用了几张表格
stat_Wet <- read.delim('DEF_standard.txt', sep = '\t', stringsAsFactors = T, check.names = FALSE)
stat_Wet$Group <- factor(stat_Wet$Group, levels = c("Wet_Meso_Control", "Wet_Meso_Low", "Wet_Meso_Medium"))

stat_Dry <- read.delim('Mesocosm_2023W_Data_standard.txt', sep = '\t', stringsAsFactors = T, check.names = FALSE)
stat_Dry$Group <- factor(stat_Dry$Group, levels = c("Dry_Meso_Control", "Dry_Meso_Low", "Dry_Meso_Medium"))
stat_Dry_carbon <- read.delim('Mesocosm_2023W_Data_standard_carbon.txt', sep = '\t', stringsAsFactors = T, check.names = FALSE)
stat_Dry_carbon$Group <- factor(stat_Dry_carbon$Group, levels = c("Dry_Meso_Control", "Dry_Meso_Low", "Dry_Meso_Medium"))
stat_Dry_DIC <- read.delim('Mesocosm_2023W_Data_standard_DIC.txt', sep = '\t', stringsAsFactors = T, check.names = FALSE)
stat_Dry_DIC$Group <- factor(stat_Dry_DIC$Group, levels = c("Dry_Meso_Control", "Dry_Meso_Low", "Dry_Meso_Medium"))

stat_55L <- read.delim('Data_55L_standard.txt', sep = '\t', stringsAsFactors = T, check.names = FALSE)
stat_55L$Group <- factor(stat_55L$Group, levels = c("Dry_Micro_Control", "Dry_Micro_Low", "Dry_Micro_Medium", "Dry_Micro_High"))
stat_55L_chlo <- read.delim('Data_55L_standard_chlorophyll_carbon.txt', sep = '\t', stringsAsFactors = T, check.names = FALSE)
stat_55L_chlo$Group <- factor(stat_55L_chlo$Group, levels = c("Dry_Micro_Control", "Dry_Micro_Low", "Dry_Micro_Medium", "Dry_Micro_High"))

group_colors_Wet <- c(
  "Wet_Meso_Control" = "#00ff00", # 浅红
  "Wet_Meso_Low" = "#1eb31e", # 浅绿
  "Wet_Meso_Medium" = "#3c673c"
)
group_colors_Dry <- c(
  "Dry_Meso_Control" = "#00ffff", # 浅红
  "Dry_Meso_Low" = "#02bbff", # 浅绿
  "Dry_Meso_Medium" = "#0532ff"
)
group_colors_55L <- c(
  "Dry_Micro_Control" = "#ffc60a",
  "Dry_Micro_Low" = "#ff9107",
  "Dry_Micro_Medium" = "#ff2700",
  "Dry_Micro_High" = "#9d4d3e"
)

Wet1 <- ggplot(data = stat_Wet, aes(x = Day, y = Salinity_normalised_TA,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  # scale_shape_manual(values = c(19,19,19))
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Wet) +
  labs(x = "Day", y = "Salinity normalised TA (µmol/kg)", subtitle = "Wet season mesocosm", tag = "A") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(2050,2650) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
Wet2 <- ggplot(data = stat_Wet, aes(x = Day, y = pH,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Wet) +
  labs(x = "Day", y = "pH", subtitle = "Wet season mesocosm", tag = "B")  +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(7.9,8.8) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
Wet3 <- ggplot(data = stat_Wet, aes(x = Day, y = Chla,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Wet) +
  ylim(0,300) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  labs(x = "Day", y = "Chlorophyll a (µg/L)", subtitle = "Wet season mesocosm", tag = "C")+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
        #, legend.justification="left"
Wet4 <- ggplot(data = stat_Wet, aes(x = Day, y = DO,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Wet) +
  ylim(5.0,8.3) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  labs(x = "Day", y = "Dissolved oxygen (mg/L)", subtitle = "Wet season mesocosm", tag = "D")+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))

Dry1 <- ggplot(data = stat_Dry, aes(x = Day, y = Salinity_normalised_TA,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Dry) +
  labs(x = "Day", y = "Salinity normalised TA (µmol/kg)", subtitle = "Dry season mesocosm", tag = "E") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  ylim(2050,2650) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
Dry2 <- ggplot(data = stat_Dry, aes(x = Day, y = pH,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Dry) +
  labs(x = "Day", y = "pH", subtitle = "Dry season mesocosm", tag = "F") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(7.9,8.8) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
Dry3 <- ggplot(data = stat_Dry, aes(x = Day, y = Chla,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Dry) +
  labs(x = "Day", y = "Chlorophyll a (µg/L)", subtitle = "Dry season mesocosm", tag = "G")+
  ylim(0,300) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
Dry4 <- ggplot(data = stat_Dry, aes(x = Day, y = DO,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Dry) +
  labs(x = "Day", y = "Dissolved oxygen (mg/L)", subtitle = "Dry season mesocosm", tag = "H")+
  ylim(5.0,8.3) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))

M1 <- ggplot(data = stat_55L, aes(x = Day, y = Salinity_normalised_TA,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_55L) +
  labs(x = "Day", y = "Salinity normalised TA (µmol/kg)", subtitle = "55L microcosm", tag = "I") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(2050,2650) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
M2 <- ggplot(data = stat_55L, aes(x = Day, y = pH,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_55L) +
  labs(x = "Day", y = "pH", subtitle = "55L microcosm", tag = "J") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(7.9,8.8) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
M3 <- ggplot(data = stat_55L_chlo, aes(x = Day, y = Chla,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_55L) +
  labs(x = "Day", y = "Chlorophyll a (µg/L)", subtitle = "55L microcosm", tag = "K")+
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(0,300) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
        #panel.grid.minor = element_line(colour = "white"))
M4 <- ggplot(data = stat_55L_chlo, aes(x = Day, y = DO,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_55L) +
  labs(x = "Day", y = "Dissolved oxygen (mg/L)", subtitle = "55L microcosm", tag = "L") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(5.0,8.3) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.justification="left"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))

#总图
p_all <- (Wet1/Wet2/Wet3/Wet4)|(Dry1/Dry2/Dry3/Dry4)|(M1/M2/M3/M4)
p_all


#附图
Wet5 <- ggplot(data = stat_Wet, aes(x = Day, y = pCO2,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Wet) +
  ylim(105,1745) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  labs(x = "Day", y = "pCO2 (μatm)", subtitle = "Wet season mesocosm", tag = "A")+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))
#, legend.justification="left"
Wet6 <- ggplot(data = stat_Wet, aes(x = Day, y = OmegaAragonite,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Wet) +
  ylim(1.2,7.0) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  labs(x = "Day", y = "OmegaAragonite", subtitle = "Wet season mesocosm", tag = "B")+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))
Wet7 <- ggplot(data = stat_Wet, aes(x = Day, y = DIC,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Wet) +
  ylim(1800,2400) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  labs(x = "Day", y = "DIC (µmol/kg)", subtitle = "Wet season mesocosm", tag = "C")+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))

Dry5 <- ggplot(data = stat_Dry_carbon, aes(x = Day, y = pCO2,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Dry) +
  labs(x = "Day", y = "pCO2 (μatm)", subtitle = "Dry season mesocosm", tag = "D")+
  ylim(105,1745) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))
Dry6 <- ggplot(data = stat_Dry_carbon, aes(x = Day, y = OmegaAragonite,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Dry) +
  labs(x = "Day", y = "OmegaAragonite", subtitle = "Dry season mesocosm", tag = "E")+
  ylim(1.2,7.0) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))
Dry7 <- ggplot(data = stat_Dry_DIC, aes(x = Day, y = DIC,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_Dry) +
  labs(x = "Day", y = "DIC (µmol/kg)", subtitle = "Dry season mesocosm", tag = "F")+
  ylim(1800,2400) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))

M5 <- ggplot(data = stat_55L_chlo, aes(x = Day, y = pCO2,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_55L) +
  labs(x = "Day", y = "pCO2 (μatm)", subtitle = "55L microcosm", tag = "G")+
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(105,1745) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))
M6 <- ggplot(data = stat_55L_chlo, aes(x = Day, y = OmegaAragonite,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_55L) +
  labs(x = "Day", y = "OmegaAragonite", subtitle = "55L microcosm", tag = "H") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(1.2,7.0) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.position = "none"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))
M7 <- ggplot(data = stat_55L_chlo, aes(x = Day, y = DIC,  color = Group, shape = Group)) + 
  geom_point(shape = 19, size = 2) + 
  geom_line(linewidth = 1, aes(group = Group)) + 
  scale_color_manual(values = group_colors_55L) +
  labs(x = "Day", y = "DIC (µmol/kg)", subtitle = "55L microcosm", tag = "I") +
  # theme(axis.text.x=element_text(angle = 45,hjust = 1))  +
  ylim(1800,2400) +
  scale_x_continuous(limits = c(0,35), breaks = c(0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))+
  geom_vline(xintercept = 2, linetype = "dashed",color = "black",linewidth = 0.75)+
  theme_bw(base_size = 8) +
  theme(plot.tag = element_text(size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5),legend.justification="left"
        ,panel.background = element_rect(fill = "#EAEAEA", colour = NA),
        panel.grid.major = element_line(colour = "white"))
#panel.grid.minor = element_line(colour = "white"))

p_sup <- (Wet5/Wet6/Wet7)|(Dry5/Dry6/Dry7)|(M5/M6/M7)
p_sup
