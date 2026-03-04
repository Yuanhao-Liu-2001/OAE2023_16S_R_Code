library(vegan)
library(ggplot2)
# doBy包用于分组统计
library(doBy)
# ggalt包用于绘制拟合曲线
library(ggalt)
# BiodiversityR包用于绘制Rank-abundance曲线
library(BiodiversityR)
library(RColorBrewer)
library(patchwork)

#导入数据
wd="C:/Users/l1065/Documents/Voyage/github/code/Rarefaction_Curve"
setwd(wd)

asv <- data.frame(read.delim('feature-table.biom.tsv', sep = '\t', row.names=1, stringsAsFactors = T, check.names = FALSE))
# 将ASVs数据转置
asv <- t(asv)

df_group <- data.frame(read.delim('sample-metadata.tsv', sep = '\t', stringsAsFactors = T, check.names = FALSE))

## 为了方便后续运行，先对需要用到的函数进行打包定义，即alpha_index函数与alpha_curves函数
## 定义alpha_index函数，计算多种Alpha多样性指数且返回结果至向量
# alpha_index()函数用于计算Alpha多样性指数，asv为代表性物种丰度表；method选项指定具体Alpha多样性指数，可选“richness”、“shannon”、“simpson”、“pielou”、“chao1”、“ace”、“gc”和“pd”等；若计算PD_whole_tree，还需指定进化树文件tree.nwk；base用于指定Shannon指数的对数底数，默认自然对数底数e，即exp(1)
alpha_index <- function(asv, method = 'richness', tree = NULL, base = exp(1)) {
  if (method == 'richness') result <- rowSums(asv > 0)
  else if (method == 'chao1') result <- estimateR(asv)[2, ]
  else if (method == 'ace') result <- estimateR(asv)[4, ]
  else if (method == 'shannon') result <- diversity(asv, index = 'shannon', base = base)
  else if (method == 'simpson') result <- diversity(asv, index = 'simpson')
  else if (method == 'pielou') result <- diversity(asv, index = 'shannon', base = base) / log(estimateR(asv)[1, ], base)
  else if (method == 'gc') result <- 1 - rowSums(asv == 1) / rowSums(asv)
  else if (method == 'pd' & !is.null(tree)) {
    pd <- pd(asv, tree, include.root = FALSE)
    result <- pd[ ,1]
    names(result) <- rownames(pd)
  }
  result
}

## 定义alpha_curves函数，根据抽样步长(step)，统计每个稀释梯度下的Alpha多样性指数且返回结果至列表
# alpha_curves函数套用了alpha_index函数，先稀释抽样ASV丰度表，再应用alpha_index()函数计算各个抽样深度下的Alpha多样性指数。其中，asv为代表性物种丰度表；step为抽样步长，例如指定step = 1000则以1000条序列为一个梯度节点抽取第1000条、第2000条、第3000条、第4000条…序列，直到达最大抽样深度为止；rare选项用于指定最大抽样深度，默认抽样样本总量为止；其余参数method、tree和base同alpha_index()函数一致
alpha_curves <- function(asv, step, method = 'richness', rare = NULL, tree = NULL, base = exp(1)) {
  asv_nrow <- nrow(asv)
  if (is.null(rare)) rare <- rowSums(asv) else rare <- rep(rare, asv_nrow)
  alpha_rare <- list()
  
  for (i in 1:asv_nrow) {
    step_num <- seq(0, rare[i], step)
    if (max(step_num) < rare[i]) step_num <- c(step_num, rare[i])
    
    alpha_rare_i <- NULL
    for (step_num_n in step_num) alpha_rare_i <- c(alpha_rare_i, alpha_index(asv = rrarefy(asv[i, ], step_num_n), method = method, tree = tree, base = base))
    names(alpha_rare_i) <- step_num
    alpha_rare <- c(alpha_rare, list(alpha_rare_i))
  }
  
  names(alpha_rare) <- rownames(asv)
  alpha_rare
}

# 指定step = 500，抽样深度默认样本序列总量，对数底数指定e
# “richness_curves”以列表类型存储统计结果
richness_curves <- alpha_curves(asv, step = 1000, method = 'richness')
# 转化为数据框类型，构建ggolot2作图文件
plot_richness <- data.frame()
for (i in names(richness_curves)) {
  richness_curves_i <- (richness_curves[[i]])
  richness_curves_i <- data.frame(rare = names(richness_curves_i), alpha = richness_curves_i, sample = i, stringsAsFactors = FALSE)
  plot_richness <- rbind(plot_richness, richness_curves_i)
}
rownames(plot_richness) <- NULL
plot_richness$rare <- as.numeric(plot_richness$rare)
plot_richness$alpha <- as.numeric(plot_richness$alpha)

#输出表格，手动添加group
write.csv(plot_richness, 'plot_richness.csv')
#添加完后，重新载入表格
plot_richness_group <- data.frame(read.delim('plot_richness.csv', sep = ',', row.names=1, stringsAsFactors = T, check.names = FALSE))
plot_richness_group$Group <- factor(plot_richness_group$Group,levels = c("Wet_Background", "Wet_Meso_Control", "Wet_Meso_Low", "Wet_Meso_Medium", "Dry_Background", "Dry_Meso_Control", "Dry_Meso_Low", "Dry_Meso_Medium", "Dry_Micro_Day0","Dry_Micro_Control", "Dry_Micro_Low", "Dry_Micro_Medium", "Dry_Micro_High"))

group_colors <- c(
  "Wet_Background" = "#9400ff",
  "Wet_Meso_Control" = "#00ff00",
  "Wet_Meso_Low" = "#1eb31e",
  "Wet_Meso_Medium" = "#3c673c",
  "Dry_Background" = "#ffc0ca",
  "Dry_Meso_Control" = "#00ffff",
  "Dry_Meso_Low" = "#02bbff",
  "Dry_Meso_Medium" = "#0532ff",
  "Dry_Micro_Day0" = "#fffb0d",
  "Dry_Micro_Control" = "#ffc60a",
  "Dry_Micro_Low" = "#ff9107",
  "Dry_Micro_Medium" = "#ff2700",
  "Dry_Micro_High" = "#9d4d3e"
)

# "#666666"

#绘图
p1 <- ggplot(plot_richness_group, aes(rare, alpha, group = sample, color = Group)) +
  geom_line() +
  labs(x = 'Number of reads', y = 'Number of ASVs', color = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  geom_vline(xintercept = min(rowSums(asv)), linetype = 2) +
  geom_vline(xintercept = 10142, linetype = 1) +
  scale_color_manual(values = group_colors)+
  scale_x_continuous(limits = c(0, 150000), breaks = seq(0, 150000, 30000), labels = as.character(seq(0, 150000, 30000)))+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
    )

p2 <- ggplot(plot_richness_group, aes(rare, alpha, group = sample, color = Group)) +
  geom_line() +
  labs(x = 'Number of reads', y = 'Number of ASVs', color = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  scale_color_manual(values = group_colors)+
  scale_x_continuous(limits = c(150000, 570000), breaks = seq(150000, 570000, 140000), labels = as.character(seq(150000, 570000, 140000)))+
  theme(
    plot.title = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    legend.key.height = unit(4,"pt"),
    legend.key.width = unit(8,"pt")
  )

p3 <- (p1|p2) + plot_layout(ncol  = 2, widths = c(3, 1))
