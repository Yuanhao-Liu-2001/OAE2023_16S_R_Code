# OAE2023_16S_R_Code
此项目中记录着文章“Resilience to Alkalinity Perturbations Reveals Ecosystem Stability under Ocean Alkalinity Enhancement”中绘制所有图片的代码，以及修改文章过程中使用的endnote的library与style文件

1. endnote
在endnote文件夹中，Mesocosm_2023_Summer.enlx为文章撰写修改过程中使用的library
OLAR.ens文件是参考文献模板（只有journal article调整好了，其他类型的参考文献还需要手动修改）
journal_list.txt文件记录了参考文献中使用的期刊缩写

2. code
！！！！！只要涉及到差异检验的分析/绘图，都只使用了加入氢氧化镁之后的样品数据进行差异检验。
2.1 alpha_NMDS_Zeta：绘制alpha多样性、beta多样性（NMDS分析）、Zeta多样性的图片。对应文中的Fig. 4（alpha_NMDS_Zeta_Cosm.R）和Fig. S6（alpha_NMDS_Zeta_BG.R）。只使用加入氢氧化镁之后的样品。
2.2 Barplot：绘制群落组成的柱状图。对应文中的Fig. 2（Barplot_L2.R）、Fig.3（Barplot_L6.R）、Fig. S3（Barplot_L2.R）和Fig. S5（Barplot_L6.R）
2.3 CLR_correlationship：绘制蓝藻相对丰度和叶绿素浓度相关性的图片。对应文中的Fig. S4
2.4 faprotax：绘制群落功能预测的箱线图/热图。对应文中的Fig. 5（faprotax_boxplot.R）和Fig. S7（faprotax_heatmap.R）。只使用加入氢氧化镁之后的样品。
2.5 GAM：广义加性模型分析，不是绘图而是分析用的代码。对应文中的Table S4。只使用加入氢氧化镁之后的样品。
2.6 line_chart：绘制折线图，用于展示环境参数变化。对应文中的Fig. 1和Fig. S2
2.7 Rarefaction_Curve：绘制稀疏曲线。对应文中的Fig. S1
每个代码具体的用法详见代码内部的备注
图片绘制好后使用pdf输出图片，随后使用其他软件（比如Photoshop）转化为jpg图，图像宽度不超过2100像素，分辨率300dpi
如果代码直接输出出来的图片和结果图不一样，那就是在输出为pdf后又手动编辑过pdf图

3. Others
其他可能有用的文件：
3.1 12-count：群落组成表，包括各个分类水平的，既有百分比形式的也有ASV数形式的
3.2 FAPROTAX_1.2.12：群落功能预测结果，也包含每种功能所对应的ASV的分表
3.3 dna-sequences.fasta：ASV的序列文件
3.4 feature-table.biom.no.cp.tsv：经合并预处理后的原始特征表
3.5 feature-table_10142.tsv：在3.3基础上，去除了ASV数小于10000的样品，并将样本重采样至同一测序深度的特征表（几乎所有分析都是基于这张表格）
3.6 taxonomy.tsv：ASV的分类表
