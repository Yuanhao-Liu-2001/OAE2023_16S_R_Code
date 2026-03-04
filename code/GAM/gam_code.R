#复刻文章方法：海州湾游泳动物群落物种多样性指数分布及其与环境因子的关系
library(mgcv)    # GAM分析
library(ggplot2) # 可视化
library(car)    # 共线性检验
library(dplyr)  # 数据清洗
library(gratia)  # 增强GAM可视化

wd="C:/Users/l1065/Documents/Voyage/Mesocosms/论文用图/correlation"
setwd(wd)

#载入数据，根据需要替换成其他的数据
#只使用加入氢氧化镁之后的样品
data <- data.frame(read.delim('GAM_data_wet.txt', sep = '\t', row.names=1, stringsAsFactors = T, check.names = FALSE))

# 全变量线性模型（仅用于VIF检验）
#！！！！！！在干燥季节不能把DIP加进去，因为DIP的浓度绝大部分都是0
lm_full <- lm(Simpson ~ pH + Temperature + Salinity + Chlorophyll_a + DO + Salinity_normalised_TA + DSi + NOx + DIP + DIC + pCO2 + OmegaAragonite, 
              data = data)

# 计算VIF（方差膨胀因子）
vif_results <- vif(lm_full)
# 打印VIF，将VIF输入附表4中
print(vif_results)

# 剔除VIF > 4的变量（即去除多重共线性参数）
vars_to_keep <- names(vif_results)[vif_results <= 4]
data_filtered <- data[, c("Simpson", vars_to_keep)]

# 初始模型（仅截距）
null_model <- gam(Simpson ~ 1, 
                  family = gaussian(link = "log"),  
                  data = data_filtered, 
                  method = "REML")
AIC(null_model)

# 手动选择AIC最小的模型，贡献率为Deviance explained
# 每次只往Simpson ~ 后面添加一个参数，先AIC最小的那个参数
# 然后再往后添加另一个参数，找出AIC最小的那个组合
# 一直找，知道AIC不再下降，此时的组合就是最佳模型

#例如，在GAM_data_wet.txt中，只有四个参数的VIF < 4：DSi、DIP、NOx和Chla
#为了看这四个参数对Simpson指数的影响，依次去尝试以下组合：
#Simpson ~ s(DSi)，Simpson ~ s(DIP)，Simpson ~ s(NOx)，Simpson ~ s(Chla)
#最后发现Simpson ~ s(DSi)的AIC值最小，因此保留DSi。随后，再往模型中添加其他参数，依次去尝试以下组合：
#Simpson ~ s(DSi) + s(DIP)、Simpson ~ s(DSi) + s(NOx)、Simpson ~ s(DSi) + s(Chla)
#最后发现Simpson ~ s(DSi) + s(DIP)的AIC值最小，因此保留DSi和DIP。
#再往模型中添加其他参数，依次去尝试以下组合：
#Simpson ~ s(DSi) + s(DIP) + s(NOx)，Simpson ~ s(DSi) + s(DIP) + s(Chla)
#最后发现Simpson ~ s(DSi) + s(DIP) + s(NOx)的AIC值最小，因此保留DSi、DIP和NOx
#再往模型中加上Chla，发现Simpson ~ s(DSi) + s(DIP) + s(NOx)的AIC值比Simpson ~ s(DSi) + s(DIP) + s(NOx) + s(Chla)小
#所以不往模型中加入Chla了（加入Chla反而会使得模型效果变差）
#因此Wet季节的最优模型就是Simpson ~ s(DSi) + s(DIP) + s(NOx)
select_model <- gam(Simpson ~ s(DSi) + s(DIP) + s(NOx),
                    family = tw(),
                    data = data_filtered, 
                    method = "REML")
AIC(select_model)

#如果要看单个变量的贡献率，就要通过拟合剔除单个变量的模型，比较方差解释量的变化，例如：
#最优模型select_model用的变量是Simpson ~ s(DSi) + s(DIP) + s(NOx)
#如果要看DSi单独的贡献率，就先把最优模型中的该变量去除：Simpson ~ s(DIP) + s(NOx)，这样得到一个模型select_model_without_single
#然后summary(select_model)$dev.expl - summary(select_model_without_single)$dev.expl
#即用最优模型的解释率减去去掉单变量之后的新模型的解释率，得到single_contribution
#这个single_contribution就是单个变量的解释率
#summary(select_model)$dev.expl则是最优模型的解释率
#将解释率输入附表4中
select_model_without_single <- gam(Simpson ~ s(DIP) + s(NOx), 
                                family = tw(),
                                data = data_filtered, 
                                method = "REML")
single_contribution <- summary(select_model)$dev.expl - summary(select_model_without_single)$dev.expl
print(paste("单独贡献率:", round(single_contribution * 100, 2), "%"))

#总结，查看最优模型中每个参数的p值
summary(select_model)
