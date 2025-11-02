
# LPS组和control组之间小鼠体重的比较 --------------------------------------------------

library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(dplyr)
library(emmeans)
library(ggplot2)
library(afex)
library(emmeans)
library(xlsx)
library(readxl)
weight_2025_10_28 <- read_excel("weight_2025_10_28.xlsx")

data <- weight_2025_10_28

# 确保days为因子
data$days <- as.factor(data$days)
data$group<- as.factor(data$group)
data$ids <- as.numeric(data$ids)

# 对于每个days与group组合，进行正态性检验
normality_tests <- data %>%
  group_by(days, group) %>%
  summarize(p_value = shapiro.test(weight)$p.value)

normality_tests

# 重复测量方差分析 ----------------------------------------------------------------
# # 双因素重复测量ANOVA
# # ids: 受试者编号
# # dv: 因变量 (weight)
# # within: 受试者内因素 (days)
# # between: 受试者间因素 (group)
res <- aov_ez(
  id = "ids",
  dv = "weight",
  data = data,
  within = "days",
  na.rm = TRUE,
  between = "group",
  anova_table = list(es = "pes")   ###获取偏η²效应量：
)
# 
# # 事后比较 (Post-hoc tests)
# # 如果对 group 在各个 days 下的差异有兴趣：
# em <- emmeans(res, pairwise ~ group | days)
# em
# # 查看每个days下不同group之间的比较及p值
# 
# # 如果对 days 在各个 group 下的差异有兴趣：
# em_days <- emmeans(res, pairwise ~ days | group)
# em_days


# 混合效应线性模型 ----------------------------------------------------------------

library(lme4)
library(lmerTest)
library(car)

# 1️⃣ 拟合线性混合模型
model <- lmer(weight ~ group * days + (1 | ids), data = data)

summary(model)
#   结果的解释
# 二、随机效应（Random effects）
# Groups   Name        Variance Std.Dev.
# ids      (Intercept) 0.02602  0.1613  
# Residual             1.38184  1.1755  
# 
# 
# 小鼠间的差异（随机截距）的标准差约为 0.161，很小
# 
# 体重测量的残差标准差为 1.176
# 
# 📌 解释：
# 
# 小鼠之间的基础体重差异较小，而日内（或测量误差）变异较大。
# 
# 这表明：大部分体重变化是由时间、组别或它们交互造成的，而不是个体本身。
# 
# 三、固定效应（Fixed effects）
# 项	含义	Estimate	p 值	解读
# (Intercept)	Control组第1天的平均体重	24.07	<0.001	Control 组基线体重约 24 g
# grouplps	LPS组相对于Control组在第1天的差异	-0.41	0.44	基线差异不显著
# daysD2–D7	Control组第2~7天相对第1天的变化	均非显著		Control体重变化不明显
# group×days（交互）	LPS组在各天相对Control的额外变化	D2–D5显著下降	p < 0.05	LPS组在D2–D5体重显著低于Control

# 
# 这些显著的交互项是你结果的关键发现：
# 
# 交互项	Estimate	t	p	含义
# groupLPS:daysD2	-1.59	-2.14	0.035	LPS组在第2天体重比Control低1.6g
# groupLPS:daysD3	-2.05	-2.76	0.0068	LPS组在第3天体重显著更低
# groupLPS:daysD4	-2.00	-2.69	0.0083	第4天仍显著更低
# groupLPS:daysD5	-1.64	-2.21	0.030	第5天仍显著更低
# groupLPS:daysD6–D7	ns			差异消失（体重开始恢复）

####输出 group、days、group×days 的 F、df、p。
anova(model)


library(emmeans)
emm <- emmeans(model, pairwise ~ group | days)
plot(emm)


library(effectsize)
eta_squared(model, partial = TRUE)



# 从 emmeans 中提取均值和标准误差
em_means <- emmeans(res, ~ group*days) %>%
  as.data.frame()


ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")

library(ggplot2)
p1 <- ggplot(em_means, aes(x=days, y=emmean, color=group, group=group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2) +
  labs(x = "Time (day)", y = "Weight (g)") +
  scale_color_manual(values = ordercolors,####color=group  所以这个地方使用scale_color_manual而不能使用scale_fill_manual()
                     breaks = c("control", "lps", "LPS+Mino"),
                     labels = c("Ctrl", "LPS", "LPS+Mino")) +     ###更改图例标签显示 
  scale_x_discrete(labels = c("D1" = "1", ####\n  下一行显示的
                              "D2" = "2",
                              "D3" = "3",
                              "D4" = "4",
                              "D5" = "5",
                              "D6" = "6",
                              "D7" = "7"))+
  scale_y_continuous(limits = c(15, 30), expand = c(0, 0),breaks = seq(15, 30, by = 5)) + 
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = c(0.2, 0.2),   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用  其中 0 表示最左/下，1 表示最右/上。
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=12, color = "black",face = "bold"),
        axis.title.x = element_text(size=12, color = "black",face = "bold"),
        axis.title.y = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())
p1


ggsave("weight1.pdf", p1 , width =60, height = 60, units = "mm")
