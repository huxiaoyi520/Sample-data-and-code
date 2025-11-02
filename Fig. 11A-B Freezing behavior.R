####FC_training######
# ctrl+shift+c  可以快速注释

library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(FSA)
library(rstatix)
library(Rmisc)
library(dplyr)
library(lme4)
library(lmerTest)
library(afex)
library(emmeans)
library(readxl)


FC <- read_excel("I:/条件性恐惧/FC.xlsx")

colnames(FC)
FC$`不动时间百分比(%)`
FC$次数 <- as.factor(FC$次数)
FC$group <- as.factor(FC$group)
levels(FC$group)


FC$group_short <- str_extract(FC$group, "^(control|LPS)")
FC$subgroup <- ifelse(FC$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")
FC_long <- summarySE(FC, measurevar="不动时间百分比(%)", groupvars=c("group","次数"))
FC_long$subgroup<- ifelse(FC_long$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")


# # 双因素重复测量ANOVA
# # ids: 受试者编号
# # dv: 因变量 (weight)
# # within: 受试者内因素 (days)
# # between: 受试者间因素 (group)
# # mixed model: Inters. ~ group * Radius + (1|ids)  
model <- lmer(`不动时间百分比(%)` ~ group_short*subgroup * 次数 + (1 | id),
              data = FC)

# 模型摘要
summary(model)
# Type III ANOVA（F值、p值等）
anova(model)
# 可选：事后比较
emmeans(model, pairwise ~ group_short)
# 如果对 group 在各个 `不动时间百分比(%)` 下的差异有兴趣：
em <- emmeans(model, pairwise ~ group_short | 次数)
em
em <- emmeans(model, pairwise ~ 次数 | group_short)
em
# 从 emmeans 中提取均值和标准误差
em_means <- emmeans(model, ~ group_short*次数) %>%
  as.data.frame()

ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")


p1 <- ggplot(FC_long, 
             aes(x=次数, 
                 y=`不动时间百分比(%)`, 
                 color=interaction(group, subgroup), # # 创建组合分组
                 shape = interaction(group, subgroup),# # 创建组合分组
                 group=interaction(group, subgroup))) +
  geom_line() +
  geom_point(size = 2, stroke = 0.5) +# 调整点大小
  geom_errorbar(aes(ymin=`不动时间百分比(%)`-se, 
                    ymax=`不动时间百分比(%)`+se), 
                width=0.2) +
  labs(y = "Freezing (%)",x="Shock") +
  scale_shape_manual(values = c("control.Vehicle" = 16,     # 圆形
                                "LPS_TAT_GluA23Y.TAT_GluA23Y" = 17,   # 三角形
                                "control_TAT_GluA23Y.TAT_GluA23Y" = 18,         # 菱形
                                "LPS.Vehicle" = 15
  ),      # 方形
  #guide = "none"  # 隐藏形状图例
  name = "Group"
  ) +
  scale_color_manual(values = c("control.Vehicle" = "#5CB0C3",
                                "LPS_TAT_GluA23Y.TAT_GluA23Y" = "#f5c75b",
                                "LPS.Vehicle" = "#5CB0C3",
                                "control_TAT_GluA23Y.TAT_GluA23Y" = "#f5c75b"),
                     labels = c("Ctrl",
                                "LPS+TAT-GluA23Y",
                                "LPS",
                                "Ctrl+TAT-GluA23Y")#,  # 图例标签
                     #name = "Group"
  )+####图例名字的修改
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0),breaks = seq(0, 100, by = 20)) + 
  #######自定义x轴刻度（针对因子变量）
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),  # 指定要显示的刻度
                   labels = c("1", "2", "3", "4", "5")   # 对应的标签
  )+
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.key = element_blank(),  # 移除图例键背景
        legend.position = c(0.26, 0.7),   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用  其中 0 表示最左/下，1 表示最右/上。
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=12, color = "black",face = "bold"),
        axis.title.x = element_text(size=12, color = "black",face = "bold"),
        axis.title.y = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+
  # 可选：调整图例排列（如颜色和形状图例分两列）
  guides(
    color = guide_legend(order = 1)#,  # 先显示颜色图例
    #shape = guide_legend(order = 2)   # 再显示形状图例
  )
p1
ggsave("p1.pdf", p1 , width =100, height = 70, units = "mm")



####FC_test######

library(readxl)
FC_test <- read_excel("I:/条件性恐惧/FC_test.xlsx", sheet = "Sheet2")



ordercolors<-c("#FDDED7","#F5BE8F","#C1E0DB","#CCD376","#A28CC2","#8498AB","#5CB0C3","#f5c75b")

# ordercolors<-c("#2F2D54","#9193B4","#EAB080","#BD9AAD","#B31761")
# Process_comparisons <- list(c("control", "lps","min"))
# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(FC_test$group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(FC_test$`不动时间百分比(%)`,FC_test$group, shapiro.test)
# 输出结果
print(shapiro_results)
library(stringr)
FC_test$group_short <- str_extract(FC_test$group, "^(control|LPS)")

FC_test$subgroup <- ifelse(FC_test$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")

# 进行双因素方差分析
anova_result <- aov(FC_test$`不动时间百分比(%)` ~ group_short*subgroup, data = FC_test)
summary(anova_result)
##事后检验
TukeyHSD(anova_result)

kruskal_test <- kruskal.test(FC_test$`不动时间百分比(%)` ~ group, data = FC_test)
kruskal_test
dunnTest(FC_test$`不动时间百分比(%)` ~ group, 
         data = FC_test, method = "bonferroni")

# 使用 t_test 替代 wilcox_test（参数检验）
t_test_result <- FC_test %>%
  t_test(`不动时间百分比(%)` ~ group, comparisons = Process_comparisons) %>%
  adjust_pvalue(p.col = "p") %>%
  add_significance(p.col = "p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = ifelse(
      p.adj < 0.001,
      "p < 0.001",
      paste0("p = ", formatC(p.adj, format = "f", digits = 3))
    )
  )
t_test_result
# wilcox_test_result <- FC_test %>%
#   wilcox_test(`不动时间百分比(%)` ~ group,comparisons = Process_comparisons) %>%
#   adjust_pvalue(p.col = "p") %>%
#   add_significance(p.col = "p.adj") %>%
#   # 对所有数值列保留3位小数
#   mutate(
#     across(where(is.numeric), ~ round(., 3)),  # 关键修改点
#     annotation = ifelse(
#       p.adj < 0.001,
#       "p < 0.001",
#       paste0("p = ", formatC(p.adj, format = "f", digits = 3))  # 确保显示3位小数
#     )
#   )
# wilcox_test_result

FC_test_long <- summarySE(FC_test, measurevar="不动时间百分比(%)", groupvars=c("group"))
FC_test_long$subgroup <- ifelse(FC_test_long$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")
FC_test$subgroup <- ifelse(FC_test$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")


####此处是直方图
# Process_comparisons <- list(
#   c("control", "LPS"),
#   c("control_TAT_GluA23Y", "LPS"),
#   c("LPS_TAT_GluA23Y", "LPS")
# )

g1 <- ggplot(FC_test_long, aes(x = group, y = `不动时间百分比(%)`, 
                               fill = subgroup )) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = `不动时间百分比(%)` - se, ymax = `不动时间百分比(%)` + se), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_jitter(data = FC_test,aes(x = group, y = `不动时间百分比(%)`),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values =c("TAT_GluA23Y" = "#f5c75b", "Vehicle" = "#5CB0C3"))+
  labs(y="Freezing (%)",x=NULL) +
  scale_x_discrete(labels = c("control" = "Ctrl", ####\n  下一行显示的
                              "LPS_TAT_GluA23Y" = "LPS",
                              "control_TAT_GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  scale_y_continuous(labels = function(x) paste0("  ", x),  ####可使得y轴字符占据三个位置,两个空格占据一个字符。
                     limits = c(0, 100),
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 20)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.key.size = unit(3, "mm"),  # 单位可以是 "mm", "cm", "in" 等  控制legend的大小
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g1 <- g1+geom_signif(comparisons = Process_comparisons, 
                     annotations = t_test_result$annotation,
                     step_increase = 0.02,
                     textsize = 3,      # 调整此处数值以控制注释文本的大小
                     size = 0.3) # Adjust this value to control line thickness, default is 0.5

g1
ggsave("g1.pdf", g1, width = 70, height = 70, units = "mm")



