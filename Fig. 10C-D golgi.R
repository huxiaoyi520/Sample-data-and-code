library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(FSA)
library(rstatix)
library(Rmisc)
library(readxl)

#############Total_dendritic_length#############

Total_dendritic_length <- read_excel("Total_dendritic_length.xlsx")

colnames(Total_dendritic_length)


Total_dendritic_length_samll <- Total_dendritic_length[,c("Sum Len [unit]",
                                                          "group"),]

ordercolors<-c("#FDDED7","#F5BE8F","#C1E0DB","#CCD376","#A28CC2","#8498AB","#5CB0C3","#f5c75b")

# ordercolors<-c("#2F2D54","#9193B4","#EAB080","#BD9AAD","#B31761")
# Process_comparisons <- list(c("control", "lps","min"))
# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(Total_dendritic_length_samll$group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####

Total_dendritic_length_samll$group <- as.factor(Total_dendritic_length_samll$group)
# 对每个 subgroup 进行正态性检验
shapiro_results <- by(Total_dendritic_length_samll$`Sum Len [unit]`,Total_dendritic_length_samll$group, shapiro.test)
# 输出结果

print(shapiro_results)

Total_dendritic_length_samll$group_short <- str_extract(Total_dendritic_length_samll$group, "^(control|LPS)")
Total_dendritic_length_samll$subgroup <- ifelse(Total_dendritic_length_samll$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")


# 进行双因素方差分析
anova_result <- aov(Total_dendritic_length_samll$`Sum Len [unit]` ~ group_short*subgroup, 
                    data = Total_dendritic_length_samll)
summary(anova_result)
##事后检验
TukeyHSD(anova_result)
kruskal_test <- kruskal.test(Total_dendritic_length_samll$`Sum Len [unit]` ~ group, data = Total_dendritic_length_samll)
kruskal_test
dunnTest(Total_dendritic_length_samll$`Sum Len [unit]` ~ group, 
         data = Total_dendritic_length_samll, method = "bonferroni")
# 使用 t_test 替代 wilcox_test（参数检验）
t_test_result <- Total_dendritic_length_samll %>%
  t_test(`Sum Len [unit]` ~ group, comparisons = Process_comparisons) %>%
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
# wilcox_test_result <- Total_dendritic_length_samll %>%
#   wilcox_test(`Sum Len [unit]` ~ group,comparisons = Process_comparisons) %>%
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


Total_dendritic_length_samll_long <- summarySE(Total_dendritic_length_samll, measurevar="Sum Len [unit]", groupvars=c("group"))

Total_dendritic_length_samll_long$subgroup <- ifelse(Total_dendritic_length_samll_long$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")

####此处是直方图
g1 <- ggplot(Total_dendritic_length_samll_long, aes(x = group, y = `Sum Len [unit]`, 
                                 fill = subgroup )) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = `Sum Len [unit]` - se, ymax = `Sum Len [unit]` + se), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_jitter(data = Total_dendritic_length_samll,aes(x = group, y = `Sum Len [unit]`),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = c("TAT_GluA23Y" = "#f5c75b", "Vehicle" = "#5CB0C3"))+
  labs(y="Total dendritic length (um)",x=NULL) +
  scale_x_discrete(labels = c("control" = "Ctrl", ####\n  下一行显示的
                              "LPS_TAT_GluA23Y" = "LPS",
                              "control_TAT_GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  scale_y_continuous(labels = function(x) paste0("  ", x),  ####可使得y轴字符占据三个位置,两个空格占据一个字符。
                     limits = c(0, 4000),
                     expand = c(0, 0),
                     breaks = seq(0, 4000, by = 1000)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.key.size = unit(5, "mm"),  # 单位可以是 "mm", "cm", "in" 等
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线
g1


g1 <- g1+geom_signif(comparisons = Process_comparisons, 
                     annotations = t_test_result$annotation,
                     step_increase = 0.05,
                     textsize = 3,      # 调整此处数值以控制注释文本的大小
                     size = 0.3) # Adjust this value to control line thickness, default is 0.5

g1
ggsave("g1.pdf", g1, width = 70, height = 70, units = "mm")


##########################################Number of intersections###########
# 
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(dplyr)
library(emmeans)
library(ggplot2)
library(afex)
library(emmeans)
library(readr)
combined_neuron1_profiles_with_group <- read_csv("combined_neuron1_profiles_with_group.csv")
combined_neuron1_profiles_with_group$ids <- rep(1:48, each = 30)

colnames(combined_neuron1_profiles_with_group)

unique(combined_neuron1_profiles_with_group$group)
table(combined_neuron1_profiles_with_group$group)

combined_neuron1_profiles_with_group$`Radius (Polyn. fit)` <- as.factor(combined_neuron1_profiles_with_group$`Radius (Polyn. fit)`)
combined_neuron1_profiles_with_group$group <- as.factor(combined_neuron1_profiles_with_group$group)


##########
normality_tests <- combined_neuron1_profiles_with_group %>%
  group_by(`Radius (Polyn. fit)`, group) %>%
  summarize(
    p_value = if (length(unique(Inters.)) > 1) {
      shapiro.test(Inters.)$p.value
    } else {
      NA_real_  # 返回NA，避免中断
    },
    .groups = "drop"  # 避免分组保留
  )
normality_tests

# 
# # 双因素重复测量ANOVA
# # ids: 受试者编号
# # dv: 因变量 (weight)
# # within: 受试者内因素 (days)
# # between: 受试者间因素 (group)
# # mixed model: Inters. ~ group * Radius + (1|ids)  

combined_neuron1_profiles_with_group$group_short <- str_extract(combined_neuron1_profiles_with_group$group,
                                                                "^(control|LPS)")
combined_neuron1_profiles_with_group$subgroup <- ifelse(combined_neuron1_profiles_with_group$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")

model <- lmer(`Inters.` ~ group_short * subgroup*`Radius (Polyn. fit)` + (1 | ids),
              data = combined_neuron1_profiles_with_group)
# 模型摘要
summary(model)
# Type III ANOVA（F值、p值等）
anova(model)
# 可选：事后比较
emmeans(model, pairwise ~ group_short)
# 如果对 group 在各个 `Radius (Polyn. fit)` 下的差异有兴趣：
em <- emmeans(model, pairwise ~ group_short | `Radius (Polyn. fit)`)
em
em <- emmeans(model, pairwise ~ `Radius (Polyn. fit)` | group_short)
em
# 从 emmeans 中提取均值和标准误差
em_means <- emmeans(model, ~ group_short*`Radius (Polyn. fit)`) %>%
  as.data.frame()




ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")

combined_neuron1_profiles_with_group$Radius_Polyn_fit <- combined_neuron1_profiles_with_group$`Radius (Polyn. fit)`
combined_neuron1_profiles_with_group_long <- summarySE(combined_neuron1_profiles_with_group, measurevar="Inters.", groupvars=c("group","Radius_Polyn_fit"))

combined_neuron1_profiles_with_group_long$subgroup <- ifelse(combined_neuron1_profiles_with_group_long$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")
combined_neuron1_profiles_with_group$subgroup <- ifelse(combined_neuron1_profiles_with_group$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")


p1 <- ggplot(combined_neuron1_profiles_with_group_long, 
             aes(x=Radius_Polyn_fit, 
                 y=Inters., 
                 color=interaction(group, subgroup), # # 创建组合分组
                 shape = interaction(group, subgroup),# # 创建组合分组
                 group=interaction(group, subgroup))) +
  geom_line() +
  geom_point(size = 2, stroke = 0.5) +# 调整点大小
  geom_errorbar(aes(ymin=Inters.-se, 
                    ymax=Inters.+se), 
                width=0.2) +
  labs(y = "Number of intersections",x="") +
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

  scale_y_continuous(limits = c(0, 15), expand = c(0, 0),breaks = seq(0, 15, by = 5)) + 
  #######自定义x轴刻度（针对因子变量）
  scale_x_discrete(breaks = c("0", "50", "100", "150", "200", "250", "300"),  # 指定要显示的刻度
                   labels = c("0", "50", "100", "150", "200", "250", "300")   # 对应的标签
                   )+
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.key = element_blank(),  # 移除图例键背景
        legend.position = c(0.26, 0.2),   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用  其中 0 表示最左/下，1 表示最右/上。
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
