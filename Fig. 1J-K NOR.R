
# TRAINING ----------------------------------------------------------------


library(rstatix)
library(Rmisc)
library(readxl)
library(stringr)
NOR_training_2025_6_4 <- read_excel("NOR_training_2025_6_4.xlsx", 
                                             sheet = "two_group")


NOR_training_2025_6_4$Discrimination_index_1 <- NOR_training_2025_6_4$Discrimination_index_1*100
NOR_training_2025_6_4$Discrimination_index_2 <- NOR_training_2025_6_4$Discrimination_index_2*100
min(NOR_training_2025_6_4$Discrimination_index_1)
max(NOR_training_2025_6_4$Discrimination_index_2)

NOR_training_2025_6_4_long <- gather(NOR_training_2025_6_4[,c("group","Discrimination_index_1","Discrimination_index_2")],
                                     key = "index1_index2", 
                                     value = "value",
                                     -`group`)
NOR_training_2025_6_4_long$subgroup <-paste(NOR_training_2025_6_4_long$group, NOR_training_2025_6_4_long$index1_index2, sep = "_")
unique(NOR_training_2025_6_4_long$subgroup)


# 对每个 subgroup 进行正态性检验
shapiro_results <- by(NOR_training_2025_6_4_long$value, NOR_training_2025_6_4_long$subgroup, shapiro.test)
# 输出结果
print(shapiro_results)

# kruskal.test(value~subgroup,data = NOR_training_2025_6_4_long)
# 
# # 事后进行Dunn's Test
# dunnTest(value~subgroup,data = NOR_training_2025_6_4_long, method = "bonferroni")

NOR_training_2025_6_4_long_result <- summarySE(NOR_training_2025_6_4_long, measurevar="value", groupvars=c("group","subgroup"))

# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(NOR_training_2025_6_4_long$subgroup), 2, simplify = FALSE)
Process_comparisons <- combinations  ####
# 手动指定 subgroup 的因子顺序
NOR_training_2025_6_4_long_result$subgroup <- factor(
  NOR_training_2025_6_4_long_result$subgroup,
  levels = c(
    "control_Discrimination_index_1",
    "control_Discrimination_index_2",
    "LPS_Discrimination_index_1",
    "LPS_Discrimination_index_2"
  )
)


NOR_training_2025_6_4_long$subgroup_short <- as.numeric(str_extract(NOR_training_2025_6_4_long$subgroup, "\\d+$"))


NOR_training_2025_6_4_long$group <- factor(NOR_training_2025_6_4_long$group,
                                           levels = c("control","LPS"))
NOR_training_2025_6_4_long$subgroup_short <- factor(NOR_training_2025_6_4_long$subgroup_short,
                                           levels = c("1","2"),labels = c("A","B"))


# 进行双因素方差分析
anova_result <- aov(NOR_training_2025_6_4_long$value ~ group*subgroup_short, data = NOR_training_2025_6_4_long)
summary(anova_result)
##事后检验
TukeyHSD(anova_result)

# 
# wilcox_test_result <- NOR_training_2025_6_4_long %>%
#   wilcox_test(value ~ subgroup,comparisons = Process_comparisons) %>%
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
# 
# t_test_result <- NOR_training_2025_6_4_long %>%
#   t_test(value ~ subgroup, comparisons = Process_comparisons) %>%
#   adjust_pvalue(p.col = "p") %>%
#   add_significance(p.col = "p.adj") %>%
#   mutate(
#     across(where(is.numeric), ~ round(., 3)),
#     annotation = ifelse(
#       p.adj < 0.001,
#       "p < 0.001",
#       paste0("p = ", formatC(p.adj, format = "f", digits = 3))
#     )
#   )
# t_test_result


library(ggplot2)
library(ggsignif)
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
####此处是直方图
g1 <- ggplot(NOR_training_2025_6_4_long_result, aes(x = subgroup, y = value, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = NOR_training_2025_6_4_long,aes(x = subgroup, y = value),
              shape =21, alpha = 0.9)+#,
  #scale_fill_manual(values = c("Vehicle" = "#CCD376", "CNO" = "#A28CC2"))+
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  labs(y="Exploration time (%)",x=NULL) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 20)) +   ####expand = c(0, 0)  xy的0点重合
  scale_x_discrete(labels = c("control_Discrimination_index_1" = "Ctrl", ####\n  下一行显示的
                              "control_Discrimination_index_2" = "Ctrl",
                              "LPS_Discrimination_index_1" = "LPS",
                              "LPS_Discrimination_index_2" = "LPS"
  ))+
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  #####控制标题居中，并且是粗体
  ggtitle("Training")   ####添加标题
g1
g1 <- g1+geom_signif(comparisons = Process_comparisons, 
                         annotations = t_test_result$annotation,
                         step_increase = 0.1,
                         textsize = 3,      # 调整此处数值以控制注释文本的大小
                         size = 0.3) # Adjust this value to control line thickness, default is 0.5
g1



############################TEST#####################################################

library(readxl)
NOR_2025_10_28 <- read_excel("NOR_2025_10_28.xlsx")
data <- NOR_2025_10_28


# 对每个 subgroup 进行正态性检验
shapiro_results <- by(data$value, data$group, shapiro.test)
# 输出结果
print(shapiro_results)


t_test_result <- data %>%
  t_test(value ~ group, comparisons = Process_comparisons) %>%
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



data_long <- summarySE(data,
                       measurevar="value",
                       groupvars=c("group"))


Process_comparisons <- list(c("control", "lps"))
g2 <- ggplot(data_long, aes(x = group, y = value, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = data,aes(x = group, y = value),
              shape =21, alpha = 0.9)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Discrimination index (%)",x=NULL) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 20)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  #####控制标题居中，并且是粗体
  ggtitle("Testing")   ####添加标题

g2 <- g2+geom_signif(comparisons = Process_comparisons, 
                     annotations = t_test_result$annotation,
                     step_increase = 0.1,
                     textsize = 3,      # 调整此处数值以控制注释文本的大小
                     size = 0.3) # Adjust this value to control line thickness, default is 0.5
g2


# geom_signif(comparisons = Process_comparisons,
#             map_signif_level = FALSE,  # 显示具体的 p 值而不是星号
#             textsize = 3.5,  # 设置 p 值大小
#             vjust = 0.01, # 调整 p 值与线条的垂直距离
#             tip_length = 0.04,  # 控制线条末端长度
#             size = 0.5,# 控制线条粗细
#             data = NOR_test_long_combined_result)


g2

