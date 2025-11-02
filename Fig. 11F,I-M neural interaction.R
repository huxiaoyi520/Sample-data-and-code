#################################LDA##############################
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(dplyr)
library(ggpubr)  # 用于统计检验和添加显著性标记
library(rstatix) # 用于Wilcoxon检验
library(readxl)
fc_accuracy <- read_excel("fc_accuracy.xlsx", 
                          sheet = "LDA", col_types = c("text", 
                                                       "numeric", "text", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "text", "numeric"))
# 定义要比较的方法组合
method_comparisons <- list(
  c("Original", "Mean"),
  c("Original", "Cov1"),
  c("Mean", "Cov1")
)



fc_accuracy$subgroup <- factor(fc_accuracy$subgroup,levels = c("Original", "Mean" , "Cov1"))


# 构造示例数据（修改组别名称）
set.seed(123)
fc_accuracy <- fc_accuracy %>%
  arrange(group, id, subgroup) %>%    # 按组别、ID和方法排序
  mutate(
    Accuracy_shifted = Accuracy - 0.5,   # 将数据以0.7为中心偏移
    group_num = as.numeric(factor(group, levels = c("Ctrl", "Ctrl+TAT-GluA23Y","LPS" , "LPS+TAT-GluA23Y"))),  # 将组别转换为数值
    subgroup_offset = case_when(       
      subgroup == "Original" ~ -0.25,
      subgroup == "Mean" ~ 0,
      subgroup == "Cov1" ~ 0.25
    ),              # 不同方法的x轴偏移量  控制original  mean  Cov点在各个直方图上
    xval = group_num + subgroup_offset,    # 最终x轴位置
    # 新增处理条件列
    treatment = ifelse(grepl("\\+TAT-GluA23Y", group), "TAT-GluA23Y", "Vehicle")
  )

treatment_colors <- c("Vehicle" = "#5CB0C3", "TAT-GluA23Y" = "#f5c75b")  # 处理条件颜色

subgroup_patterns <- c("Original" = "none", "Mean" = "stripe", "Cov1" = "crosshatch")  # 方法图案
subgroup_shapes <- c("Original" = 21, "Mean" = 22, "Cov1" = 24) # 方法形状


# 汇总数据
summary_df <- fc_accuracy %>%
  group_by(group, subgroup,treatment) %>%
  summarise(
    mean = mean(Accuracy_shifted),
    se = sd(Accuracy_shifted)/sqrt(n()),
    .groups = "drop"
  )
summary_df


shapiro_results <- fc_accuracy %>%
  group_by(group, subgroup) %>%
  filter(n() >= 3) %>%  # 确保每组至少有3个观测值
  summarise(
    shapiro_p = shapiro.test(Accuracy)$p.value,
    shapiro_W = shapiro.test(Accuracy)$statistic,
    n = n(),
    .groups = 'drop'
  )
shapiro_results

fc_accuracy$group_short <- str_extract(fc_accuracy$group, "^(Ctrl|LPS)")

# 可用混合效应线性模型进行分析
library(lme4)      # fit mixed models
library(lmerTest)  # 给 lmer 提供 p 值
library(emmeans)   # 做 post-hoc
lmm_res <- lmer(Accuracy ~ group_short * treatment * subgroup + (1+ subgroup|id), data = fc_accuracy)

anova_res <- anova(lmm_res)
anova_res
summary(lmm_res)
emm <- emmeans(lmm_res, ~ subgroup | group_short * treatment)

# 查看 LPS + TAT-GluA23Y 条件下模型两两比较
# 两两比较，只在 LPS × TAT-GluA23Y 条件下
pairs(emm, by = c("group_short", "treatment"), adjust = "bonferroni")



# 对每个组别分别进行方法比较
wilcox_results <- fc_accuracy %>%
  group_by(group) %>%
  wilcox_test(Accuracy ~ subgroup, comparisons = method_comparisons) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = ifelse(
      p.adj < 0.001, "***",
      ifelse(p.adj < 0.01, "**",
             ifelse(p.adj < 0.05, "*", "ns"))))
wilcox_results


min(fc_accuracy$Accuracy_shifted)
max(fc_accuracy$Accuracy_shifted)
g1 <- ggplot() +
  # 1. 条形图（按处理条件着色）
  geom_bar_pattern(
    data = summary_df,
    aes(x = group, y = mean, fill = treatment, pattern = subgroup,
        pattern_fill = after_scale(fill),
        pattern_color = after_scale(fill)),
    stat = "identity",
    alpha = 0.8,
    position = position_dodge(width = 0.8),
    width = 0.7,  # 条形宽度
    #color = "black",   # 条形边框颜色
    # pattern_fill = after_scale(fill),  # 使用与填充色相同的颜色,#"gray", # 图案颜色
    # pattern_color = after_scale(fill),  # 使用与填充色相同的颜色,#"gray",
    pattern_angle = 45,  # 图案角度
    pattern_density = 0.3,  # 图案密度
    pattern_spacing = 0.02   # 图案间距
  ) +
  
  # 2. 误差棒
  geom_errorbar(
    data = summary_df,
    aes(x = group, ymin = mean - se, ymax = mean + se, group = subgroup),
    position = position_dodge(width = 0.8),
    width = 0.2,   # 误差棒宽度
    color = "black"  # 误差棒颜色
  ) +
  
  # 3. 数据点（按处理条件着色，形状按方法）
  geom_point(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        shape = subgroup,    # 形状表示方法
        color = treatment, # 颜色表示处理条件
        fill = treatment),  # 填充色表示处理条件
    #shape =21,
    alpha=0.6,  # 设置透明度(在aes外部)
    size = 0.5,  # 点大小
    ## fill = "white",
    stroke = 1.2   # 边框粗细
  ) +
  
  # 4. 配对连线（按处理条件着色）
  geom_line(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        group = interaction(group, id), # 按组和id分组
        color = treatment),  # 颜色按处理条件
    linewidth = 0.5,  # 线宽
    alpha = 0.6
  ) +
  
  # 5. 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # 坐标轴设置
  scale_y_continuous(
    name = "Accuracy",  # 轴标题
    breaks = seq(-0.1, 0.5, 0.1),  # 刻度位置
    labels = function(x) format(round(x + 0.5, 1), nsmall = 1),  # 刻度标签(还原偏移)
    limits = c(-0.1, 0.5),  # 轴范围
    expand = c(0, 0)  # 无扩展
  ) +
  scale_x_discrete(labels = c("Ctrl" = "Ctrl", ####\n  下一行显示的
                              "LPS+TAT-GluA23Y" = "LPS",
                              "Ctrl+TAT-GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  # 颜色与图案设置
  scale_fill_manual(values = treatment_colors) +
  scale_color_manual(values = treatment_colors) +
  scale_pattern_manual(values = subgroup_patterns) +
  scale_shape_manual(values = subgroup_shapes) +
  
  # 图例与主题
  labs(
    x = NULL,  # x轴标题(无)
    fill = "Treatment",   # 填充图例标题
    color = "Treatment",  # 颜色图例标题
    shape = "subgroup",   # 形状图例标题
    pattern = "subgroup",  # 图案图例标题
    title = "LDA"
  ) +
  theme_bw() +   # 使用黑白主题
  theme(panel.grid = element_blank(),  ####去除网格线
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 10, color = "black", face = "bold"),
        legend.key.size = unit(3, "mm"),  # 单位可以是 "mm", "cm", "in" 等  控制legend的大小
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线 
  # 图例调整
  guides(
    fill = guide_legend(order = 1, # 第一顺序
                        override.aes = list(pattern = "none"),# 不显示图案
                        alpha = 1  # 强制设置alpha为1，不显示透明度值
    ),
    color = guide_legend(order = 1),
    pattern = guide_legend(order = 2, override.aes = list(fill = "white"),
                           alpha = 1  # 强制设置alpha为1
    ),
    shape = guide_legend(order = 2,
                         override.aes = list(
                           alpha = 1  # 强制设置alpha为1
                         ))) 
# +
#   
#   # 添加分组标签
#   annotate("text", 
#            x = c(1.5, 3.5), 
#            y = -0.095,
#            label = c("Ctrl", "LPS"), 
#            size = 4.5, 
#            fontface = "bold")
g1
# g1 <- g1+geom_signif(comparisons = method_comparisons, 
#                      annotations = wilcox_results$annotation,
#                      step_increase = 0.01,
#                      textsize = 3,      # 调整此处数值以控制注释文本的大小
#                      size = 0.3)
# 
# ggsave("g1.pdf", g1, width = 100, height = 70, units = "mm")
#######################################NB############################
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(dplyr)
library(readxl)
library(ggpubr)  # 用于统计检验和添加显著性标记
library(rstatix) # 用于Wilcoxon检验
fc_accuracy <- read_excel("fc_accuracy.xlsx", 
                          sheet = "NB", col_types = c("text", 
                                                      "numeric", "text", "text", "text", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "text", "text", "numeric"))
# 定义要比较的方法组合
method_comparisons <- list(
  c("Original", "Mean"),
  c("Original", "Cov1"),
  c("Mean", "Cov1")
)



fc_accuracy$subgroup <- factor(fc_accuracy$subgroup,levels = c("Original", "Mean" , "Cov1"))


# 构造示例数据（修改组别名称）
set.seed(123)
fc_accuracy <- fc_accuracy %>%
  arrange(group, id, subgroup) %>%    # 按组别、ID和方法排序
  mutate(
    Accuracy_shifted = Accuracy - 0.5,   # 将数据以0.7为中心偏移
    group_num = as.numeric(factor(group, levels = c("Ctrl", "Ctrl+TAT-GluA23Y","LPS" , "LPS+TAT-GluA23Y"))),  # 将组别转换为数值
    subgroup_offset = case_when(       
      subgroup == "Original" ~ -0.25,
      subgroup == "Mean" ~ 0,
      subgroup == "Cov1" ~ 0.25
    ),              # 不同方法的x轴偏移量  控制original  mean  Cov点在各个直方图上
    xval = group_num + subgroup_offset,    # 最终x轴位置
    # 新增处理条件列
    treatment = ifelse(grepl("\\+TAT-GluA23Y", group), "TAT-GluA23Y", "Vehicle")
  )

treatment_colors <- c("Vehicle" = "#5CB0C3", "TAT-GluA23Y" = "#f5c75b")  # 处理条件颜色

subgroup_patterns <- c("Original" = "none", "Mean" = "stripe", "Cov1" = "crosshatch")  # 方法图案
subgroup_shapes <- c("Original" = 21, "Mean" = 22, "Cov1" = 24) # 方法形状


# 汇总数据
summary_df <- fc_accuracy %>%
  group_by(group, subgroup,treatment) %>%
  summarise(
    mean = mean(Accuracy_shifted),
    se = sd(Accuracy_shifted)/sqrt(n()),
    .groups = "drop"
  )
summary_df


shapiro_results <- fc_accuracy %>%
  group_by(group, subgroup) %>%
  filter(n() >= 3) %>%  # 确保每组至少有3个观测值
  summarise(
    shapiro_p = shapiro.test(Accuracy)$p.value,
    shapiro_W = shapiro.test(Accuracy)$statistic,
    n = n(),
    .groups = 'drop'
  )
shapiro_results
library(stringr)
fc_accuracy$group_short <- str_extract(fc_accuracy$group, "^(Ctrl|LPS)")

# 可用混合效应线性模型进行分析
library(lme4)      # fit mixed models
library(lmerTest)  # 给 lmer 提供 p 值
library(emmeans)   # 做 post-hoc
lmm_res <- lmer(Accuracy ~ group_short * treatment * subgroup + (1+ subgroup|id), data = fc_accuracy)

anova_res <- anova(lmm_res)
anova_res
summary(lmm_res)
emm <- emmeans(lmm_res, ~ subgroup | group_short * treatment)

# 查看 LPS + TAT-GluA23Y 条件下模型两两比较
# 两两比较，只在 LPS × TAT-GluA23Y 条件下
pairs(emm, by = c("group_short", "treatment"), adjust = "bonferroni")



# 对每个组别分别进行方法比较
wilcox_results <- fc_accuracy %>%
  group_by(group) %>%
  wilcox_test(Accuracy ~ subgroup, comparisons = method_comparisons) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = ifelse(
      p.adj < 0.001, "***",
      ifelse(p.adj < 0.01, "**",
             ifelse(p.adj < 0.05, "*", "ns"))))
wilcox_results


min(fc_accuracy$Accuracy_shifted)
max(fc_accuracy$Accuracy_shifted)
g2 <- ggplot() +
  # 1. 条形图（按处理条件着色）
  geom_bar_pattern(
    data = summary_df,
    aes(x = group, y = mean, fill = treatment, pattern = subgroup,
        pattern_fill = after_scale(fill),
        pattern_color = after_scale(fill)),
    stat = "identity",
    alpha = 0.8,
    position = position_dodge(width = 0.8),
    width = 0.7,  # 条形宽度
    #color = "black",   # 条形边框颜色
    # pattern_fill = after_scale(fill),  # 使用与填充色相同的颜色,#"gray", # 图案颜色
    # pattern_color = after_scale(fill),  # 使用与填充色相同的颜色,#"gray",
    pattern_angle = 45,  # 图案角度
    pattern_density = 0.3,  # 图案密度
    pattern_spacing = 0.02   # 图案间距
  ) +
  
  # 2. 误差棒
  geom_errorbar(
    data = summary_df,
    aes(x = group, ymin = mean - se, ymax = mean + se, group = subgroup),
    position = position_dodge(width = 0.8),
    width = 0.2,   # 误差棒宽度
    color = "black"  # 误差棒颜色
  ) +
  
  # 3. 数据点（按处理条件着色，形状按方法）
  geom_point(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        shape = subgroup,    # 形状表示方法
        color = treatment, # 颜色表示处理条件
        fill = treatment),  # 填充色表示处理条件
    #shape =21,
    alpha=0.6,  # 设置透明度(在aes外部)
    size = 0.5,  # 点大小
    ## fill = "white",
    stroke = 1.2   # 边框粗细
  ) +
  
  # 4. 配对连线（按处理条件着色）
  geom_line(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        group = interaction(group, id), # 按组和id分组
        color = treatment),  # 颜色按处理条件
    linewidth = 0.5,  # 线宽
    alpha = 0.6
  ) +
  
  # 5. 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # 坐标轴设置
  scale_y_continuous(
    name = "Accuracy",  # 轴标题
    breaks = seq(-0.1, 0.5, 0.1),  # 刻度位置
    labels = function(x) format(round(x + 0.5, 1), nsmall = 1),  # 刻度标签(还原偏移)
    limits = c(-0.1, 0.5),  # 轴范围
    expand = c(0, 0)  # 无扩展
  ) +
  scale_x_discrete(labels = c("Ctrl" = "Ctrl", ####\n  下一行显示的
                              "LPS+TAT-GluA23Y" = "LPS",
                              "Ctrl+TAT-GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  # 颜色与图案设置
  scale_fill_manual(values = treatment_colors) +
  scale_color_manual(values = treatment_colors) +
  scale_pattern_manual(values = subgroup_patterns) +
  scale_shape_manual(values = subgroup_shapes) +
  
  # 图例与主题
  labs(
    x = NULL,  # x轴标题(无)
    fill = "Treatment",   # 填充图例标题
    color = "Treatment",  # 颜色图例标题
    shape = "subgroup",   # 形状图例标题
    pattern = "subgroup",  # 图案图例标题
    title = "NB"
  ) +
  theme_bw() +   # 使用黑白主题
  theme(panel.grid = element_blank(),  ####去除网格线
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 10, color = "black", face = "bold"),
        legend.key.size = unit(3, "mm"),  # 单位可以是 "mm", "cm", "in" 等  控制legend的大小
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线 
  # 图例调整
  guides(
    fill = guide_legend(order = 1, # 第一顺序
                        override.aes = list(pattern = "none"),# 不显示图案
                        alpha = 1  # 强制设置alpha为1，不显示透明度值
    ),
    color = guide_legend(order = 1),
    pattern = guide_legend(order = 2, override.aes = list(fill = "white"),
                           alpha = 1  # 强制设置alpha为1
    ),
    shape = guide_legend(order = 2,
                         override.aes = list(
                           alpha = 1  # 强制设置alpha为1
                         ))) 
# +
#   
#   # 添加分组标签
#   annotate("text", 
#            x = c(1.5, 3.5), 
#            y = -0.095,
#            label = c("Ctrl", "LPS"), 
#            size = 4.5, 
#            fontface = "bold")
g2
# g2 <- g2+geom_signif(comparisons = method_comparisons, 
#                      annotations = wilcox_results$annotation,
#                      step_increase = 0.01,
#                      textsize = 3,      # 调整此处数值以控制注释文本的大小
#                      size = 0.3)
# 
# ggsave("g2.pdf", g2, width = 100, height = 70, units = "mm")

###########################################QDA############################
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(dplyr)
library(ggpubr)  # 用于统计检验和添加显著性标记
library(rstatix) # 用于Wilcoxon检验
library(readxl)
fc_accuracy <- read_excel("fc_accuracy.xlsx", 
                          sheet = "QDA", col_types = c("text", 
                                                       "numeric", "text", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "text", "numeric"))
# 定义要比较的方法组合
method_comparisons <- list(
  c("Original", "Mean"),
  c("Original", "Cov1"),
  c("Mean", "Cov1")
)



fc_accuracy$subgroup <- factor(fc_accuracy$subgroup,levels = c("Original", "Mean" , "Cov1"))


# 构造示例数据（修改组别名称）
set.seed(123)
fc_accuracy <- fc_accuracy %>%
  arrange(group, id, subgroup) %>%    # 按组别、ID和方法排序
  mutate(
    Accuracy_shifted = Accuracy - 0.5,   # 将数据以0.7为中心偏移
    group_num = as.numeric(factor(group, levels = c("Ctrl", "Ctrl+TAT-GluA23Y","LPS" , "LPS+TAT-GluA23Y"))),  # 将组别转换为数值
    subgroup_offset = case_when(       
      subgroup == "Original" ~ -0.25,
      subgroup == "Mean" ~ 0,
      subgroup == "Cov1" ~ 0.25
    ),              # 不同方法的x轴偏移量  控制original  mean  Cov点在各个直方图上
    xval = group_num + subgroup_offset,    # 最终x轴位置
    # 新增处理条件列
    treatment = ifelse(grepl("\\+TAT-GluA23Y", group), "TAT-GluA23Y", "Vehicle")
  )

treatment_colors <- c("Vehicle" = "#5CB0C3", "TAT-GluA23Y" = "#f5c75b")  # 处理条件颜色

subgroup_patterns <- c("Original" = "none", "Mean" = "stripe", "Cov1" = "crosshatch")  # 方法图案
subgroup_shapes <- c("Original" = 21, "Mean" = 22, "Cov1" = 24) # 方法形状


# 汇总数据
summary_df <- fc_accuracy %>%
  group_by(group, subgroup,treatment) %>%
  summarise(
    mean = mean(Accuracy_shifted),
    se = sd(Accuracy_shifted)/sqrt(n()),
    .groups = "drop"
  )
summary_df

shapiro_results <- fc_accuracy %>%
  group_by(group, subgroup) %>%
  filter(n() >= 3) %>%  # 确保每组至少有3个观测值
  summarise(
    shapiro_p = shapiro.test(Accuracy)$p.value,
    shapiro_W = shapiro.test(Accuracy)$statistic,
    n = n(),
    .groups = 'drop'
  )
shapiro_results

fc_accuracy$group_short <- str_extract(fc_accuracy$group, "^(Ctrl|LPS)")

# 可用混合效应线性模型进行分析
library(lme4)      # fit mixed models
library(lmerTest)  # 给 lmer 提供 p 值
library(emmeans)   # 做 post-hoc
lmm_res <- lmer(Accuracy ~ group_short * treatment * subgroup + (1+ subgroup|id), data = fc_accuracy)

anova_res <- anova(lmm_res)
anova_res
summary(lmm_res)
emm <- emmeans(lmm_res, ~ subgroup | group_short * treatment)

# 查看 LPS + TAT-GluA23Y 条件下模型两两比较
# 两两比较，只在 LPS × TAT-GluA23Y 条件下
pairs(emm, by = c("group_short", "treatment"), adjust = "bonferroni")



# 对每个组别分别进行方法比较
wilcox_results <- fc_accuracy %>%
  group_by(group) %>%
  wilcox_test(Accuracy ~ subgroup, comparisons = method_comparisons) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = ifelse(
      p.adj < 0.001, "***",
      ifelse(p.adj < 0.01, "**",
             ifelse(p.adj < 0.05, "*", "ns"))))
wilcox_results


min(fc_accuracy$Accuracy_shifted)
max(fc_accuracy$Accuracy_shifted)
g3 <- ggplot() +
  # 1. 条形图（按处理条件着色）
  geom_bar_pattern(
    data = summary_df,
    aes(x = group, y = mean, fill = treatment, pattern = subgroup,
        pattern_fill = after_scale(fill),
        pattern_color = after_scale(fill)),
    stat = "identity",
    alpha = 0.8,
    position = position_dodge(width = 0.8),
    width = 0.7,  # 条形宽度
    #color = "black",   # 条形边框颜色
    # pattern_fill = after_scale(fill),  # 使用与填充色相同的颜色,#"gray", # 图案颜色
    # pattern_color = after_scale(fill),  # 使用与填充色相同的颜色,#"gray",
    pattern_angle = 45,  # 图案角度
    pattern_density = 0.3,  # 图案密度
    pattern_spacing = 0.02   # 图案间距
  ) +
  
  # 2. 误差棒
  geom_errorbar(
    data = summary_df,
    aes(x = group, ymin = mean - se, ymax = mean + se, group = subgroup),
    position = position_dodge(width = 0.8),
    width = 0.2,   # 误差棒宽度
    color = "black"  # 误差棒颜色
  ) +
  
  # 3. 数据点（按处理条件着色，形状按方法）
  geom_point(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        shape = subgroup,    # 形状表示方法
        color = treatment, # 颜色表示处理条件
        fill = treatment),  # 填充色表示处理条件
    #shape =21,
    alpha=0.6,  # 设置透明度(在aes外部)
    size = 0.5,  # 点大小
    ## fill = "white",
    stroke = 1.2   # 边框粗细
  ) +
  
  # 4. 配对连线（按处理条件着色）
  geom_line(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        group = interaction(group, id), # 按组和id分组
        color = treatment),  # 颜色按处理条件
    linewidth = 0.5,  # 线宽
    alpha = 0.6
  ) +
  
  # 5. 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # 坐标轴设置
  scale_y_continuous(
    name = "Accuracy",  # 轴标题
    breaks = seq(-0.1, 0.5, 0.1),  # 刻度位置
    labels = function(x) format(round(x + 0.5, 1), nsmall = 1),  # 刻度标签(还原偏移)
    limits = c(-0.1, 0.5),  # 轴范围
    expand = c(0, 0)  # 无扩展
  ) +
  scale_x_discrete(labels = c("Ctrl" = "Ctrl", ####\n  下一行显示的
                              "LPS+TAT-GluA23Y" = "LPS",
                              "Ctrl+TAT-GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  # 颜色与图案设置
  scale_fill_manual(values = treatment_colors) +
  scale_color_manual(values = treatment_colors) +
  scale_pattern_manual(values = subgroup_patterns) +
  scale_shape_manual(values = subgroup_shapes) +
  
  # 图例与主题
  labs(
    x = NULL,  # x轴标题(无)
    fill = "Treatment",   # 填充图例标题
    color = "Treatment",  # 颜色图例标题
    shape = "subgroup",   # 形状图例标题
    pattern = "subgroup",  # 图案图例标题
    title = "QDA"
  ) +
  theme_bw() +   # 使用黑白主题
  theme(panel.grid = element_blank(),  ####去除网格线
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 10, color = "black", face = "bold"),
        legend.key.size = unit(3, "mm"),  # 单位可以是 "mm", "cm", "in" 等  控制legend的大小
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线 
  # 图例调整
  guides(
    fill = guide_legend(order = 1, # 第一顺序
                        override.aes = list(pattern = "none"),# 不显示图案
                        alpha = 1  # 强制设置alpha为1，不显示透明度值
    ),
    color = guide_legend(order = 1),
    pattern = guide_legend(order = 2, override.aes = list(fill = "white"),
                           alpha = 1  # 强制设置alpha为1
    ),
    shape = guide_legend(order = 2,
                         override.aes = list(
                           alpha = 1  # 强制设置alpha为1
                         ))) 
# +
#   
#   # 添加分组标签
#   annotate("text", 
#            x = c(1.5, 3.5), 
#            y = -0.095,
#            label = c("Ctrl", "LPS"), 
#            size = 4.5, 
#            fontface = "bold")
g3
# g3 <- g3+geom_signif(comparisons = method_comparisons, 
#                      annotations = wilcox_results$annotation,
#                      step_increase = 0.01,
#                      textsize = 3,      # 调整此处数值以控制注释文本的大小
#                      size = 0.3)
# 
# ggsave("g3.pdf", g3, width = 100, height = 70, units = "mm")



###########################################NN########################
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(dplyr)
library(ggpubr)  # 用于统计检验和添加显著性标记
library(rstatix) # 用于Wilcoxon检验
library(readxl)
fc_accuracy <- read_excel("fc_accuracy.xlsx", 
                          sheet = "NN", col_types = c("text", 
                                                      "numeric", "text", "text", "text", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "text", "text", "numeric"))

# 定义要比较的方法组合
method_comparisons <- list(
  c("Original", "Mean"),
  c("Original", "Cov1"),
  c("Mean", "Cov1")
)

method_comparisons <- list(
  c("Original", "Mean"),
  c("Original", "Cov1"),
  c("Mean", "Cov1")
)


fc_accuracy$subgroup <- factor(fc_accuracy$subgroup,levels = c("Original", "Mean" , "Cov1"))


# 构造示例数据（修改组别名称）
set.seed(123)
fc_accuracy <- fc_accuracy %>%
  arrange(group, id, subgroup) %>%    # 按组别、ID和方法排序
  mutate(
    Accuracy_shifted = Accuracy - 0.5,   # 将数据以0.7为中心偏移
    group_num = as.numeric(factor(group, levels = c("Ctrl", "Ctrl+TAT-GluA23Y","LPS" , "LPS+TAT-GluA23Y"))),  # 将组别转换为数值
    subgroup_offset = case_when(       
      subgroup == "Original" ~ -0.25,
      subgroup == "Mean" ~ 0,
      subgroup == "Cov1" ~ 0.25
    ),              # 不同方法的x轴偏移量  控制original  mean  Cov点在各个直方图上
    xval = group_num + subgroup_offset,    # 最终x轴位置
    # 新增处理条件列
    treatment = ifelse(grepl("\\+TAT-GluA23Y", group), "TAT-GluA23Y", "Vehicle")
  )

treatment_colors <- c("Vehicle" = "#5CB0C3", "TAT-GluA23Y" = "#f5c75b")  # 处理条件颜色

subgroup_patterns <- c("Original" = "none", "Mean" = "stripe", "Cov1" = "crosshatch")  # 方法图案
subgroup_shapes <- c("Original" = 21, "Mean" = 22, "Cov1" = 24) # 方法形状


# 汇总数据
summary_df <- fc_accuracy %>%
  group_by(group, subgroup,treatment) %>%
  summarise(
    mean = mean(Accuracy_shifted),
    se = sd(Accuracy_shifted)/sqrt(n()),
    .groups = "drop"
  )
summary_df

shapiro_results <- fc_accuracy %>%
  group_by(group, subgroup) %>%
  filter(n() >= 3) %>%  # 确保每组至少有3个观测值
  summarise(
    shapiro_p = shapiro.test(Accuracy)$p.value,
    shapiro_W = shapiro.test(Accuracy)$statistic,
    n = n(),
    .groups = 'drop'
  )
shapiro_results

fc_accuracy$group_short <- str_extract(fc_accuracy$group, "^(Ctrl|LPS)")

# 可用混合效应线性模型进行分析
library(lme4)      # fit mixed models
library(lmerTest)  # 给 lmer 提供 p 值
library(emmeans)   # 做 post-hoc
lmm_res <- lmer(Accuracy ~ group_short * treatment * subgroup + (1+ subgroup|id), data = fc_accuracy)

anova_res <- anova(lmm_res)
anova_res
summary(lmm_res)
emm <- emmeans(lmm_res, ~ subgroup | group_short * treatment)

# 查看 LPS + TAT-GluA23Y 条件下模型两两比较
# 两两比较，只在 LPS × TAT-GluA23Y 条件下
pairs(emm, by = c("group_short", "treatment"), adjust = "bonferroni")

# 对每个组别分别进行方法比较
wilcox_results <- fc_accuracy %>%
  group_by(group) %>%
  wilcox_test(Accuracy ~ subgroup, comparisons = method_comparisons) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = ifelse(
      p.adj < 0.001, "***",
      ifelse(p.adj < 0.01, "**",
             ifelse(p.adj < 0.05, "*", "ns"))))
wilcox_results

# 首先创建所有可能的 group 比较组合
group_comparisons <- list(
  c("Ctrl", "Ctrl+TAT-GluA23Y"),
  c("Ctrl", "LPS"),
  c("Ctrl", "LPS+TAT-GluA23Y"),
  c("Ctrl+TAT-GluA23Y", "LPS"),
  c("Ctrl+TAT-GluA23Y", "LPS+TAT-GluA23Y"),
  c("LPS", "LPS+TAT-GluA23Y")
)
# 对每个 subgroup 分别进行 group 比较
wilcox_results_by_subgroup <- fc_accuracy %>%
  group_by(subgroup) %>%
  wilcox_test(Accuracy ~ group, comparisons = group_comparisons) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01 ~ "**",
      p.adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

# 查看结果
wilcox_results_by_subgroup



min(fc_accuracy$Accuracy_shifted)
max(fc_accuracy$Accuracy_shifted)
g4 <- ggplot() +
  # 1. 条形图（按处理条件着色）
  geom_bar_pattern(
    data = summary_df,
    aes(x = group, y = mean, fill = treatment, pattern = subgroup,
        pattern_fill = after_scale(fill),
        pattern_color = after_scale(fill)),
    stat = "identity",
    alpha = 0.8,
    position = position_dodge(width = 0.8),
    width = 0.7,  # 条形宽度
    #color = "black",   # 条形边框颜色
    # pattern_fill = after_scale(fill),  # 使用与填充色相同的颜色,#"gray", # 图案颜色
    # pattern_color = after_scale(fill),  # 使用与填充色相同的颜色,#"gray",
    pattern_angle = 45,  # 图案角度
    pattern_density = 0.3,  # 图案密度
    pattern_spacing = 0.02   # 图案间距
  ) +
  
  # 2. 误差棒
  geom_errorbar(
    data = summary_df,
    aes(x = group, ymin = mean - se, ymax = mean + se, group = subgroup),
    position = position_dodge(width = 0.8),
    width = 0.2,   # 误差棒宽度
    color = "black"  # 误差棒颜色
  ) +
  
  # 3. 数据点（按处理条件着色，形状按方法）
  geom_point(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        shape = subgroup,    # 形状表示方法
        color = treatment, # 颜色表示处理条件
        fill = treatment),  # 填充色表示处理条件
    #shape =21,
    alpha=0.6,  # 设置透明度(在aes外部)
    size = 0.5,  # 点大小
    ## fill = "white",
    stroke = 1.2   # 边框粗细
  ) +
  
  # 4. 配对连线（按处理条件着色）
  geom_line(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        group = interaction(group, id), # 按组和id分组
        color = treatment),  # 颜色按处理条件
    linewidth = 0.5,  # 线宽
    alpha = 0.6
  ) +
  
  # 5. 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # 坐标轴设置
  scale_y_continuous(
    name = "Accuracy",  # 轴标题
    breaks = seq(-0.1, 0.5, 0.1),  # 刻度位置
    labels = function(x) format(round(x + 0.5, 1), nsmall = 1),  # 刻度标签(还原偏移)
    limits = c(-0.1, 0.5),  # 轴范围
    expand = c(0, 0)  # 无扩展
  ) +
  scale_x_discrete(labels = c("Ctrl" = "Ctrl", ####\n  下一行显示的
                              "LPS+TAT-GluA23Y" = "LPS",
                              "Ctrl+TAT-GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  # 颜色与图案设置
  scale_fill_manual(values = treatment_colors) +
  scale_color_manual(values = treatment_colors) +
  scale_pattern_manual(values = subgroup_patterns) +
  scale_shape_manual(values = subgroup_shapes) +
  
  # 图例与主题
  labs(
    x = NULL,  # x轴标题(无)
    fill = "Treatment",   # 填充图例标题
    color = "Treatment",  # 颜色图例标题
    shape = "subgroup",   # 形状图例标题
    pattern = "subgroup",  # 图案图例标题
    title = "NN"
  ) +
  theme_bw() +   # 使用黑白主题
  theme(panel.grid = element_blank(),  ####去除网格线
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 10, color = "black", face = "bold"),
        legend.key.size = unit(3, "mm"),  # 单位可以是 "mm", "cm", "in" 等  控制legend的大小
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线 
  # 图例调整
  guides(
    fill = guide_legend(order = 1, # 第一顺序
                        override.aes = list(pattern = "none"),# 不显示图案
                        alpha = 1  # 强制设置alpha为1，不显示透明度值
    ),
    color = guide_legend(order = 1),
    pattern = guide_legend(order = 2, override.aes = list(fill = "white"),
                           alpha = 1  # 强制设置alpha为1
    ),
    shape = guide_legend(order = 2,
                         override.aes = list(
                           alpha = 1  # 强制设置alpha为1
                         ))) 
# +
#   
#   # 添加分组标签
#   annotate("text", 
#            x = c(1.5, 3.5), 
#            y = -0.095,
#            label = c("Ctrl", "LPS"), 
#            size = 4.5, 
#            fontface = "bold")
g4
# g4 <- g4+geom_signif(comparisons = method_comparisons, 
#                      annotations = wilcox_results$annotation,
#                      step_increase = 0.01,
#                      textsize = 3,      # 调整此处数值以控制注释文本的大小
#                      size = 0.3)
# 
# ggsave("g4.pdf", g4, width = 100, height = 70, units = "mm")

#################################################gSVM##########################
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(dplyr)
library(ggpubr)  # 用于统计检验和添加显著性标记
library(rstatix) # 用于Wilcoxon检验
library(readxl)
fc_accuracy <- read_excel("fc_accuracy.xlsx", 
                          sheet = "gSVM", col_types = c("text", 
                                                        "numeric", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "numeric"))

# 定义要比较的方法组合
method_comparisons <- list(
  c("Original", "Mean"),
  c("Original", "Cov1"),
  c("Mean", "Cov1")
)


fc_accuracy$subgroup <- factor(fc_accuracy$subgroup,levels = c("Original", "Mean" , "Cov1"))


# 构造示例数据（修改组别名称）
set.seed(123)
fc_accuracy <- fc_accuracy %>%
  arrange(group, id, subgroup) %>%    # 按组别、ID和方法排序
  mutate(
    Accuracy_shifted = Accuracy - 0.5,   # 将数据以0.7为中心偏移
    group_num = as.numeric(factor(group, levels = c("Ctrl", "Ctrl+TAT-GluA23Y","LPS" , "LPS+TAT-GluA23Y"))),  # 将组别转换为数值
    subgroup_offset = case_when(       
      subgroup == "Original" ~ -0.25,
      subgroup == "Mean" ~ 0,
      subgroup == "Cov1" ~ 0.25
    ),              # 不同方法的x轴偏移量  控制original  mean  Cov点在各个直方图上
    xval = group_num + subgroup_offset,    # 最终x轴位置
    # 新增处理条件列
    treatment = ifelse(grepl("\\+TAT-GluA23Y", group), "TAT-GluA23Y", "Vehicle")
  )

treatment_colors <- c("Vehicle" = "#5CB0C3", "TAT-GluA23Y" = "#f5c75b")  # 处理条件颜色

subgroup_patterns <- c("Original" = "none", "Mean" = "stripe", "Cov1" = "crosshatch")  # 方法图案
subgroup_shapes <- c("Original" = 21, "Mean" = 22, "Cov1" = 24) # 方法形状


# 汇总数据
summary_df <- fc_accuracy %>%
  group_by(group, subgroup,treatment) %>%
  summarise(
    mean = mean(Accuracy_shifted),
    se = sd(Accuracy_shifted)/sqrt(n()),
    .groups = "drop"
  )
summary_df

shapiro_results <- fc_accuracy %>%
  group_by(group, subgroup) %>%
  filter(n() >= 3) %>%  # 确保每组至少有3个观测值
  summarise(
    shapiro_p = shapiro.test(Accuracy)$p.value,
    shapiro_W = shapiro.test(Accuracy)$statistic,
    n = n(),
    .groups = 'drop'
  )
shapiro_results

fc_accuracy$group_short <- str_extract(fc_accuracy$group, "^(Ctrl|LPS)")

# 可用混合效应线性模型进行分析
library(lme4)      # fit mixed models
library(lmerTest)  # 给 lmer 提供 p 值
library(emmeans)   # 做 post-hoc
lmm_res <- lmer(Accuracy ~ group_short * treatment * subgroup + (1+ subgroup|id), data = fc_accuracy)

anova_res <- anova(lmm_res)
anova_res
summary(lmm_res)
emm <- emmeans(lmm_res, ~ subgroup | group_short * treatment)

# 查看 LPS + TAT-GluA23Y 条件下模型两两比较
# 两两比较，只在 LPS × TAT-GluA23Y 条件下
pairs(emm, by = c("group_short", "treatment"), adjust = "bonferroni")

# 对每个组别分别进行方法比较
wilcox_results <- fc_accuracy %>%
  group_by(group) %>%
  wilcox_test(Accuracy ~ subgroup, comparisons = method_comparisons) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = ifelse(
      p.adj < 0.001, "***",
      ifelse(p.adj < 0.01, "**",
             ifelse(p.adj < 0.05, "*", "ns"))))
wilcox_results

# 首先创建所有可能的 group 比较组合
group_comparisons <- list(
  c("Ctrl", "Ctrl+TAT-GluA23Y"),
  c("Ctrl", "LPS"),
  c("Ctrl", "LPS+TAT-GluA23Y"),
  c("Ctrl+TAT-GluA23Y", "LPS"),
  c("Ctrl+TAT-GluA23Y", "LPS+TAT-GluA23Y"),
  c("LPS", "LPS+TAT-GluA23Y")
)
# 对每个 subgroup 分别进行 group 比较
wilcox_results_by_subgroup <- fc_accuracy %>%
  group_by(subgroup) %>%
  wilcox_test(Accuracy ~ group, comparisons = group_comparisons) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01 ~ "**",
      p.adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

# 查看结果
wilcox_results_by_subgroup



min(fc_accuracy$Accuracy_shifted)
max(fc_accuracy$Accuracy_shifted)
g5 <- ggplot() +
  # 1. 条形图（按处理条件着色）
  geom_bar_pattern(
    data = summary_df,
    aes(x = group, y = mean, fill = treatment, pattern = subgroup,
        pattern_fill = after_scale(fill),
        pattern_color = after_scale(fill)),
    stat = "identity",
    alpha = 0.8,
    position = position_dodge(width = 0.8),
    width = 0.7,  # 条形宽度
    #color = "black",   # 条形边框颜色
    # pattern_fill = after_scale(fill),  # 使用与填充色相同的颜色,#"gray", # 图案颜色
    # pattern_color = after_scale(fill),  # 使用与填充色相同的颜色,#"gray",
    pattern_angle = 45,  # 图案角度
    pattern_density = 0.3,  # 图案密度
    pattern_spacing = 0.02   # 图案间距
  ) +
  
  # 2. 误差棒
  geom_errorbar(
    data = summary_df,
    aes(x = group, ymin = mean - se, ymax = mean + se, group = subgroup),
    position = position_dodge(width = 0.8),
    width = 0.2,   # 误差棒宽度
    color = "black"  # 误差棒颜色
  ) +
  
  # 3. 数据点（按处理条件着色，形状按方法）
  geom_point(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        shape = subgroup,    # 形状表示方法
        color = treatment, # 颜色表示处理条件
        fill = treatment),  # 填充色表示处理条件
    #shape =21,
    alpha=0.6,  # 设置透明度(在aes外部)
    size = 0.5,  # 点大小
    ## fill = "white",
    stroke = 1.2   # 边框粗细
  ) +
  
  # 4. 配对连线（按处理条件着色）
  geom_line(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        group = interaction(group, id), # 按组和id分组
        color = treatment),  # 颜色按处理条件
    linewidth = 0.5,  # 线宽
    alpha = 0.6
  ) +
  
  # 5. 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # 坐标轴设置
  scale_y_continuous(
    name = "Accuracy",  # 轴标题
    breaks = seq(-0.1, 0.5, 0.1),  # 刻度位置
    labels = function(x) format(round(x + 0.5, 1), nsmall = 1),  # 刻度标签(还原偏移)
    limits = c(-0.1, 0.5),  # 轴范围
    expand = c(0, 0)  # 无扩展
  ) +
  scale_x_discrete(labels = c("Ctrl" = "Ctrl", ####\n  下一行显示的
                              "LPS+TAT-GluA23Y" = "LPS",
                              "Ctrl+TAT-GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  # 颜色与图案设置
  scale_fill_manual(values = treatment_colors) +
  scale_color_manual(values = treatment_colors) +
  scale_pattern_manual(values = subgroup_patterns) +
  scale_shape_manual(values = subgroup_shapes) +
  
  # 图例与主题
  labs(
    x = NULL,  # x轴标题(无)
    fill = "Treatment",   # 填充图例标题
    color = "Treatment",  # 颜色图例标题
    shape = "subgroup",   # 形状图例标题
    pattern = "subgroup",  # 图案图例标题
    title = "gSVM"
  ) +
  theme_bw() +   # 使用黑白主题
  theme(panel.grid = element_blank(),  ####去除网格线
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 10, color = "black", face = "bold"),
        legend.key.size = unit(3, "mm"),  # 单位可以是 "mm", "cm", "in" 等  控制legend的大小
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线 
  # 图例调整
  guides(
    fill = guide_legend(order = 1, # 第一顺序
                        override.aes = list(pattern = "none"),# 不显示图案
                        alpha = 1  # 强制设置alpha为1，不显示透明度值
    ),
    color = guide_legend(order = 1),
    pattern = guide_legend(order = 2, override.aes = list(fill = "white"),
                           alpha = 1  # 强制设置alpha为1
    ),
    shape = guide_legend(order = 2,
                         override.aes = list(
                           alpha = 1  # 强制设置alpha为1
                         ))) 
# +
#   
#   # 添加分组标签
#   annotate("text", 
#            x = c(1.5, 3.5), 
#            y = -0.095,
#            label = c("Ctrl", "LPS"), 
#            size = 4.5, 
#            fontface = "bold")
g5
# g5 <- g5+geom_signif(comparisons = method_comparisons, 
#                      annotations = wilcox_results$annotation,
#                      step_increase = 0.01,
#                      textsize = 3,      # 调整此处数值以控制注释文本的大小
#                      size = 0.3)
# 
# ggsave("g5.pdf", g5, width = 100, height = 70, units = "mm")


####################################original 各组之间比较#####################
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(dplyr)
library(ggpubr)  # 用于统计检验和添加显著性标记
library(rstatix) # 用于Wilcoxon检验
library(readxl)
fc_accuracy <- read_excel("fc_accuracy.xlsx", 
                          sheet = "LNQNS", col_types = c("text", 
                                                         "numeric", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "numeric"))

fc_accuracy$model <- factor(fc_accuracy$model,levels = c("LDA", "NB" , "QDA","NN","gSVM"))


# 定义要比较的方法组合
method_comparisons <- combn(unique(fc_accuracy$model), 2, simplify = FALSE)


# 构造示例数据（修改组别名称）
set.seed(123)
fc_accuracy <- fc_accuracy %>%
  arrange(group, id, model) %>%    # 按组别、ID和方法排序
  mutate(
    Accuracy_shifted = Accuracy - 0.5,   # 将数据以0.7为中心偏移
    group_num = as.numeric(factor(group, levels = c("Ctrl", "Ctrl+TAT-GluA23Y","LPS" , "LPS+TAT-GluA23Y"))),  # 将组别转换为数值
    # model_offset = case_when(       
    #   model == "LDA" ~ -0.50,
    #   model == "NB" ~ -0.25,
    #   model == "QDA" ~ 0,
    #   model == "NN" ~ 0.25,
    #   model == "gSVM" ~ 0.50,
    #   
    # ),              # 不同方法的x轴偏移量  控制original  mean  Cov点在各个直方图上
    # xval = group_num + model_offset,    # 最终x轴位置
    model_rank = as.numeric(factor(model, levels = c("LDA", "NB", "QDA", "NN", "gSVM"))),
    xval = group_num + (model_rank - 3) * 0.16,  # 关键修改：对齐 position_dodge(width = 0.8)
    
    # 新增处理条件列
    treatment = ifelse(grepl("\\+TAT-GluA23Y", group), "TAT-GluA23Y", "Vehicle")
  )

treatment_colors <- c("Vehicle" = "#5CB0C3", "TAT-GluA23Y" = "#f5c75b")  # 处理条件颜色

model_patterns <- c(
  "LDA" = "none",        # 无图案
  "NB" = "stripe",       # 条纹
  "QDA" = "stripe",  # 交叉线
  "NN" = "circle",       # 圆圈图案
  "gSVM" = "crosshatch"        # 
)  # 方法图案
model_shapes <- c(
  "LDA" = 21,  # 圆形（带边框）
  "NB" = 22,   # 方形（带边框）
  "QDA" = 24,  # 三角形（带边框）
  "NN" = 23,   # 菱形（带边框）
  "gSVM" = 25  # 倒三角形（带边框）
) # 方法形状


# 汇总数据
summary_df <- fc_accuracy %>%
  group_by(group, model,treatment) %>%
  summarise(
    mean = mean(Accuracy_shifted),
    se = sd(Accuracy_shifted)/sqrt(n()),
    .groups = "drop"
  )
summary_df
# 对每个 subgroup 进行正态性检验
shapiro_results <- fc_accuracy %>%
  group_by(group, model) %>%
  filter(n() >= 3) %>%  # 确保每组至少有3个观测值
  summarise(
    shapiro_p = shapiro.test(Accuracy)$p.value,
    shapiro_W = shapiro.test(Accuracy)$statistic,
    n = n(),
    .groups = 'drop'
  )
# 输出结果
print(shapiro_results)
library(stringr)
fc_accuracy$group_short <- str_extract(fc_accuracy$group, "^(Ctrl|LPS)")

# 模型之间相当于是重复测量， 所以这个地方 应该使用重复测量的方差分析 三因素的   但是这个
# 地方怎么弄LPS组都存在意义
fc_accuracy$group_short
aov_res <- aov(Accuracy ~ group_short*treatment*model + Error(id/group_short), data = fc_accuracy)
summary(aov_res)

emm <- emmeans(aov_res, ~ model | group_short * treatment)  # 每个 group 内比较模型
pairs(emm, adjust = "bonferroni")


# 可用混合效应线性模型进行分析
library(lme4)      # fit mixed models
library(lmerTest)  # 给 lmer 提供 p 值
library(emmeans)   # 做 post-hoc
lmm_res <- lmer(Accuracy ~ group_short * treatment * model + (1+ model|id), data = fc_accuracy)
anova_res <- anova(lmm_res)
summary(lmm_res)
emm <- emmeans(lmm_res, ~ model | group_short * treatment)

# 查看 LPS + TAT-GluA23Y 条件下模型两两比较
# 两两比较，只在 LPS × TAT-GluA23Y 条件下
pairs(emm, by = c("group_short", "treatment"), adjust = "bonferroni")
library(tibble)
anova_summary <- anova_res %>%
  as.data.frame() %>%
  rownames_to_column("Effect") %>%
  mutate(
    F_stat = round(`F value`, 3),
    NumDF = as.integer(NumDF),
    DenDF = round(DenDF, 0),
    P_val = case_when(
      `Pr(>F)` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(`Pr(>F)`, 3))
    ),
    # 拼接成 F(df1, df2) = F, P = ...
    text = paste0(Effect, ": F(", NumDF, ",", DenDF, ") = ", F_stat, ", P ", P_val)
  )

# 拼接成一句话
paste(anova_summary$text, collapse = "; ")




# 对每个组别分别进行方法比较
wilcox_results <- fc_accuracy %>%
  group_by(group) %>%
  wilcox_test(Accuracy ~ model, comparisons = method_comparisons,paired = TRUE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    annotation = ifelse(
      p.adj < 0.001, "***",
      ifelse(p.adj < 0.01, "**",
             ifelse(p.adj < 0.05, "*", "ns"))))


wilcox_results
print(wilcox_results,n =40)

min(fc_accuracy$Accuracy_shifted)
max(fc_accuracy$Accuracy_shifted)
g6 <- ggplot() +
  # 1. 条形图（按处理条件着色）
  geom_bar_pattern(
    data = summary_df,
    aes(x = group, y = mean, fill = treatment, pattern = model,
        pattern_fill = after_scale(fill),
        pattern_color = after_scale(fill)),
    stat = "identity",
    alpha = 0.8,
    position = position_dodge(width = 0.8),
    width = 0.7,  # 条形宽度
    #color = "black",   # 条形边框颜色
    # pattern_fill = after_scale(fill),  # 使用与填充色相同的颜色,#"gray", # 图案颜色
    # pattern_color = after_scale(fill),  # 使用与填充色相同的颜色,#"gray",
    pattern_angle = ifelse(summary_df$model == "NB", 0, 45),  # NB的角度为0（水平），其他为45度
    pattern_density = 0.3,  # 图案密度
    pattern_spacing = 0.02   # 图案间距
  ) +
  
  # 2. 误差棒
  geom_errorbar(
    data = summary_df,
    aes(x = group, ymin = mean - se, ymax = mean + se, group = model),
    position = position_dodge(width = 0.8),
    width = 0.2,   # 误差棒宽度
    color = "black"  # 误差棒颜色
  ) +
  
  # 3. 数据点（按处理条件着色，形状按方法）
  geom_point(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        shape = model,    # 形状表示方法
        color = treatment, # 颜色表示处理条件
        fill = treatment),  # 填充色表示处理条件
    #shape =21,
    alpha=0.6,  # 设置透明度(在aes外部)
    size = 0.5,  # 点大小
    ## fill = "white",
    stroke = 1.2   # 边框粗细
  ) +
  
  # 4. 配对连线（按处理条件着色）
  geom_line(
    data = fc_accuracy,
    aes(x = xval, y = Accuracy_shifted, 
        group = interaction(group, id), # 按组和id分组
        color = treatment),  # 颜色按处理条件
    linewidth = 0.5,  # 线宽
    alpha = 0.6
  ) +
  
  # 5. 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # 坐标轴设置
  scale_y_continuous(
    name = "Accuracy",  # 轴标题
    breaks = seq(0, 0.5, 0.1),  # 刻度位置
    labels = function(x) format(round(x + 0.5, 1), nsmall = 1),  # 刻度标签(还原偏移)
    limits = c(0, 0.5),  # 轴范围
    expand = c(0, 0)  # 无扩展
  ) +
  scale_x_discrete(labels = c("Ctrl" = "Ctrl", ####\n  下一行显示的
                              "LPS+TAT-GluA23Y" = "LPS",
                              "Ctrl+TAT-GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  # 颜色与图案设置
  scale_fill_manual(values = treatment_colors) +
  scale_color_manual(values = treatment_colors) +
  scale_pattern_manual(values = model_patterns) +
  scale_shape_manual(values = model_shapes) +
  
  # 图例与主题
  labs(
    x = NULL,  # x轴标题(无)
    fill = "Treatment",   # 填充图例标题
    color = "Treatment",  # 颜色图例标题
    shape = "model",   # 形状图例标题
    pattern = "model",  # 图案图例标题
    #title = "LDA"
  ) +
  theme_bw() +   # 使用黑白主题
  theme(panel.grid = element_blank(),  ####去除网格线
        #plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 10, color = "black", face = "bold"),
        legend.key.size = unit(3, "mm"),  # 单位可以是 "mm", "cm", "in" 等  控制legend的大小
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "top",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线 
  # 图例调整
  guides(
    fill = guide_legend(order = 1, # 第一顺序
                        override.aes = list(pattern = "none"),# 不显示图案
                        alpha = 1  # 强制设置alpha为1，不显示透明度值
    ),
    color = guide_legend(order = 1),
    pattern = guide_legend(order = 2, override.aes = list(fill = "white"),
                           alpha = 1  # 强制设置alpha为1
    ),
    shape = guide_legend(order = 2,
                         override.aes = list(
                           alpha = 1  # 强制设置alpha为1
                         ))) 
# +
#   
#   # 添加分组标签
#   annotate("text", 
#            x = c(1.5, 3.5), 
#            y = -0.095,
#            label = c("Ctrl", "LPS"), 
#            size = 4.5, 
#            fontface = "bold")
g6
# g6 <- g6+geom_signif(comparisons = method_comparisons, 
#                      annotations = wilcox_results$annotation,
#                      step_increase = 0.01,
#                      textsize = 3,      # 调整此处数值以控制注释文本的大小
#                      size = 0.3)
# 
# ggsave("g6.pdf", g6, width = 280, height = 70, units = "mm")
# 
# 
# g1
# g2
# g3
# g4
# g5
# g6
ggsave("g1.pdf", g1, width = 100, height = 70, units = "mm")
ggsave("g2.pdf", g2, width = 100, height = 70, units = "mm")
ggsave("g3.pdf", g3, width = 100, height = 70, units = "mm")
ggsave("g4.pdf", g4, width = 100, height = 70, units = "mm")
ggsave("g5.pdf", g5, width = 100, height = 70, units = "mm")
ggsave("g6.pdf", g6, width = 160, height = 70, units = "mm")