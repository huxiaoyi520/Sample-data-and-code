library(ggplot2)
library(gghalves)
library(ggpubr)
library(FSA)
library(rstatix)
library(Rmisc)
library(scales)
library(ggbreak)
library(dplyr)
library(gridExtra)
library(readxl)
DCC_LPS_CONTROL <- read_excel("DCC_LPS_CONTROL.xlsx")


ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
#################################DCC############################################
# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(DCC_LPS_CONTROL$group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(DCC_LPS_CONTROL$DCC,DCC_LPS_CONTROL$group, shapiro.test)
# 输出结果
print(shapiro_results)

# kruskal_test <- kruskal.test(DCC_LPS_CONTROL$DCC ~ group, data = DCC_LPS_CONTROL)
# kruskal_test
# dunnTest(DCC_LPS_CONTROL$DCC ~ group, 
#          data = DCC_LPS_CONTROL, method = "bonferroni")

# 使用 t_test 替代 wilcox_test（参数检验）
t_test_result <- DCC_LPS_CONTROL %>%
  t_test(DCC ~ group, comparisons = Process_comparisons) %>%
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
# wilcox_test_result <- DCC_LPS_CONTROL %>%
#   wilcox_test(DCC ~ group,comparisons = Process_comparisons) %>%
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

DCC_LPS_CONTROL$group
DCC_long <- summarySE(DCC_LPS_CONTROL, measurevar="DCC", groupvars=c("group"))


####此处是直方图
g1 <- ggplot(data = DCC_long, aes(x = group, y = DCC, 
                                  fill = group )) +
  geom_half_violin(side = "r", color=NA, alpha=0.35,data = DCC_LPS_CONTROL) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5,data = DCC_LPS_CONTROL) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white",data = DCC_LPS_CONTROL) +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 0.4), 
                     breaks = seq(0, 0.4, by = 0.1),
                     expand = c(0, 0)) +     ####这个位置可以更改
  labs(y="DCC",x=NULL) +
  # scale_x_discrete(labels = c("lps7d_hm3dq_cno" = "LPS", ####\n  下一行显示的
  #                             "control_hm3dq_cno" = "Ctrl",
  #                             "control_hm3dq" = "Ctrl",
  #                             "lps7d_hm3dq" = "LPS"
  # ))+
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.key.size = unit(5, "mm"),  # 单位可以是 "mm", "cm", "in" 等
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        legend.text = element_text(size = 10, color = "black",
                                   face = "bold"),   # ← 这里将图例标签设为粗体
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g1 <- g1 + geom_signif(comparisons = Process_comparisons, 
                       annotations = t_test_result$annotation,
                       step_increase = 0.1,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5
g1

ggsave("g1.pdf", g1, width = 60, height = 60, units = "mm")


##################alpha############################

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(DCC_LPS_CONTROL$alpha,DCC_LPS_CONTROL$group, shapiro.test)
# 输出结果
print(shapiro_results)

# kruskal_test <- kruskal.test(DCC_LPS_CONTROL$alpha ~ group, data = DCC_LPS_CONTROL)
# kruskal_test
# dunnTest(DCC_LPS_CONTROL$DCC ~ group, 
#          data = DCC_LPS_CONTROL, method = "bonferroni")

# 使用 t_test 替代 wilcox_test（参数检验）
t_test_result <- DCC_LPS_CONTROL %>%
  t_test(alpha ~ group, comparisons = Process_comparisons) %>%
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
# wilcox_test_result <- DCC_LPS_CONTROL %>%
#   wilcox_test(alpha ~ group,comparisons = Process_comparisons) %>%
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

alpha_long <- summarySE(DCC_LPS_CONTROL, measurevar="alpha", groupvars=c("group"))


g2 <- ggplot(data = alpha_long, aes(x = group, y = alpha, 
                                    fill = group )) +
  geom_half_violin(side = "r", color=NA, alpha=0.35,data = DCC_LPS_CONTROL) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5,data = DCC_LPS_CONTROL) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white",data = DCC_LPS_CONTROL) +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 4), 
                     breaks = seq(0, 4, by = 1),
                     expand = c(0, 0),
                     labels = label_number(accuracy = 0.1)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Avalanche Size Exponent (α)",x=NULL) +
  scale_x_discrete(labels = c("lps7d_hm3dq_cno" = "LPS", ####\n  下一行显示的
                              "control_hm3dq_cno" = "Ctrl",
                              "control_hm3dq" = "Ctrl",
                              "lps7d_hm3dq" = "LPS"
  ))+
  scale_y_break(breaks = c(0.5,1),expand = c(0,0),scales = c(50,1))+
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.key.size = unit(5, "mm"),  # 单位可以是 "mm", "cm", "in" 等
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        legend.text = element_text(size = 10, color = "black",
                                   face = "bold"),   # ← 这里将图例标签设为粗体
        axis.text.x = element_text(size=10, color = "black",face = "bold"),
        axis.line.y.right = element_blank(),    # 移除右侧y轴线
        axis.text.y.right = element_blank(),    # 移除右边Y轴标签
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g2 <- g2 + geom_signif(comparisons = Process_comparisons, 
                       annotations = t_test_result$annotation,
                       step_increase = 0.1,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5
g2

ggsave("g2.pdf", g2, width = 60, height = 60, units = "mm")

##################beta############################

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(DCC_LPS_CONTROL$beta,DCC_LPS_CONTROL$group, shapiro.test)
# 输出结果
print(shapiro_results)

# kruskal_test <- kruskal.test(DCC_LPS_CONTROL$beta ~ group, data = DCC_LPS_CONTROL)
# kruskal_test
# dunnTest(DCC_LPS_CONTROL$DCC ~ group, 
#          data = DCC_LPS_CONTROL, method = "bonferroni")

# 使用 t_test 替代 wilcox_test（参数检验）
t_test_result <- DCC_LPS_CONTROL %>%
  t_test(beta ~ group, comparisons = Process_comparisons) %>%
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
# wilcox_test_result <- DCC_LPS_CONTROL %>%
#   wilcox_test(beta ~ group,comparisons = Process_comparisons) %>%
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


beta_long <- summarySE(DCC_LPS_CONTROL, measurevar="beta", groupvars=c("group"))

g3 <- ggplot(data = beta_long, aes(x = group, y = beta, 
                                   fill = group )) +
  geom_half_violin(side = "r", color=NA, alpha=0.35,data = DCC_LPS_CONTROL) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5,data = DCC_LPS_CONTROL) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white",data = DCC_LPS_CONTROL) +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 5), 
                     breaks = seq(0, 5, by = 1),
                     expand = c(0, 0),
                     labels = label_number(accuracy = 0.1)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Avalanche time Exponent (β)",x=NULL) +
  # scale_x_discrete(labels = c("lps7d_hm3dq_cno" = "LPS", ####\n  下一行显示的
  #                             "control_hm3dq_cno" = "Ctrl",
  #                             "control_hm3dq" = "Ctrl",
  #                             "lps7d_hm3dq" = "LPS"
  # ))+
  scale_y_break(breaks = c(0.5,1.5),expand = c(0,0),scales = c(50,1))+
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.key.size = unit(5, "mm"),  # 单位可以是 "mm", "cm", "in" 等
        legend.title = element_blank(), ###element_text(size = 10, face = "bold"),  # 调整标题文字大小和样式
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        legend.text = element_text(size = 10, color = "black",
                                   face = "bold"),   # ← 这里将图例标签设为粗体
        axis.line.y.right = element_blank(),    # 移除右侧y轴线
        axis.text.y.right = element_blank(),    # 移除右边Y轴标签
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        axis.title = element_text(size = 10, color = "black",face = "bold"),  
        axis.text = element_text(size=10, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g3 <- g3 + geom_signif(comparisons = Process_comparisons, 
                       annotations = t_test_result$annotation,
                       step_increase = 0.12,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5
g3

ggsave("g3.pdf", g3, width = 60, height = 60, units = "mm")


p1 <- grid.arrange(g2,g3,g1,nrow = 1,ncol = 3)
p1
ggsave("p_2025_4_26.pdf", p1, width = 180, height = 60, units = "mm")
g1
