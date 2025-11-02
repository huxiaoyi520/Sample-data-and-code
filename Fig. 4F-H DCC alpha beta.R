#########雪崩中的dcc#######
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
library(xlsx)
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")

combined_rows_control_lps7d_min7d <- read_excel("combined_rows_control_lps7d_min7d.xlsx")
combined_rows_control_lps7d_min7d$DCC <- as.numeric(combined_rows_control_lps7d_min7d$DCC)
# 对每个 group 进行正态性检验
shapiro_results <- by(combined_rows_control_lps7d_min7d$DCC,
                      combined_rows_control_lps7d_min7d$group, shapiro.test)


shapiro_results

# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(combined_rows_control_lps7d_min7d$group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####


# # 进行Kruskal-Wallis检验
# kruskal_test <- kruskal.test(DCC ~ group, data = combined_rows_control_lps7d_min7d)
# kruskal_test
# Dunn's Test 进行两两比较
# dunn_result <- dunn.test(combined_rows_control_lps7d_min7d$DCC, 
#                          combined_rows_control_lps7d_min7d$group, 
#                          method = "bonferroni")
# print(dunn_result)


# 进行单因素方差分析
anova_result <- aov(combined_rows_control_lps7d_min7d$DCC ~ group, data = combined_rows_control_lps7d_min7d)
summary(anova_result)
##事后检验
TukeyHSD(anova_result)



t_test_result <- combined_rows_control_lps7d_min7d %>%
  t_test(DCC ~ group,comparisons = Process_comparisons) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>%
  mutate(annotation = ifelse(p.adj < 0.001, "p < 0.001", paste0("p = ", round(p.adj, 3)))) # Adjust format as needed

Process_comparisons


g1 <- ggplot(data = combined_rows_control_lps7d_min7d,
             aes(x=group, y=DCC, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 0.4), 
                     breaks = seq(0, 0.4, by = 0.1),
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="DCC",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=8, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线
# stat_compare_means(comparisons = Process_comparisons,
#                    #method = "kruskal.test",###此处方法默认是kruskal.test,t.test,anova,wilcox.test
#                    vjust = 0.02,
#                    alternative = "two.sided",
#                    p.adjust.method = "bonferroni",  # 确保没有进行多重比较修正
#                    label.x = 1,
#                    data = combined_rows_control_lps7d_min7d)+
# stat_compare_means(method = "wilcox.test",ref.group = "control")
g1 <- g1 + geom_signif(comparisons = Process_comparisons, 
                       annotations = t_test_result$annotation,
                       step_increase = 0.2,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5
g1


#########雪崩中的alpha#######
combined_rows_control_lps7d_min7d$alpha <- as.numeric(combined_rows_control_lps7d_min7d$alpha)
# 对每个 group 进行正态性检验
shapiro_results <- by(combined_rows_control_lps7d_min7d$alpha,
                      combined_rows_control_lps7d_min7d$group, shapiro.test)
shapiro_results

# 进行Kruskal-Wallis检验
kruskal_test <- kruskal.test(alpha ~ group, data = combined_rows_control_lps7d_min7d)
kruskal_test


wilcox_test_result <- combined_rows_control_lps7d_min7d %>%
  wilcox_test(alpha ~ group,comparisons = Process_comparisons) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>%
  mutate(annotation = ifelse(p.adj < 0.001, "p < 0.001", paste0("p = ", round(p.adj, 3)))) # Adjust format as needed

g2 <- ggplot(data = combined_rows_control_lps7d_min7d,
             aes(x=group, y=alpha, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 4), 
                     expand = c(0, 4),
                     breaks = seq(0, 4, by = 1),
                     labels = label_number(accuracy = 0.1)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Avalanche Size Exponent (α)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=8, color = "black",face = "bold"),
        axis.line.y.right = element_blank(),    # 移除右侧y轴线
        axis.text.y.right = element_blank(),    # 移除右边Y轴标签
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  scale_y_break(breaks = c(0.5,1),expand = c(0,0),scales = c(50,1))
# stat_compare_means(comparisons = Process_comparisons,
#                    #method = "kruskal.test",###此处方法默认是kruskal.test,t.test,anova,wilcox.test
#                    vjust = 0.02,
#                    alternative = "two.sided",
#                    p.adjust.method = "bonferroni",  # 确保没有进行多重比较修正
#                    label.x = 1,
#                    data = combined_rows_control_lps7d_min7d)+
# stat_compare_means(method = "wilcox.test",ref.group = "control")
g2 <- g2 + geom_signif(comparisons = Process_comparisons, 
                       annotations = wilcox_test_result$annotation,
                       step_increase = 0.2,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5

g2

#########雪崩中的beta#######
library(ggbreak)
library(tidyverse)
# library(gapminder)
library(ggpubr)
library(rstatix) 
library(ggprism)
library(scales)  # 加载 scales 包来使用 label_number()
# 对每个 group 进行正态性检验
combined_rows_control_lps7d_min7d$beta <- as.numeric(combined_rows_control_lps7d_min7d$beta)
shapiro_results <- by(combined_rows_control_lps7d_min7d$beta,
                      combined_rows_control_lps7d_min7d$group, shapiro.test)
shapiro_results

# 进行Kruskal-Wallis检验
kruskal_test <- kruskal.test(beta ~ group, data = combined_rows_control_lps7d_min7d)
kruskal_test


wilcox_test_result <- combined_rows_control_lps7d_min7d %>%
  wilcox_test(beta ~ group,comparisons = Process_comparisons) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>%
  mutate(annotation = ifelse(p.adj < 0.001, "p < 0.001", paste0("p = ", round(p.adj, 3)))) # Adjust format as needed

g3 <- ggplot(data = combined_rows_control_lps7d_min7d,
             aes(x=group, y=beta, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 5), 
                     expand = c(0, 0),
                     breaks = seq(0, 5, by = 1),
                     labels = label_number(accuracy = 0.1)) +     ####label_number  可以使得y轴精确到小数点后一位数
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Avalanche time Exponent (β)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=8, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        axis.line.y.right = element_blank(),    # 移除右侧y轴线
        axis.text.y.right = element_blank(),    # 移除右边Y轴标签
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  scale_y_break(breaks = c(0.5,2),expand = c(0,0),scales = c(50,1))
# stat_compare_means(comparisons = Process_comparisons,
#                    #method = "kruskal.test",###此处方法默认是kruskal.test,t.test,anova,wilcox.test
#                    vjust = 0.02,
#                    alternative = "two.sided",
#                    p.adjust.method = "bonferroni",  # 确保没有进行多重比较修正
#                    label.x = 1,
#                    data = combined_rows_control_lps7d_min7d)+
# stat_compare_means(method = "wilcox.test",
#                    ref.group = "control")+

g3 <- g3 + geom_signif(comparisons = Process_comparisons, 
                       annotations = wilcox_test_result$annotation,
                       step_increase = 0.2,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5

g3

library(gridExtra)
# 加载 gridExtra 包
library(ggplot2)

p1 <- grid.arrange(g1,g2,g3,nrow = 1,ncol = 3)
ggsave("p_2024_11_19.pdf", p1, width = 180, height = 60, units = "mm")

write.csv(combined_rows_control_lps7d_min7d,"combined_rows_control_lps7d_min7d.csv")