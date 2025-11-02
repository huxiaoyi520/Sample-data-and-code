library(ggbreak)
library(tidyverse)
# library(gapminder)
library(ggpubr)
library(rstatix)
library(ggprism)
library(scales)
library(gghalves)
library(ggplot2)
merged_result_2024_11_22 <- read_excel("merged_result_2024_11_22.xlsx")


min(merged_result_2024_11_22$`Branching Ratio`)

# 自定义颜色顺序
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#FF9700")
# merged_result_2024_11_22  这个对应分支比文件夹下面的merged_result_2024_11_22.xlsx文件   注意！！注意！！
g1 <- ggplot(merged_result_2024_11_22, aes(x = `Branching Ratio`, y = DCC, color = group, fill = group)) +
  geom_point() + 
  # 为每个 group 绘制 LOESS 拟合曲线和置信区间
  geom_smooth(method = "loess", se = FALSE, alpha = 0.2) +
  # 绘制整体数据的拟合曲线和置信区间，颜色为第一个指定颜色（灰色）
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = ordercolors[4], fill = ordercolors[4], linetype = "dashed", alpha = 0.2,linewidth=1) +
  labs(#title = "Scatter Plot of DCC vs Branching Ratio",
       x = "Branching Ratio",
       y = "DCC") + 
  scale_color_manual(values = ordercolors[-4]) + # 为每个 group 设置颜色（去除整体拟合的颜色）
  scale_fill_manual(values = ordercolors[-4]) + # 为每个 group 设置填充颜色（去除整体拟合的颜色）
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.1),expand = c(0, 0)) +
  scale_x_continuous(limits = c(0.4, 1.4), breaks = seq(0.4, 1.4, by = 0.2),expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = c(0.8,0.9),   #####"none", "left", "right", "bottom", "top"  "inside"这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        axis.line.y.right = element_blank(),    # 移除右侧y轴线
        axis.text.y.right = element_blank(),    # 移除右边Y轴标签
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线
g1

ggsave("p_2024_11_22.pdf", g1 , width = 70, height = 70, units = "mm")



# 对每个 group 进行正态性检验

shapiro_results <- by(merged_result_2024_11_22$`Branching Ratio`,
                      merged_result_2024_11_22$group, shapiro.test)
shapiro_results


combinations <- combn(unique(merged_result_2024_11_22$group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####


kruskal_test <- kruskal.test(merged_result_2024_11_22$`Branching Ratio` ~ group, data = merged_result_2024_11_22)
kruskal_test


wilcox_test_result <- merged_result_2024_11_22 %>%
  wilcox_test(`Branching Ratio` ~ group,
              comparisons = Process_comparisons) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>%
  mutate(annotation = ifelse(p.adj < 0.001, "p < 0.001", paste0("p = ", round(p.adj, 3)))) # Adjust format as needed
wilcox_test_result

# merged_result_2024_11_22  这个对应分支比文件夹下面的merged_result_2024_11_22.xlsx文件
g2 <- ggplot(data = merged_result_2024_11_22,
             aes(x=group, y=`Branching Ratio`, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 2), 
                     expand = c(0, 0),
                     breaks = seq(0, 2, by = 0.5),
                     labels = label_number(accuracy = 0.1)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Branching Ratio",x=NULL) +
  scale_x_discrete(labels = c("control" = "Ctrl", "lps7d" = "LPS","min7d"="Min"))+############################自定义x轴刻度内容
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=12, color = "black",face = "bold"),
        axis.line.y.right = element_blank(),    # 移除右侧y轴线
        axis.text.y.right = element_blank(),    # 移除右边Y轴标签
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())#+  #####这个可以去除顶部和右边的边框线
  #scale_y_break(breaks = c(0.5,1),expand = c(0,0),scales = c(50,1))
# stat_compare_means(comparisons = Process_comparisons,
#                    #method = "kruskal.test",###此处方法默认是kruskal.test,t.test,anova,wilcox.test
#                    vjust = 0.02,
#                    alternative = "two.sided",
#                    p.adjust.method = "bonferroni",  # 确保没有进行多重比较修正
#                    label.x = 1,
#                    data = merged_result_2024_11_22)+
# stat_compare_means(method = "wilcox.test",ref.group = "control")
g2 <- g2 + geom_signif(comparisons = Process_comparisons, 
                       annotations = wilcox_test_result$annotation,
                       step_increase = 0.2,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5
g2
library(gridExtra)
p1 <- grid.arrange(g1,g2,nrow = 1,ncol = 2)
p1
ggsave("p1_2024_11_22.pdf", p1 , width = 140, height = 70, units = "mm")
