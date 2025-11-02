
library(ggbreak)
library(tidyverse)
# library(gapminder)
library(ggpubr)
library(rstatix)
library(ggprism)
library(readxl)
library(ggplot2)
# 4. 绘图 A: 半宽 vs 全宽 (类似 Spike half-width vs Spike width 图)
# 使用 geom_jitter 为每个 cluster 的点添加不同的抖动参数

excitatory_inhibitory_neurons_subset <- read_excel("excitatory_inhibitory_neurons_subset.xlsx")

g1 <- ggplot(excitatory_inhibitory_neurons_subset, aes(x = peak_to_valley, y = half_width, fill = as.factor(cluster))) +
  geom_point(size = 4, alpha = 0.95, shape = 21, color = "white", stroke = 1
             ,position = position_jitter(width = 5, height = 5)
  ) +
  stat_ellipse(aes(color = as.factor(cluster)),type = "norm", level = 0.80, linetype = 2, linewidth = 1,) + # 更高的聚合趋势
  labs(x = "Spike peak to valley (μs)", y = "Spike half-width (μs)", fill = "Cluster") +
  scale_fill_manual(values = c("#038355", "purple")) +
  scale_color_manual(values = c("#038355", "purple")) + # 椭圆线的颜色与点的颜色一致
  scale_y_continuous(limits = c(100, 500),breaks = seq(100, 500, by = 100),expand = c(0, 0))+
  scale_x_continuous(limits = c(200, 1200),breaks = seq(200, 1200, by = 200),expand = c(0, 0))+
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        axis.line.y.right = element_blank(),    # 移除右侧y轴线
        axis.text.y.right = element_blank(),    # 移除右边Y轴标签
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        panel.border = element_blank())
g1
ggsave("g2.pdf", g1 , width = 100, height = 70, units = "mm")

#######################################################

combinations <- combn(unique(excitatory_inhibitory_neurons_subset$group_cluster), 2, simplify = FALSE)
Process_comparisons <- combinations  ####
Process_comparisons


# 对每个 subgroup 进行正态性检验
shapiro_results <- by(excitatory_inhibitory_neurons_subset$firing_rate,
                      excitatory_inhibitory_neurons_subset$group_cluster, shapiro.test)
# 输出结果
print(shapiro_results)

kruskal_test <- kruskal.test(excitatory_inhibitory_neurons_subset$firing_rate ~ group_cluster,
                             data = excitatory_inhibitory_neurons_subset)
kruskal_test
library(FSA)
dunnTest(excitatory_inhibitory_neurons_subset$firing_rate ~ group, 
         data = excitatory_inhibitory_neurons_subset, method = "bonferroni")



unique(excitatory_inhibitory_neurons_subset$group_cluster)

wilcox_test_result <- excitatory_inhibitory_neurons_subset %>%
  wilcox_test(firing_rate ~ group_cluster,comparisons = Process_comparisons) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>%
  mutate(annotation = ifelse(p.adj < 0.001, "p < 0.001", paste0("p = ", round(p.adj, 3)))) # Adjust format as needed
wilcox_test_result

excitatory_inhibitory_neurons_subset$subgroup <- ifelse(excitatory_inhibitory_neurons_subset$group_cluster %in% 
                                                          c("lps7d_hm3dq_cno_1", "control_hm3dq_cno_1",
                                                            "lps7d_hm3dq_cno_2", "control_hm3dq_cno_2"), 
                                                        "CNO", "Vehicle")





g2 <- ggplot(excitatory_inhibitory_neurons_subset, aes(x = group_cluster, y = firing_rate, fill = subgroup)) +
  geom_half_violin(side = "r", color=NA
                   #, alpha=0.75
  ) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = c("Vehicle" = "#CCD376", "CNO" = "#A28CC2"))+
  labs(x = "", y = "Firing rate (Hz)") +
  scale_x_discrete(labels = c("control_hm3dq_1" = "pPN\nCtrl", ####\n  下一行显示的
                              "control_hm3dq_2" = "pIN\nCtrl",
                              "lps7d_hm3dq_1" = "pPN\nLPS",
                              "lps7d_hm3dq_2" = "pIN\nLPS",
                              "control_hm3dq_cno_1" = "pPN\nCtrl+CNO",
                              "control_hm3dq_cno_2" = "pIN\nCtrl+CNO",
                              "lps7d_hm3dq_cno_1" = "pPN\nLPS+CNO",
                              "lps7d_hm3dq_cno_2" = "pIN\nLPS+CNO"))+
  scale_y_continuous(limits = c(0,30), expand = c(0, 0),breaks = seq(0, 30, by = 5)) + 
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.text.x = element_text(size=12, color = "black",face = "bold"), ##, angle = 45, hjust = 1将 x 轴刻度文本旋转 45 度。hjust = 1：调整水平对齐方式，使文本的末端与刻度对齐。hjust = 1 表示右对齐，如果希望居中可以使用 hjust = 0.5。
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())
g2

Process_comparisons
# 定义你需要的特定比较
selected_comparisons <- list(
  c("control_hm3dq_1", "lps7d_hm3dq_cno_1"),
  c("control_hm3dq_1", "lps7d_hm3dq_1"),
  c("control_hm3dq_1", "control_hm3dq_cno_1"),
  c("control_hm3dq_2", "lps7d_hm3dq_2"),
  c("control_hm3dq_2", "control_hm3dq_cno_2"),
  c("control_hm3dq_2", "lps7d_hm3dq_cno_2"),
  c("lps7d_hm3dq_1", "control_hm3dq_cno_1"),
  c("lps7d_hm3dq_1", "lps7d_hm3dq_cno_1"),
  c("lps7d_hm3dq_2", "control_hm3dq_cno_2"),
  c("lps7d_hm3dq_2", "lps7d_hm3dq_cno_2"),
  c("control_hm3dq_cno_1", "lps7d_hm3dq_cno_1"),
  c("control_hm3dq_cno_2", "lps7d_hm3dq_cno_2")
)

wilcox_test_result <- excitatory_inhibitory_neurons_subset %>%
  wilcox_test(firing_rate ~ group_cluster,comparisons = selected_comparisons) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>%
  mutate(annotation = ifelse(p.adj < 0.001, "p < 0.001", paste0("p = ", round(p.adj, 3)))) # Adjust format as needed
wilcox_test_result




g2 <- g2 + geom_signif(comparisons = selected_comparisons, 
                       annotations = wilcox_test_result$annotation,
                       step_increase = 0.015,
                       size = 0.3)
g2
ggsave("g2.pdf", g2 , width = 100, height = 70, units = "mm")
