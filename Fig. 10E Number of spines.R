library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(FSA)
library(rstatix)
library(Rmisc)

library(readxl)
dendritic_spine <- read_excel("dendritic_spine.xlsx")
colnames(dendritic_spine)


dendritic_spine_samll <- dendritic_spine[,c("count",
                                            "group"),]

ordercolors<-c("#FDDED7","#F5BE8F","#C1E0DB","#CCD376","#A28CC2","#8498AB","#5CB0C3","#f5c75b")

# ordercolors<-c("#2F2D54","#9193B4","#EAB080","#BD9AAD","#B31761")
# Process_comparisons <- list(c("control", "lps","min")) 
# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(dendritic_spine_samll$group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####

dendritic_spine_samll$group <- as.factor(dendritic_spine_samll$group)
# 对每个 subgroup 进行正态性检验
shapiro_results <- by(dendritic_spine_samll$count,dendritic_spine_samll$group, shapiro.test)
# 输出结果

print(shapiro_results)

dendritic_spine_samll$group_short <- str_extract(dendritic_spine_samll$group, "^(control|LPS)")

dendritic_spine_samll$subgroup_short <- ifelse(grepl("TAT_GluA23Y", dendritic_spine_samll$group), "TAT_GluA23Y", "Vehicle")

library(ARTool)

dendritic_spine_samll$group_short <- as.factor(dendritic_spine_samll$group_short)
dendritic_spine_samll$subgroup_short <- as.factor(dendritic_spine_samll$subgroup_short)
# 构建 ART 模型
art_model <- art(count ~ group_short * subgroup_short, data = dendritic_spine_samll)

# ANOVA 结果
anova(art_model)

# 主效应对比
art.con(art_model, ~ group_short)
art.con(art_model, ~ subgroup)

# 交互作用对比
art.con(art_model, ~ group_short * subgroup_short)


kruskal_test <- kruskal.test(dendritic_spine_samll$count ~ group, data = dendritic_spine_samll)
kruskal_test
dunnTest(dendritic_spine_samll$count ~ group, 
         data = dendritic_spine_samll, method = "bonferroni")
# 使用 t_test 替代 wilcox_test（参数检验）
t_test_result <- dendritic_spine_samll %>%
  t_test(count ~ group, comparisons = Process_comparisons) %>%
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
# wilcox_test_result <- dendritic_spine_samll %>%
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


dendritic_spine_samll_long <- summarySE(dendritic_spine_samll, measurevar="count", groupvars=c("group"))

dendritic_spine_samll_long$subgroup <- ifelse(dendritic_spine_samll_long$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")
dendritic_spine_samll$subgroup <- ifelse(dendritic_spine_samll$group %in% c("LPS_TAT_GluA23Y", "control_TAT_GluA23Y"), "TAT_GluA23Y", "Vehicle")

####此处是直方图
g1 <- ggplot(dendritic_spine_samll_long, aes(x = group, y = count, 
                                                    fill = subgroup )) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = count - se, ymax = count + se), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_jitter(data = dendritic_spine_samll,aes(x = group, y = count),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = c("TAT_GluA23Y" = "#f5c75b", "Vehicle" = "#5CB0C3"))+
  labs(y="Number of spines/10 μm",x=NULL) +
  scale_x_discrete(labels = c("control" = "Ctrl", ####\n  下一行显示的
                              "LPS_TAT_GluA23Y" = "LPS",
                              "control_TAT_GluA23Y" = "Ctrl",
                              "LPS" = "LPS"
  ))+
  scale_y_continuous(labels = function(x) paste0("  ", x),  ####可使得y轴字符占据三个位置,两个空格占据一个字符。
                     limits = c(0, 15),
                     expand = c(0, 0),
                     breaks = seq(0, 15, by = 5)) +   ####expand = c(0, 0)  xy的0点重合
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
                     step_increase = 0.1,
                     textsize = 3,      # 调整此处数值以控制注释文本的大小
                     size = 0.3) # Adjust this value to control line thickness, default is 0.5
g1
ggsave("g1.pdf", g1, width = 70, height = 70, units = "mm")


