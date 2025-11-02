library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(Rmisc)
library(FSA)
library(rstatix)
library(Rmisc)

##################TNF######################
library(readxl)
TNF <- read_excel("TNF_three.xlsx")

ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
combinations <- combn(unique(TNF$Group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####
Process_comparisons

TNF


min(TNF$Value)
max(TNF$Value)


# 对每个 group 进行正态性检验
shapiro_results <- by(TNF$Value, 
                      TNF$Group, shapiro.test)
# 输出结果
print(shapiro_results)

####此处是直方图
TNF <- as.data.frame(TNF)
TNF_long <- summarySE(TNF, measurevar="Value", groupvars=c("Group"))
str(TNF_long)

# 进行单因素方差分析
anova_result <- aov(TNF$Value ~ Group, data = TNF)
summary(anova_result)
##事后检验
TukeyHSD(anova_result)



t_test_result <- TNF %>%
  t_test(Value ~ Group, comparisons = Process_comparisons) %>%
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



g1 <- ggplot(TNF_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Value - se, ymax = Value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = TNF,aes(x = Group, y = Value),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="TNF-α/β-actin",x=NULL) +
  scale_x_discrete(labels = c("Control" = "Ctrl", ####\n  下一行显示的
                              "LPS" = "LPS",
                              "Min" = "LPS+Min"
  ))+
  scale_y_continuous(limits = c(0, 2),
                     expand = c(0, 0)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=8, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g1 <- g1 + geom_signif(comparisons = Process_comparisons, 
                      annotations = t_test_result$annotation,
                      step_increase = 0.05,
                      textsize = 3,      # 调整此处数值以控制注释文本的大小
                      size = 0.3) # Adjust this value to control line thickness, default is 0.5
g1

ggsave("TNF-α.pdf", g2, width = 50, height = 50, units = "mm")


####通常在-1到1之间，其中0表示文本的基线与位置之间的对齐，负值表示文本向上移动，
####正值表示文本向下移动。
####label.x 是 stat_compare_means() 函数的一个参数，用于控制比较结果标签的水平位置。
####这个参数接受一个数值，指定了标签的水平位置，通常在0到1之间，其中0表示标签位于比较组的左侧，
####1表示标签位于比较组的右侧。


########################IL-6###########################
library(readxl)
IL_6 <- read_excel("IL-6_three.xlsx")

ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
combinations <- combn(unique(IL_6$Group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####
Process_comparisons

IL_6
min(IL_6$Value)
max(IL_6$Value)
# 对每个 group 进行正态性检验
shapiro_results <- by(IL_6$Value, 
                      IL_6$Group, shapiro.test)
# 输出结果
print(shapiro_results)

####此处是直方图
IL_6 <- as.data.frame(IL_6)
IL_6_long <- summarySE(IL_6, measurevar="Value", groupvars=c("Group"))
str(IL_6_long)

# 进行单因素方差分析
anova_result <- aov(IL_6$Value ~ Group, data = IL_6)
summary(anova_result)
##事后检验
TukeyHSD(anova_result)


t_test_result <- IL_6 %>%
  t_test(Value ~ Group, comparisons = Process_comparisons) %>%
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

g2 <- ggplot(IL_6_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Value - se, ymax = Value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = IL_6,aes(x = Group, y = Value),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="IL-6/β-actin",x=NULL) +
  scale_x_discrete(labels = c("Control" = "Ctrl", ####\n  下一行显示的
                              "LPS" = "LPS",
                              "Min" = "LPS+Min"
  ))+
  scale_y_continuous(limits = c(0, 2),
                     expand = c(0, 0)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=8, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g2 <- g2+ geom_signif(comparisons = Process_comparisons, 
                      annotations = t_test_result$annotation,
                      step_increase = 0.05,
                      textsize = 3,      # 调整此处数值以控制注释文本的大小
                      size = 0.3) # Adjust this value to control line thickness, default is 0.5
g2

ggsave("IL_6.pdf", g2, width = 50, height = 50, units = "mm")


##########################IL-1################
library(readxl)
IL_1 <- read_excel("IL-1_three.xlsx")

ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
combinations <- combn(unique(IL_1$Group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####
Process_comparisons

# 进行单因素方差分析
anova_result <- aov(IL_1$Value ~ Group, data = IL_1)
summary(anova_result)
##事后检验
TukeyHSD(anova_result)


min(IL_1$Value)
max(IL_1$Value)


# 对每个 group 进行正态性检验
shapiro_results <- by(IL_1$Value, 
                      IL_1$Group, shapiro.test)
# 输出结果
print(shapiro_results)


library(ggplot2)

####此处是直方图
IL_1 <- as.data.frame(IL_1)
IL_1_long <- summarySE(IL_1, measurevar="Value", groupvars=c("Group"))
str(IL_1_long)


t_test_result <- IL_1 %>%
  t_test(Value ~ Group, comparisons = Process_comparisons) %>%
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



g3 <- ggplot(IL_1_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Value - se, ymax = Value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = IL_1,aes(x = Group, y = Value),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="IL-1/β-actin",x=NULL) +
  scale_x_discrete(labels = c("Control" = "Ctrl", ####\n  下一行显示的
                              "LPS" = "LPS",
                              "Min" = "LPS+Min"
  ))+
  scale_y_continuous(limits = c(0, 2),
                     expand = c(0, 0)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=8, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g3 <- g3 + geom_signif(comparisons = Process_comparisons, 
                      annotations = t_test_result$annotation,
                      step_increase = 0.05,
                      textsize = 3,      # 调整此处数值以控制注释文本的大小
                      size = 0.3) # Adjust this value to control line thickness, default is 0.5
g3
ggsave("IL_1.pdf", g2, width = 50, height = 50, units = "mm")
