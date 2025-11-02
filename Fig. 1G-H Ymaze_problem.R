library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(Rmisc)
library(FSA)
library(readxl)


ymaze_2025_10_28 <- read_excel("ymaze_2025_10_28.xlsx")
data <- ymaze_2025_10_28

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(data$arm_entries, 
                      data$group, shapiro.test)
shapiro_results
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
Process_comparisons <- list(c("control", "lps"))  ####


# 使用 t_test 
t_test_result <- data %>%
  t_test(arm_entries ~ group, comparisons = Process_comparisons) %>%
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



data_long <- summarySE(data, measurevar="arm_entries", groupvars=c("group"))

####此处是直方图
g1 <- ggplot(data_long, aes(x = group, y = arm_entries, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = arm_entries - se, ymax = arm_entries + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = data,aes(x = group, y = arm_entries),
              shape =21, alpha = 0.9)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Total arm entries",x=NULL) +
  scale_y_continuous(labels = function(x) paste0("  ", x),  ####可使得y轴字符占据三个位置
                     limits = c(0, 50),
                     expand = c(0, 0),
                     breaks = seq(0, 60, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))  #####控制标题居中，并且是粗体
g1

library(ggpubr)
library(ggsignif)
g1 <- g1 + geom_signif(comparisons = Process_comparisons, 
                       annotations = t_test_result$annotation,
                       step_increase = 0.2,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5
g1


# 自发交替率 -------------------------------------------------------------------
data <- read_excel("ymaze_alternation_2025_10_28.xlsx")
library(readxl)

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(data$alternation_ratio, 
                      data$group, shapiro.test)
shapiro_results
# 使用 t_test 
t_test_result <- data %>%
  t_test(alternation_ratio ~ group, comparisons = Process_comparisons) %>%
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

data_long <- summarySE(data, measurevar="alternation_ratio", groupvars=c("group"))


g2 <- ggplot(data_long, aes(x = group, y = alternation_ratio, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = alternation_ratio - se, ymax = alternation_ratio + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = data,aes(x = group, y = alternation_ratio),
              shape =21, alpha = 0.9)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Alternation (%)",x=NULL) +
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
        plot.title = element_text(hjust = 0.5, face = "bold"))
g2

g2 <- g2 + geom_signif(comparisons = Process_comparisons, 
                annotations = t_test_result$annotation,
                step_increase = 0.3,
                size = 0.3) # Adjust this value to control line thickness, default is 0.5
g2
