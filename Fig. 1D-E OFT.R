library(xlsx)
library(readxl)
library(Rmisc)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggsignif)
library(ggplot2)
library(gghalves)
library(FSA)



# 总距离 ---------------------------------------------------------------------

OFT_2025_10_28 <- read_excel("OFT_2025_10_28.xlsx")
data <- OFT_2025_10_28
# 对每个 subgroup 进行正态性检验
shapiro_results <- by(data$`Total Distance (m)`,
                      data$group, shapiro.test)
print(shapiro_results)

data_long <- summarySE(data, measurevar="Total Distance (m)", groupvars=c("group"))


data$group
str(data)
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
Process_comparisons <- list(c("control", "lps"))  ####


# 使用 t_test 
t_test_result <- data %>%
  t_test(`Total Distance (cm)` ~ group, comparisons = Process_comparisons) %>%
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

data_long <- summarySE(data, measurevar="Total Distance (m)", groupvars=c("group"))
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
Process_comparisons <- list(c("control", "lps"))  ####
####此处是直方图
g1 <- ggplot(data_long, aes(x = group, y = `Total Distance (m)`, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = `Total Distance (m)` - se, ymax = `Total Distance (m)` + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = data,aes(x = group, y = `Total Distance (m)`),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Total distance traveled (m)",x=NULL) +
  scale_y_continuous(labels = function(x) paste0("  ",x),
                     limits = c(0, 50),
                     expand = c(0, 0),
                     breaks = seq(0, 50, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g1


g1 <- g1 + geom_signif(comparisons = Process_comparisons, 
                       annotations = t_test_result$annotation,
                       step_increase = 0.1,
                       size = 0.3) # Adjust this value to control line thickness, default is 0.5
g1

ggsave("g1.pdf", g1, width = 60, height = 60, units = "mm")




# 平均速度 --------------------------------------------------------------------

combinations <- combn(unique(data$group), 2, simplify = FALSE)
Process_comparisons <- combinations  ####
data_long <- summarySE(data, measurevar="Average Speed (m/s)", groupvars=c("group"))

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(data$`Average Speed (m/s)`,
                      data$group, shapiro.test)
shapiro_results

t_test_result <- data %>%
  t_test(`Average Speed (m/s)` ~ group, comparisons = Process_comparisons) %>%
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
# wilcox_test_result <- data %>%
#   wilcox_test(`Average Speed (m/s)` ~ group,comparisons = Process_comparisons) %>%
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


g2 <- ggplot(data_long, aes(x = group, y = `Average Speed (m/s)`, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = `Average Speed (m/s)` - se, ymax = `Average Speed (m/s)` + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = data,aes(x = group, y = `Average Speed (m/s)`),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Mean speed (m/sec)",x=NULL) +
  scale_x_discrete(labels = c("control" = "Ctrl", ####\n  下一行显示的
                              "lps" = "LPS"
  ))+
  scale_y_continuous(limits = c(0, 0.15),
                     expand = c(0, 0),
                     breaks = seq(0, 0.15, by = 0.05)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())##+  #####这个可以去除顶部和右边的边框线

g2 <- g2+geom_signif(comparisons = Process_comparisons, 
                     annotations = t_test_result$annotation,
                     step_increase = 0.05,
                     textsize = 3,      # 调整此处数值以控制注释文本的大小
                     size = 0.3) # Adjust this value to control line thickness, default is 0.5
g2
