
# TNF-α -------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(Rmisc)
library(readxl)
TNF_α <- read_excel("TNF-α.xlsx", sheet = "Sheet2")

ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
Process_comparisons <- list(c("control", "lps"))  ####
TNF_α


min(TNF_α$Value)
max(TNF_α$Value)
TNF_α$Group <- gsub("[^A-Za-z]", "",TNF_α$Group)   ##提取group中的数字部分  比如c  l
TNF_α$Group[TNF_α$Group=="C"] <- "control"
TNF_α$Group[TNF_α$Group=="L"] <- "lps"


# 对每个 group 进行正态性检验
shapiro_results <- by(TNF_α$Value, 
                      TNF_α$Group, shapiro.test)
shapiro_results
t.test(TNF_α$Value~Group,data = TNF_α)
t_test_result <- TNF_α %>%
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



library(ggplot2)

####此处是直方图
TNF_α <- as.data.frame(TNF_α)
TNF_α_long <- summarySE(TNF_α, measurevar="Value", groupvars=c("Group"))
str(TNF_α_long)

g1 <- ggplot(TNF_α_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Value - se, ymax = Value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = TNF_α,aes(x = Group, y = Value),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="TNF-α/β-Actin",x=NULL) +
  scale_y_continuous(limits = c(0, 2),
                     expand = c(0, 0)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g1 <- g1+geom_signif(comparisons = Process_comparisons, 
            annotations = t_test_result$annotation,
            step_increase = 0.05,
            textsize = 3,      # 调整此处数值以控制注释文本的大小
            size = 0.3) # Adjust this value to control line thickness, default is 0.5



g1
ggsave("g1.pdf", g2, width = 50, height = 50, units = "mm")



# IL-1β -------------------------------------------------------------------
library(readxl)
library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(Rmisc)
IL_1β <- read_excel("IL-1β.xlsx", sheet = "Sheet2")
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
Process_comparisons <- list(c("control", "lps"))  ####
IL_1β


min(IL_1β$Value)
max(IL_1β$Value)
IL_1β$Group <- gsub("[^A-Za-z]", "",IL_1β$Group)   ##提取group中的数字部分  比如c  l
IL_1β$Group[IL_1β$Group=="C"] <- "control"
IL_1β$Group[IL_1β$Group=="L"] <- "lps"
# 对每个 group 进行正态性检验
shapiro_results <- by(IL_1β$Value, 
                      IL_1β$Group, shapiro.test)
shapiro_results

t_test_result <- IL_1β %>%
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

####此处是直方图
IL_1β <- as.data.frame(IL_1β)
IL_1β_long <- summarySE(IL_1β, measurevar="Value", groupvars=c("Group"))
str(IL_1β_long)

g2 <- ggplot(IL_1β_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Value - se, ymax = Value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = IL_1β,aes(x = Group, y = Value),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="IL-1β/β-Actin",x=NULL) +
  scale_y_continuous(limits = c(0, 2),
                     expand = c(0, 0)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())  #####这个可以去除顶部和右边的边框线

g2 <- g2+geom_signif(comparisons = Process_comparisons, 
                     annotations = t_test_result$annotation,
                     step_increase = 0.05,
                     textsize = 3,      # 调整此处数值以控制注释文本的大小
                     size = 0.3) # Adjust this value to control line thickness, default is 0.5

g2
  
  
# ggsave("g2.pdf", g2, width = 50, height = 50, units = "mm")

# ggsave("g2.pdf",device = cairo_pdf,width =3.15, height =2.36)

# IL-6 --------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(Rmisc)
library(readxl)
IL_6 <- read_excel("IL-6.xlsx", sheet = "Sheet2")


ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
Process_comparisons <- list(c("control", "lps"))  ####


min(IL_6$Value)
max(IL_6$Value)
IL_6$Group <- gsub("[^A-Za-z]", "",IL_6$Group)   ##提取group中的数字部分  比如c  l
IL_6$Group[IL_6$Group=="C"] <- "control"
IL_6$Group[IL_6$Group=="L"] <- "lps"

# 对每个 group 进行正态性检验
shapiro_results <- by(IL_6$Value, 
                      IL_6$Group, shapiro.test)
# 输出结果
print(shapiro_results)

t.test(IL_6$Value~Group,data = IL_6)




library(ggplot2)

####此处是直方图
IL_6 <- as.data.frame(IL_6)
IL_6_long <- summarySE(IL_6, measurevar="Value", groupvars=c("Group"))
str(IL_6_long)

g3 <- ggplot(IL_6_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Value - se, ymax = Value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = IL_6,aes(x = Group, y = Value),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="IL-6/β-Actin",x=NULL) +
  scale_y_continuous(limits = c(0, 2),
                     expand = c(0, 0)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,
                     method = "t.test",###此处方法默认是Kruskal-wallis
                     vjust = 0.02,label.x = 1,data = IL_6)
g3

# ggsave("IL-6.pdf", g2, width = 50, height = 50, units = "mm")

# ggsave("g3.pdf",device = cairo_pdf,width =3.15, height =2.36)

####通常在-1到1之间，其中0表示文本的基线与位置之间的对齐，负值表示文本向上移动，
####正值表示文本向下移动。
####label.x 是 stat_compare_means() 函数的一个参数，用于控制比较结果标签的水平位置。
####这个参数接受一个数值，指定了标签的水平位置，通常在0到1之间，其中0表示标签位于比较组的左侧，
####1表示标签位于比较组的右侧。
