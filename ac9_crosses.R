library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(tidyverse)
library(ggbeeswarm)
library(readxl)
library(viridis)
library(ggpubr)
library(rstatix)
theme_set(theme_cowplot())


setwd("~/Desktop/output/Cas9_crosses")
fertility <- NULL
fertility <- read_excel("vasa_crosses.xlsx")
fertility
fertility_t<-NULL
fertility_t <- gather(fertility, key = "vial", value = "offspring", vial1:vial8)

##test the different in mean number of generated offspring of individuals with or without sgRNAs
stat.test <- NULL
stat.test <- fertility_t %>%
  group_by(source)%>%
  t_test(offspring~responder)%>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p")%>%
  add_xy_position(x = "responder", dodge = 0.8)
stat.test


fertility.plot <-NULL
fertility.plot <- ggplot(fertility_t, aes(x = responder, y = offspring)) + #loads data and sets some of the aesthetic groupings (i.e. GAL4 variable on the x-axis, etc.)
  geom_quasirandom(aes(colour = responder), method = "quasirandom", dodge.width = 0.5, width = 0.05, size = 1.7, alpha = 0.9) + #this adds the dots
  stat_summary(aes(group = responder), fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.6, size = 0.5, colour = "black", position = position_dodge(width = 0.7)) + #creates black bar for the mean of each group
  scale_y_continuous(limits = c(0,250), name = "offspring", expand = c(0,0)) + #varies the y-axis
  scale_x_discrete(name = NULL ) + #removes the name of the x-axis
  scale_colour_manual(values = c("royalblue2","red2")) + #changes the colours of the dots
  facet_wrap(.~source, strip.position = "bottom") + 
  #breaks up the plot into two parts based on the genes, and places gene name on bottom not top
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) + #various theme changes  
  coord_cartesian(clip = "off")+
  stat_pvalue_manual(stat.test, label = "{p.signif}", tip.length = 0,  hide.ns = FALSE)#removes clipping issues due to "expand" argument

fertility.plot
