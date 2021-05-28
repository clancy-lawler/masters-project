library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(tidyverse)
library(ggbeeswarm)
library(readxl)
library(viridis)

theme_set(theme_cowplot()) #this sets the default theme of ggplot2 to something nice
survival_RNAi <- read_excel("survival and fertility.xlsx", sheet = "survival") #imports data from excel file
survival_RNAi #displays data
survival_RNAi_t <- gather(survival_RNAi, key = "vial", value = "survival", vial1:vial4) #converts data from "wide" format to "long" format
survival_RNAi_t <- mutate(survival_RNAi_t, prop = survival/50) #creates a new column of the proportional survival (divides survival by 50)
survival_RNAi_t #displays tidy data

(survival_RNAi.plot <- ggplot(survival_RNAi_t, aes(x = GAL4, y = prop)) + #loads data and sets some of the aesthetic groupings (i.e. GAL4 variable on the x-axis, etc.)
    geom_quasirandom(aes(colour = responder), method = "quasirandom", dodge.width = 0.7, width = 0.05, size = 1.7, alpha = 0.9) + #this adds the dots
    stat_summary(aes(group = responder), fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.6, size = 0.5, colour = "black", position = position_dodge(width = 0.7)) + #creates black bar for the mean of each group
    scale_y_continuous(limits = c(0,1), name = "survival", labels = scales::percent, expand = c(0,0)) + #varies the y-axis
    scale_x_discrete(name = NULL ) + #removes the name of the x-axis
    scale_colour_manual(values = c("royalblue2","red2")) + #changes the colours of the dots
    facet_wrap(.~gene, strip.position = "bottom") + #breaks up the plot into two parts based on the genes, and places gene name on bottom not top
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) + #various theme changes
    coord_cartesian(clip = "off") #removes clipping issues due to "expand" argument
)


fertility_RNAi <- read_excel("survival and fertility.xlsx", sheet = "fertility") #imports data from excel file
fertility_RNAi #displays data
fertility_RNAi_t <- gather(fertility_RNAi, key = "vial", value = "fertility", "1":"10") #converts data from "wide" format to "long" format
fertility_RNAi_t <- (fertility_RNAi_t %>% group_by(gene, responder, GAL4) %>% summarise(fertile = sum(fertility), infertile = 10-sum(fertility))) #counts number of fertile and infertile individuals and creates new columns with that info
fertility_RNAi_t <- gather(fertility_RNAi_t, key = "fertility", value = "count", fertile:infertile) #converts data from "wide" format to "long" format again because we made more columns
fertility_RNAi_t$fertility <- factor(fertility_RNAi_t$fertility, levels = c("infertile","fertile")) #orders the levels of fertility so our bars will stack in the order we want
fertility_RNAi_t #displays tidy data


fert_pal <- brewer.pal(n = 9, name = "PRGn") #creates a colour palette of 9 colours

fert_pal

(fertility_RNAi.plot <- ggplot(data = fertility_RNAi_t) + #data
    geom_bar(aes(y = count, x = responder, fill = fertility), stat = "identity", position = position_fill(reverse = F), alpha = 0.7) + #adds bars
    geom_text(aes(y = count, x = responder, group = fertility, label = ifelse(count >0, count, "")), stat = "identity", position = position_fill(vjust = 0.5), size = 3) + #adds numbers to bars
    scale_y_continuous(labels = scales::percent, expand = c(0,0), name = "males") + #changes to y-axis
    scale_fill_manual(values = fert_pal[c(2,8)]) + #use 2nd and 8th colours in the palette to fill the bars
    facet_wrap(gene~GAL4, nrow = 1) + #group variables together
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )  #various theme changes
)