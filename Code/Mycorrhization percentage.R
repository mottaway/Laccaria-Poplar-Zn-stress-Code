setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 
MYCORRHIZATIOND14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 28) 
head(MYCORRHIZATIOND14)


MYCORRHIZATIOND14$Tissue = as.factor(MYCORRHIZATIOND14$Tissue)
MYCORRHIZATIOND14$Zn <- factor(MYCORRHIZATIOND14$Zn, levels=c("Control", "1.5 mM Zn"))
MYCORRHIZATIOND14$`MYCORRHIZATIOND14Mean` = as.numeric(MYCORRHIZATIOND14$`MYCORRHIZATIOND14Mean`)
MYCORRHIZATIOND14$`MYCORRHIZATIOND14SE` = as.numeric(MYCORRHIZATIOND14$`MYCORRHIZATIOND14SE`)
MYCORRHIZATIOND14$`MYCORRHIZATIOND14Output` = as.numeric(MYCORRHIZATIOND14$`MYCORRHIZATIOND14Output`)


Tissue <- MYCORRHIZATIOND14$Tissue
Zn <- MYCORRHIZATIOND14$Zn
Mean<- MYCORRHIZATIOND14$`MYCORRHIZATIOND14Mean`
SE<- MYCORRHIZATIOND14$`MYCORRHIZATIOND14SE`
Output <- MYCORRHIZATIOND14$`MYCORRHIZATIOND14Output`

library(tidyverse)
library(ggplot2)
library(ggsignif)

x <- t.test(Output ~ Zn, data = MYCORRHIZATIOND14)
x
capture.output(x ,file="MYCORRHIZATIOND14 D14 ttest Paper.doc") 


#add symbols based on output of t.test with
#cutpoints = c(0, 0.001, 0.01, 0.05),
#symbols = c("***", "**", "*"))

overview <- data.frame (p_value = c(0.006861), p.adj.signif =c("**"))
overview

list <- data.frame (y_position = c(90), xmin= c(0.75), xmax= c(1.25))
df <- overview %>%   mutate (list)
df



figure <-
ggplot(MYCORRHIZATIOND14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_signif( xmin = df$xmin, xmax = df$xmax, y_position = df$y_position, annotation = df$p.adj.signif, tip_length = 0.05, size = 1, textsize = 10) +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "Mycorrhization %", x= " ", y = "Mycorrhization %") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by = 20)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure
