setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 
TPS16D14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 20) 
head(TPS16D14)


TPS16D14$Tissue = as.factor(TPS16D14$Tissue)
TPS16D14$Zn <- factor(TPS16D14$Zn, levels=c("Control", "1.5 mM Zn"))
TPS16D14$`TPS16D14Mean` = as.numeric(TPS16D14$`TPS16D14Mean`)
TPS16D14$`TPS16D14SE` = as.numeric(TPS16D14$`TPS16D14SE`)
TPS16D14$`TPS16D14Output` = as.numeric(TPS16D14$`TPS16D14Output`)
TPS16D14$`TPS16D14log2` = as.numeric(TPS16D14$`TPS16D14log2`)



Tissue <- TPS16D14$Tissue
Zn <- TPS16D14$Zn
Mean<- TPS16D14$`TPS16D14Mean`
SE<- TPS16D14$`TPS16D14SE`
Output <- TPS16D14$`TPS16D14Output`
log2 <- TPS16D14$`TPS16D14log2`


library(tidyverse)
library(ggplot2)
library(ggsignif)

x <- t.test(log2 ~ Zn, data = TPS16D14)
x
capture.output(x ,file="TPS16D14 ttest Paper.doc") 

#add symbols based on output of t.test with
#cutpoints = c(0, 0.001, 0.01, 0.05),
#symbols = c("***", "**", "*"))

overview <- data.frame (p_value = c(0.005788), p.adj.signif =c("**"))
overview

list <- data.frame (y_position = c(2), xmin= c(0.75), xmax= c(1.25))
df <- overview %>%   mutate (list)
df



figure <-
ggplot(TPS16D14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_signif( xmin = df$xmin, xmax = df$xmax, y_position = df$y_position, annotation = df$p.adj.signif, tip_length = 0.05, size = 1, textsize = 10) +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "TPS16", x= " ", y = " Log2 (Relative expression)") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(-4.5, 2.5), breaks=seq(-4, 2, by = 1)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure


