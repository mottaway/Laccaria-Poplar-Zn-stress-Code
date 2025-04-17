setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 
TPS21D14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 21) 
head(TPS21D14)


TPS21D14$Tissue = as.factor(TPS21D14$Tissue)
TPS21D14$Zn <- factor(TPS21D14$Zn, levels=c("Control", "1.5 mM Zn"))
TPS21D14$`TPS21D14Mean` = as.numeric(TPS21D14$`TPS21D14Mean`)
TPS21D14$`TPS21D14SE` = as.numeric(TPS21D14$`TPS21D14SE`)
TPS21D14$`TPS21D14Output` = as.numeric(TPS21D14$`TPS21D14Output`)
TPS21D14$`TPS21D14log2` = as.numeric(TPS21D14$`TPS21D14log2`)



Tissue <- TPS21D14$Tissue
Zn <- TPS21D14$Zn
Mean<- TPS21D14$`TPS21D14Mean`
SE<- TPS21D14$`TPS21D14SE`
Output <- TPS21D14$`TPS21D14Output`
log2 <- TPS21D14$`TPS21D14log2`


library(tidyverse)
library(ggplot2)
library(ggsignif)

x <- t.test(log2 ~ Zn, data = TPS21D14)
x
capture.output(x ,file="TPS21D14 ttest Paper.doc") 

#add symbols based on output of t.test with
#cutpoints = c(0, 0.001, 0.01, 0.05),
#symbols = c("***", "**", "*"))



figure <-
ggplot(TPS21D14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "TPS21", x= " ", y = " Log2 (Relative expression)") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(-4, 2.5), breaks=seq(-4, 2, by = 1)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure


