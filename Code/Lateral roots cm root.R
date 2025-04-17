setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 
ROOTTIPSD14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 26) 
head(ROOTTIPSD14)


ROOTTIPSD14$Tissue = as.factor(ROOTTIPSD14$Tissue)
ROOTTIPSD14$Zn <- factor(ROOTTIPSD14$Zn, levels=c("Control", "1.5 mM Zn"))
ROOTTIPSD14$`ROOTTIPSD14Mean` = as.numeric(ROOTTIPSD14$`ROOTTIPSD14Mean`)
ROOTTIPSD14$`ROOTTIPSD14SE` = as.numeric(ROOTTIPSD14$`ROOTTIPSD14SE`)
ROOTTIPSD14$`ROOTTIPSD14Output` = as.numeric(ROOTTIPSD14$`ROOTTIPSD14Output`)


Tissue <- ROOTTIPSD14$Tissue
Zn <- ROOTTIPSD14$Zn
Mean<- ROOTTIPSD14$`ROOTTIPSD14Mean`
SE<- ROOTTIPSD14$`ROOTTIPSD14SE`
Output <- ROOTTIPSD14$`ROOTTIPSD14Output`

library(tidyverse)
library(ggplot2)
library(ggsignif)

x <- t.test(Output ~ Zn, data = ROOTTIPSD14)
x
capture.output(x ,file="ROOTTIPSD14 D14 ttest Paper.doc") 



figure <-
ggplot(ROOTTIPSD14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "Lateral roots/cm root", x= " ", y = "Lateral roots/cm root") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(0, 3.5), breaks=seq(0, 4, by = 1)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure



