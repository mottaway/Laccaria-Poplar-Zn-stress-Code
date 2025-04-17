setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 
HARTIGNETD14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 25) 
head(HARTIGNETD14)

HARTIGNETD14$Tissue = as.factor(HARTIGNETD14$Tissue)
HARTIGNETD14$Zn <- factor(HARTIGNETD14$Zn, levels=c("Control", "1.5 mM Zn"))
HARTIGNETD14$`HARTIGNETD14Mean` = as.numeric(HARTIGNETD14$`HARTIGNETD14Mean`)
HARTIGNETD14$`HARTIGNETD14SE` = as.numeric(HARTIGNETD14$`HARTIGNETD14SE`)
HARTIGNETD14$`HARTIGNETD14Output` = as.numeric(HARTIGNETD14$`HARTIGNETD14Output`)

Tissue <- HARTIGNETD14$Tissue
Zn <- HARTIGNETD14$Zn
Mean<- HARTIGNETD14$`HARTIGNETD14Mean`
SE<- HARTIGNETD14$`HARTIGNETD14SE`
Output <- HARTIGNETD14$`HARTIGNETD14Output`

library(tidyverse)
library(ggplot2)
library(ggsignif)

x <- t.test(Output ~ Zn, data = HARTIGNETD14)
x
capture.output(x ,file="HARTIGNET D14 t-test Paper.doc") 


figure <-
ggplot(HARTIGNETD14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "Hartig Net", x= " ", y = "Hartig Net depth (µm)")+
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(0, 18), breaks=seq(0, 15, by = 5)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure



