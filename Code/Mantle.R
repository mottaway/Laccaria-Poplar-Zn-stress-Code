setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 
MANTLED14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 24) 
head(MANTLED14)

MANTLED14$Tissue = as.factor(MANTLED14$Tissue)
MANTLED14$Zn <- factor(MANTLED14$Zn, levels=c("Control", "1.5 mM Zn"))
MANTLED14$`MANTLED14Mean` = as.numeric(MANTLED14$`MANTLED14Mean`)
MANTLED14$`MANTLED14SE` = as.numeric(MANTLED14$`MANTLED14SE`)
MANTLED14$`MANTLED14Output` = as.numeric(MANTLED14$`MANTLED14Output`)

Tissue <- MANTLED14$Tissue
Zn <- MANTLED14$Zn
Mean<- MANTLED14$`MANTLED14Mean`
SE<- MANTLED14$`MANTLED14SE`
Output <- MANTLED14$`MANTLED14Output`

library(tidyverse)
library(ggplot2)
library(ggsignif)

x <- t.test(Output ~ Zn, data = MANTLED14)
x
capture.output(x ,file="MANTLE D14 t-test Paper.doc") 

#add symbols based on output of t.test with
#cutpoints = c(0, 0.001, 0.01, 0.05),
#symbols = c("***", "**", "*"))

overview <- data.frame (p_value = c(0.01461), p.adj.signif =c("*"))
overview

list <- data.frame (y_position = c(25), xmin= c(0.75), xmax= c(1.25))
df <- overview %>%   mutate (list)
df



figure <-
ggplot(MANTLED14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_signif( xmin = df$xmin, xmax = df$xmax, y_position = df$y_position, annotation = df$p.adj.signif, tip_length = 0.05, size = 1, textsize = 10) +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "Mantle", x= " ", y = "Mantle thickness (µm)") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(0, 27), breaks=seq(0, 25, by = 5)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure


