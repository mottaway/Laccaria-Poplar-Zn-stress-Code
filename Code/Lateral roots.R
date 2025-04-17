setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 
ROOTTIPSD14TOTAL = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 27) 
head(ROOTTIPSD14TOTAL)


ROOTTIPSD14TOTAL$Tissue = as.factor(ROOTTIPSD14TOTAL$Tissue)
ROOTTIPSD14TOTAL$Zn <- factor(ROOTTIPSD14TOTAL$Zn, levels=c("Control", "1.5 mM Zn"))
ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALMean` = as.numeric(ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALMean`)
ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALSE` = as.numeric(ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALSE`)
ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALOutput` = as.numeric(ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALOutput`)


Tissue <- ROOTTIPSD14TOTAL$Tissue
Zn <- ROOTTIPSD14TOTAL$Zn
Mean<- ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALMean`
SE<- ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALSE`
Output <- ROOTTIPSD14TOTAL$`ROOTTIPSD14TOTALOutput`

library(tidyverse)
library(ggplot2)
library(ggsignif)

x <- t.test(Output ~ Zn, data = ROOTTIPSD14TOTAL)
x
capture.output(x ,file="ROOTTIPSD14TOTAL D14 ttest Paper.doc") 

#add symbols based on output of t.test with
#cutpoints = c(0, 0.001, 0.01, 0.05),
#symbols = c("***", "**", "*"))

overview <- data.frame (p_value = c(0.004127), p.adj.signif =c("**"))
overview

list <- data.frame (y_position = c(75), xmin= c(0.75), xmax= c(1.25))
df <- overview %>%   mutate (list)
df



figure <-
ggplot(ROOTTIPSD14TOTAL, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_signif( xmin = df$xmin, xmax = df$xmax, y_position = df$y_position, annotation = df$p.adj.signif, tip_length = 0.05, size = 1, textsize = 10) +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "Lateral roots", x= " ", y = "# Lateral roots") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(0, 80), breaks=seq(0, 80, by = 20)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure


