# install the packages needed the first time through the Install package button

setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 

CATALASEACTIVITYD14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 22) 
head(CATALASEACTIVITYD14)

CATALASEACTIVITYD14$Tissue = as.factor(CATALASEACTIVITYD14$Tissue)
CATALASEACTIVITYD14$Zn <- factor(CATALASEACTIVITYD14$Zn, levels=c("Control", "1.5 mM Zn"))
CATALASEACTIVITYD14$`CATALASEACTIVITYD14Mean` = as.numeric(CATALASEACTIVITYD14$`CATALASEACTIVITYD14Mean`)
CATALASEACTIVITYD14$`CATALASEACTIVITYD14SE` = as.numeric(CATALASEACTIVITYD14$`CATALASEACTIVITYD14SE`)
CATALASEACTIVITYD14$`CATALASEACTIVITYD14Output` = as.numeric(CATALASEACTIVITYD14$`CATALASEACTIVITYD14Output`)

Tissue <- CATALASEACTIVITYD14$Tissue
Zn <- CATALASEACTIVITYD14$Zn
Mean<- CATALASEACTIVITYD14$`CATALASEACTIVITYD14Mean`
SE<- CATALASEACTIVITYD14$`CATALASEACTIVITYD14SE`
Output <- CATALASEACTIVITYD14$`CATALASEACTIVITYD14Output`

qqnorm(Output, pch = 1, frame = F)
qqline(Output, lwd = 2)
shapiro.test(Output)

x <- aov(Output ~ Tissue * Zn)
w <- summary(x)
w
capture.output(w ,file="CATALASEACTIVITY D14 Anova Paper.doc") #store data

y <- TukeyHSD(x)
y
overview <- data.frame(y$`Tissue:Zn`)


library(rstatix)

z <- add_significance(overview,
  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("***", "**", "*", ".", " "))
Statistics <- z[4:5]
Statistics

write.csv2(Statistics, file = "CATALASEACTIVITY D14 Tukey paper.csv")

library(ggplot2)
library(ggsignif)

list <- data.frame (y_position = c(NA, NA, NA, NA, 5.5, 6), xmin= c(NA, NA, NA, NA, 1.75, 1.25), xmax= c(NA, NA, NA, NA, 2.25, 2.25))
df <- Statistics %>%   mutate (list)
df

figure <-
ggplot(CATALASEACTIVITYD14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_signif( xmin = df$xmin, xmax = df$xmax, y_position = df$y_position, annotation = df$p.adj.signif, tip_length = 0.05,size = 1, textsize = 10) +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "Catalase enzyme capacity ", x= " ", y = "Enzyme Capacity (U/g)") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(0, 6.5), breaks=seq(0, 6, by = 1)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure
