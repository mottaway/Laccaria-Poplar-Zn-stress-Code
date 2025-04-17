setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 

SODACTIVITYD14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 23) 
head(SODACTIVITYD14)

SODACTIVITYD14$Tissue = as.factor(SODACTIVITYD14$Tissue)
SODACTIVITYD14$Zn <- factor(SODACTIVITYD14$Zn, levels=c("Control", "1.5 mM Zn"))
SODACTIVITYD14$`SODACTIVITYD14Mean` = as.numeric(SODACTIVITYD14$`SODACTIVITYD14Mean`)
SODACTIVITYD14$`SODACTIVITYD14SE` = as.numeric(SODACTIVITYD14$`SODACTIVITYD14SE`)
SODACTIVITYD14$`SODACTIVITYD14Output` = as.numeric(SODACTIVITYD14$`SODACTIVITYD14Output`)

Tissue <- SODACTIVITYD14$Tissue
Zn <- SODACTIVITYD14$Zn
Mean<- SODACTIVITYD14$`SODACTIVITYD14Mean`
SE<- SODACTIVITYD14$`SODACTIVITYD14SE`
Output <- SODACTIVITYD14$`SODACTIVITYD14Output`

qqnorm(Output, pch = 1, frame = F)
qqline(Output, lwd = 2)
shapiro.test(Output)

x <- aov(Output ~ Tissue * Zn)
w <- summary(x)
w
capture.output(w ,file="SODACTIVITY D14 Anova Paper.doc") #store data



y <- TukeyHSD(x)
y
overview <- data.frame(y$`Tissue:Zn`)


library(rstatix)
z <- add_significance(overview,
  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("***", "**", "*", ".", " "))
Statistics <- z[4:5]
Statistics

write.csv2(Statistics, file = "SODACTIVITY D14 Tukey paper.csv")

library(ggplot2)

figure <-
ggplot(SODACTIVITYD14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "SOD enzyme capacity", x= " ", y = "Enzyme Capacity (U/g)") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(0, 1200), breaks=seq(0, 1200, by = 200)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure


