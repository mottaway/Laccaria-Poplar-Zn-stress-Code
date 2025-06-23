setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 

NOXAD14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 16) 
head(NOXAD14)

NOXAD14$Tissue = as.factor(NOXAD14$Tissue)
NOXAD14$Zn <- factor(NOXAD14$Zn, levels=c("Control", "1.5 mM Zn"))
NOXAD14$`NOXAD14log2` = as.numeric(NOXAD14$`NOXAD14log2`)
NOXAD14$`NOXAD14Mean` = as.numeric(NOXAD14$`NOXAD14Mean`)
NOXAD14$`NOXAD14SE` = as.numeric(NOXAD14$`NOXAD14SE`)

Tissue <- NOXAD14$Tissue
Zn <- NOXAD14$Zn
log2 <- NOXAD14$`NOXAD14log2`
Mean<- NOXAD14$`NOXAD14Mean`
SE<- NOXAD14$`NOXAD14SE`

qqnorm(log2, pch = 1, frame = F)
qqline(log2, lwd = 2)
shapiro.test(log2)

x <- aov(log2 ~ Tissue * Zn)
w <- summary(x)
w
capture.output(w ,file="NOXA D14 Anova Paper.doc") #store data

y <- TukeyHSD(x)
y

overview <- data.frame(y$`Tissue:Zn`)

library(rstatix)

z <- add_significance(overview,
  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("***", "**", "*", ".", " "))

Statistics <- z[4:5]
Statistics

write.csv2(Statistics, file = "NOXA D14 Tukey paper.csv")

library(ggplot2)
library(ggsignif)



figure <-
ggplot(NOXAD14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "NoxA", x= " ", y = "Log2 (Relative expression)") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(-2, 2.5), breaks=seq(-2, 4, by = 1)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure
