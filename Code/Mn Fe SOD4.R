setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 

MNFESOD4D14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 14) 
head(MNFESOD4D14)

MNFESOD4D14$Tissue = as.factor(MNFESOD4D14$Tissue)
MNFESOD4D14$Zn <- factor(MNFESOD4D14$Zn, levels=c("Control", "1.5 mM Zn"))
MNFESOD4D14$`MNFESOD4D14log2` = as.numeric(MNFESOD4D14$`MNFESOD4D14log2`)
MNFESOD4D14$`MNFESOD4D14Mean` = as.numeric(MNFESOD4D14$`MNFESOD4D14Mean`)
MNFESOD4D14$`MNFESOD4D14SE` = as.numeric(MNFESOD4D14$`MNFESOD4D14SE`)

Tissue <- MNFESOD4D14$Tissue
Zn <- MNFESOD4D14$Zn
log2 <- MNFESOD4D14$`MNFESOD4D14log2`
Mean<- MNFESOD4D14$`MNFESOD4D14Mean`
SE<- MNFESOD4D14$`MNFESOD4D14SE`

qqnorm(log2, pch = 1, frame = F)
qqline(log2, lwd = 2)
shapiro.test(log2)

x <- aov(log2 ~ Tissue * Zn)
w <- summary(x)
w
capture.output(w ,file="MNFESOD4 D14 Anova Paper.doc") #store data

y <- TukeyHSD(x)
y

overview <- data.frame(y$`Tissue:Zn`)

library(rstatix)
z <- add_significance(overview,
  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("***", "**", "*", ".", " "))

Statistics <- z[4:5]
Statistics

write.csv2(Statistics, file = "MNFESOD4 D14 Tukey paper.csv")

library(ggplot2)

figure <-
ggplot(MNFESOD4D14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = " Mn/Fe SOD4", x= " ", y = "Log2 (Relative expression)") +
scale_color_manual (values = c("blue1", "darkorange")) +
scale_fill_manual (values = c("blue1", "darkorange")) +
scale_y_continuous(limits = c(-2, 2.5), breaks=seq(-2, 2, by = 1)) +
theme_light() +
theme(plot.title = element_text(hjust=0.5, size=50), axis.title.y = element_text(size = 40), 
strip.text = element_text(colour = 'black', size = 25), axis.text.y= element_text(size = 30), axis.text.x = element_text(size = 40),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
axis.line.y = element_line(size = 0.5, colour = "black"), axis.line.x = element_line(size = 0.5, colour = "black"), 
legend.text = element_text(size=40), legend.title = element_blank()) 

figure



