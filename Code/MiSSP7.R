setwd("C:/Users/jaswinne/OneDrive - Vrije Universiteit Brussel/Varia/Documents/Coculture 23-24")
getwd() 

MISSP7D14 = readxl::read_excel("PaperJSMOKV.xlsx", sheet = 18) 
head(MISSP7D14)

MISSP7D14$Tissue = as.factor(MISSP7D14$Tissue)
MISSP7D14$Zn <- factor(MISSP7D14$Zn, levels=c("Control", "1.5 mM Zn"))
MISSP7D14$`MISSP7D14log2` = as.numeric(MISSP7D14$`MISSP7D14log2`)
MISSP7D14$`MISSP7D14Mean` = as.numeric(MISSP7D14$`MISSP7D14Mean`)
MISSP7D14$`MISSP7D14SE` = as.numeric(MISSP7D14$`MISSP7D14SE`)

Tissue <- MISSP7D14$Tissue
Zn <- MISSP7D14$Zn
log2 <- MISSP7D14$`MISSP7D14log2`
Mean<- MISSP7D14$`MISSP7D14Mean`
SE<- MISSP7D14$`MISSP7D14SE`

qqnorm(log2, pch = 1, frame = F)
qqline(log2, lwd = 2)
shapiro.test(log2)

x <- aov(log2 ~ Tissue * Zn)
w <- summary(x)
w
capture.output(w ,file="MISSP7 D14 Anova Paper.doc") #store data

y <- TukeyHSD(x)
y

overview <- data.frame(y$`Tissue:Zn`)

library(rstatix)
z <- add_significance(overview,
  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("***", "**", "*", ".", " "))

Statistics <- z[4:5]
Statistics
write.csv2(Statistics, file = "MISSP7 D14 Tukey paper.csv")

library(ggplot2)
library(ggsignif)


list <- data.frame (y_position = c(NA, 1.5, NA, NA, 1.5, 2), xmin= c(NA, 0.75, NA, NA, 1.75, 1.25), xmax= c(NA, 1.25, NA, NA, 2.25, 2.25))
df <- Statistics %>%   mutate (list)
df

figure <-
ggplot(MISSP7D14, aes(x = Tissue, y = Mean, fill=Zn, color = Zn)) +
geom_bar(stat='identity', position="dodge") +
geom_signif( xmin = df$xmin, xmax = df$xmax, y_position = df$y_position, annotation = df$p.adj.signif, tip_length = 0.05, size = 1, textsize = 10) +
geom_errorbar( aes(ymin=Mean-SE, ymax= Mean+SE ), width=0.1, size=2, position=position_dodge(1)) +
labs(title = "MiSSP7", x= " ", y = "Log2 (Relative expression)") +
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





