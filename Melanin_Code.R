setwd("D:/Other/Tyler Thesis/R Data Files/Immunity Data")
Melanin_Data<-read.csv("Melanin_Data.csv",header=T)

library(ggpubr)
library(rstatix)
library(ggplot2)
library(tidyverse)
library(naniar)
library(lme4)
library(sjPlot)
library(sjmisc)
library(glmmTMB)
library(ggplot2)
library(tidyverse)
library(reshape)
library(ggpubr)
library(Rmisc)
library(lsmeans)
library(ggthemes)
library(stats)

source("http://goo.gl/UUyEzD") #outlier code source
outlierKD(Melanin_Data, Melanin_mg) #outlier code

#determination of normality
model<-lm(Melanin_mg~Collection*Treatment*Sym.State,data=Melanin_Data)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

data.lm<-glm(Melanin_mg~Treatment*Sym.State*Collection,data = Melanin_Data,family=Gamma(link="inverse"))
summary(data.lm)
tiff("Melanin_Plot.tiff", height = 18, width = 18, units = 'cm', 
     compression = "lzw", res = 300)
par(mfrow=c(2,2))
plot(data.lm)
dev.off()

PWC1<-lsmeans(data.lm,specs=c("Treatment","Sym.State"))
summary(PWC1)
rbind(pairs(PWC1),adjust="none")

PWC2<-lsmeans(data.lm,specs=c("Collection","Sym.State"))
summary(PWC2)
rbind(pairs(PWC2),adjust="none")

PWC3<-lsmeans(data.lm,specs=c("Treatment","Collection"))
summary(PWC3)
rbind(pairs(PWC3),adjust="none")

PWC4<-lsmeans(data.lm,specs=c("Treatment","Sym.State","Collection"))
summary(PWC4)
rbind(pairs(PWC4),adjust="none")

####Treatment:Sym State####

label1 <- "A[1]"
label2 <- "A[2]"
label3 <- "B[1]"
label4 <- "B[2]"

Melanin_Data$Treatment_Com<- with(Melanin_Data, paste0(Sym.State, Treatment, Collection)) #merging column items together

tiff("Melanin_Data.tiff", height = 12, width = 24, units = 'cm', 
     compression = "lzw", res = 300)

ggplot(Melanin_Data,mapping=aes(x=Treatment_Com,y=Melanin_mg,colour=Collection,linetype=Treatment))+
  geom_boxplot(alpha=0.3,fatten=2,lwd=1, outlier.size = 3)+
  labs(x="Symbiotic State",y="Melanin (mg/cm^2)")+
  ylab(bquote('Melanin (mg '~cm^-2*')'))+
  theme(plot.title = element_text(vjust=2))+
  theme_stata()+
  ylim(0,0.05)+
  facet_grid(. ~ Sym.State,scales="free")+
  theme(strip.text.x = element_text(size=15))+
  scale_color_manual(values = c("grey60","grey40"))+
  theme(plot.title = element_text(color="black", size=20, face="bold.italic", vjust=4, hjust=0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=6),
        plot.margin=unit(c(0.5,0.5,0.5,1),"cm"))+
  theme(axis.ticks.x = element_blank())+
  theme(legend.text = element_text(colour="black", size=12),
        legend.key.size = unit(1.5, "cm"),
        legend.title=element_blank(),
        legend.position = "right")+
  annotate(geom = "text", x = 1, y = 0.025, label = deparse(bquote(~A[1])),
           color = "grey55", size = 8,parse=T)+
  annotate(geom = "text", x = 1, y = 0.035, label = deparse(bquote(~A[2])),
           color = "grey55", size = 8,parse=T)+
  annotate(geom = "text", x = 3, y = 0.045, label = deparse(bquote(~B[1])),
           color = "grey55", size = 8,parse=T)+
  annotate(geom = "text", x = 4, y = 0.045, label = deparse(bquote(~B[2])),
           color = "grey55", size = 8,parse=T)
dev.off()
