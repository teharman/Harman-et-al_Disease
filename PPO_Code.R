setwd("D:/Tyler Thesis/R Data Files/Immunity Data")

PPO_Data<-read.csv("PPO_Data.csv",header=T)
source("http://goo.gl/UUyEzD") #outlier code source
outlierKD(PPO_Data, PPO) #outlier code

library(ggpubr)
library(rstatix)
library(ggplot2)
library(tidyverse)
library(naniar)

PPO_Data$PPO<-log(PPO_Data$PPO)
model<-lm(PPO~Collection,data=PPO_Data)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
PPO_Data %>% levene_test(PPO~Treatment)
PPO.av<-aov(PPO~Treatment*Sym.State*Collection, data = PPO_Data)
summary(PPO.av)
TukeyHSD(PPO.av)

####Summer Collections####

PPO_Data$Treatment_Com<- with(PPO_Data, paste0(Sym.State, Treatment)) #merging column items together

Boxplot1<-ggplot(PPO_Data,mapping=aes(x=Treatment_Com,y=PPO,color=Treatment,linetype=Treatment))+
  geom_boxplot(alpha=0.3,fatten=2,lwd=1, outlier.size = 3)+
  labs(x="Symbiotic State",y="\u0394Abs 490nm (PPO)")+
  ylab(bquote('Abs 490nm (mg '~protein^-1*')'))+
  theme(plot.title = element_text(vjust=2))+
  theme_bw()+
  ylim(0,0.5)+
  theme(plot.title = element_text(color="black", size=20, face="bold.italic", vjust=4, hjust=0.5),
        axis.title.x = element_text(size=14,vjust=-2),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=14,vjust=6),
        plot.margin=unit(c(1,1,1,2),"cm"))+
  theme(legend.text = element_text(colour="black", size=15),
        legend.key.size = unit(1.5, "cm"),
        legend.title=element_blank(),
        legend.position = "right")+
  scale_x_discrete(labels=c("AposymbioticAmbient" = "Aposymbiotic 18°C", "AposymbioticElevated" = "Aposymbiotic 26°C","SymbioticAmbient" = "Symbiotic 18°C","SymbioticElevated"="Symbiotic 26°C"))
Boxplot1
