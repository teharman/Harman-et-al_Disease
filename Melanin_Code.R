setwd("D:/Tyler Thesis/R Data Files/Immunity Data")

Melanin_Data<-read.csv("Melanin_Data.csv",header=T)
source("http://goo.gl/UUyEzD") #outlier code source
outlierKD(Melanin_Data, Melanin_mg) #outlier code

library(ggpubr)
library(rstatix)
library(ggplot2)
library(tidyverse)
library(naniar)

Melanin_Data$Melanin_mg<-log(Melanin_Data$Melanin_mg)
model<-lm(Melanin_mg~Collection*Treatment*Sym.State,data=Melanin_Data)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
Melanin_Data %>% levene_test(Melanin_mg~Collection*Treatment*Sym.State)
Melanin.av<-aov(Melanin_mg~Treatment*Sym.State*Collection, data = Melanin_Data)
summary(Melanin.av)
TukeyHSD(Melanin.av)

####Treatment:Sym State####

Melanin_Data$Treatment_Com<- with(Melanin_Data, paste0(Sym.State, Treatment)) #merging column items together

Boxplot1<-ggplot(Melanin_Data,mapping=aes(x=Treatment_Com,y=Melanin_mg,color=Treatment,linetype=Treatment))+
  geom_boxplot(alpha=0.3,fatten=2,lwd=1, outlier.size = 3)+
  labs(x="Symbiotic State",y="Melanin (mg/cm^2)")+
  ylab(bquote('Melanin (mg '~cm^-2*')'))+
  theme(plot.title = element_text(vjust=2))+
  theme_bw()+
  ylim(0,0.05)+
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
  scale_x_discrete(labels=c("AposymbioticAmbient" = "Aposymbiotic 18°C", "AposymbioticElevated" = "Aposymbiotic 26°C","SymbioticAmbient" = "Symbiotic 18°C","SymbioticElevated"="Symbiotic 26°C"))+
  annotate(geom="text",x=1,y=0.05,label="A",size=10)+
  annotate(geom="text",x=3,y=0.05,label="B",size=10)+
  annotate(geom="text",x=2,y=0.05,label="AB",size=10)+
  annotate(geom="text",x=4,y=0.05,label="AB",size=10)
Boxplot1

####Sym State:Collection####

Melanin_Data$Sym_State_Com<- with(Melanin_Data, paste0(Sym.State, Collection))

Boxplot2<-ggplot(Melanin_Data,mapping=aes(x=Sym_State_Com,y=Melanin_mg,color=Sym.State,linetype=Sym.State))+
  geom_boxplot(alpha=0.3,fatten=2,lwd=1, outlier.size = 3)+
  labs(x="Symbiotic State",y="Melanin (mg/cm^2)")+
  ylab(bquote('Melanin (mg '~cm^-2*')'))+
  theme(plot.title = element_text(vjust=2))+
  theme_bw()+
  ylim(0,0.05)+
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
  scale_x_discrete(labels=c("AposymbioticWinter" = "Aposymbiotic Winter", "AposymbioticSummer" = "Aposymbiotic Summer","SymbioticWinter" = "Symbiotic Winter","SymbioticSummer"="Symbiotic Summer"))+
  annotate(geom="text",x=1,y=0.05,label="AB",size=10)+
  annotate(geom="text",x=3,y=0.05,label="AB",size=10)+
  annotate(geom="text",x=2,y=0.05,label="A",size=10)+
  annotate(geom="text",x=4,y=0.05,label="B",size=10)
Boxplot2
