setwd("D:/Tyler Thesis/R Data Files/Immunity Data")
PPO_Data<-read.csv("PPO_Data.csv",header=T)

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

model<-lm(PPO~Collection,data=PPO_Data)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

data.lm<-glm(PPO~Treatment*Sym.State*Collection,data = PPO_Data,family=Gamma(link="inverse"))
summary(data.lm)
tiff("PPO_Plot.tiff", height = 18, width = 18, units = 'cm', 
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

####Summer Collections####

PPO_Data$Treatment_Com<- with(PPO_Data, paste0(Sym.State, Treatment)) #merging column items together

Boxplot3<-ggplot(PPO_Data,mapping=aes(x=Treatment_Com,y=PPO,color=Treatment,linetype=Treatment))+
  geom_boxplot(alpha=0.3,fatten=2,lwd=1, outlier.size = 3)+
  labs(x="Symbiotic State + Temperature",y="\u0394Abs 490nm (PPO)")+
  ylab(bquote('Abs 490nm (mg '~protein^-1*')'))+
  theme(plot.title = element_text(vjust=2))+
  theme_bw()+
  ylim(0,0.5)+
  scale_x_discrete(labels=c("AposymbioticAmbient" = "Aposymbiotic 18?C", "AposymbioticElevated" = "Aposymbiotic 26?C","SymbioticAmbient" = "Symbiotic 18?C","SymbioticElevated"="Symbiotic 26?C"))+
  theme(plot.title = element_text(color="black", size=20, face="bold.italic", vjust=4, hjust=0.5),
        axis.title.x = element_text(size=14,vjust=-2),
        axis.text.x = element_text(size=12,angle = 45,hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=14,vjust=6),
        plot.margin=unit(c(0.5,0.5,0.5,1),"cm"))+
  theme(legend.text = element_text(colour="black", size=15),
        legend.key.size = unit(1.5, "cm"),
        legend.title=element_blank(),
        legend.position = "none")
Boxplot3
