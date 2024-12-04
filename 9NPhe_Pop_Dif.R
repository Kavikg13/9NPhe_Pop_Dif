# Null Hypotheses: 
# 
# H1: The means of BPM grouped by Exposure_Level are the same 
# H2: The means of BPM grouped by Parental_Population are the same 
# H3: There is no interaction between Exposure_Level and Parental_Population
#
#
library(tidyverse)
library(DescTools)
library(dplyr)
library(ggsignif)

Data.raw <- X9NPhe_Pop_Dif_2024_Trial_Statistics_Cardiology
Data.raw$Exposure_Level_μM <- as.factor(Data.raw$"0")
Data.raw$Parental_Pop <- as.factor(Data.raw$`NBH F1`)
Data.raw$BPM <- Data.raw$'98.339357321393294'
Data = subset(Data.raw, select = c(Exposure_Level_μM,Parental_Pop,BPM))

#Boxplot 

ggplot(Data, aes(x = Parental_Pop, y = BPM, color = Exposure_Level_μM)) +
  geom_boxplot()+
  ggtitle("Affect of 9NPhe on Heart Rate (BPM)") +
  geom_signif(comparisons = list(c("")))

ggplot(Data, aes(x = Exposure_Level_μM, y = BPM, color = Parental_Pop)) +
  geom_boxplot() +
  ggtitle("")

#2 Histograms to visualize data 
plot(BPM ~ Parental_Pop + Exposure_Level_μM, data = Data)

#Visualize distribution
ggplot(Data, aes(x = BPM)) +
  geom_histogram() + 
  xlim(65,115) +
  facet_wrap(~Exposure_Level_μM, ncol = 1)

ggplot(Data, aes(x = BPM)) +
  geom_histogram() + 
  xlim(68,110) +
  facet_wrap(~Parental_Pop, ncol = 1)

#2-Way Anova
twoANOVA <- aov(BPM ~ Exposure_Level_μM * Parental_Pop, data = Data)
summary(twoANOVA)


 #Post Hoc
DunnettTest(x = Data$BPM, g = Data$Exposure_Level_μM)
TukeyHSD(twoANOVA)
ScheffeTest(twoANOVA)



#####Just NBH SCO 
Data.NBH.SCO <- subset(Data, Parental_Pop %in% c("SCO","NBH F1"))

ggplot(Data.NBH.SCO, aes(x = Parental_Pop, y = BPM, color = Exposure_Level_μM)) +
  geom_boxplot()+
  ggtitle("Affect of 9NPhe on Heart Rate (BPM)") +
  geom_signif(comparisons = list(c("")))

#Visualize Data
ggplot(Data.NBH.SCO, aes(x = Exposure_Level_μM,y = BPM)) + 
  geom_boxplot(aes(group = Exposure_Level_μM))

plot(lm(BPM~Exposure_Level_μM, data = Data.NBH.SCO))

#Visualize Normal Distribution 
ggplot(Data.NBH.SCO, aes(x = BPM)) +
  geom_histogram() + 
  xlim(65, 115) + 
  facet_wrap(~Exposure_Level_μM, ncol = 1)

#Shapiro Wilk Test for Normal Distrubution 
ShW.0 <- subset(Data.NBH.SCO, Exposure_Level_μM == 0)
shapiro.test(ShW.0$BPM)

ShW.1 <- subset(Data.NBH.SCO, Exposure_Level_μM == 5)
shapiro.test(ShW.1$BPM)

ShW.5 <- subset(Data.NBH.SCO, Exposure_Level_μM == 20)
shapiro.test(ShW.5$BPM)

#Test for Variance 
#Test for Variance 
Data.NBH.SCO %>%
  group_by(Exposure_Level_μM) %>%
  summarize(var(BPM))

Data.NBH.SCO %>%
  group_by(Parental_Pop) %>%
  summarize(var(BPM))

#Bartlett Test
bartlett.test(BPM ~ Exposure_Level_μM, data = Data.NBH.SCO)
bartlett.test(BPM ~ Parental_Pop, data = Data.NBH.SCO)
                            

#Two Way Anova 
twoANOVA <- aov(BPM ~ Exposure_Level_μM * Parental_Pop, data = Data.NBH.SCO)
summary(twoANOVA) 
twoANOVA.2 <- aov(BPM ~ Exposure_Level_μM + Parental_Pop + Exposure_Level_μM:Parental_Pop, data = Data.NBH.SCO)
summary(twoANOVA.2) 
TukeyHSD(twoANOVA)
DunnettTest(x = Data.NBH.SCO$BPM, g = Data.NBH.SCO$Exposure_Level_μM)

#Scheirer-Ray-Hare 
install.packages('rcompanion')
library(rcompanion)
scheirerRayHare(BPM ~ Exposure_Level_μM + Parental_Pop, data = Data.NBH.SCO)

scheirerRayHare(BPM ~ Exposure_Level_μM + Parental_Pop + Exposure_Level_μM:Parental_Pop, data = Data.NBH.SCO)

library(FSA)
dunnTest(BPM ~ Parental_Pop, data = Data.NBH.SCO, method = "bonferroni")

#Just NBH SCO KC
Data.NBH.SCO.KC <- subset(Data, Parental_Pop %in% c("SCO","NBH F1", "KC P"))
Data.NBH.SCO.KC <- Data.NBH.SCO.KC[!(Data.NBH.SCO.KC$Exposure_Level_μM %in% "20"),]

ggplot(Data.NBH.SCO.KC, aes(x = Parental_Pop, y = BPM, color = Exposure_Level_μM)) +
  geom_boxplot()+
  ggtitle("Affect of 9NPhe on Heart Rate (BPM)") +
  geom_signif(comparisons = list(c("")))

#Visualize Data
ggplot(Data.NBH.SCO.KC, aes(x = Exposure_Level_μM,y = BPM)) + 
  geom_boxplot(aes(group = Exposure_Level_μM))

plot(lm(BPM~Exposure_Level_μM, data = Data.NBH.SCO.KC))

#Visualize Normal Distribution 
ggplot(Data.NBH.SCO.KC, aes(x = BPM)) +
  geom_histogram() + 
  xlim(65, 115) + 
  facet_wrap(~Exposure_Level_μM, ncol = 1)

#Shapiro Wilk Test for Normal Distrubution 
ShW.0 <- subset(Data.NBH.SCO.KC, Exposure_Level_μM == 0)
shapiro.test(ShW.0$BPM)

ShW.1 <- subset(Data.NBH.SCO.KC, Exposure_Level_μM == 5)
shapiro.test(ShW.1$BPM)

#Test for Variance 
#Test for Variance 
Data.NBH.SCO.KC %>%
  group_by(Exposure_Level_μM) %>%
  summarize(var(BPM))

Data.NBH.SCO.KC %>%
  group_by(Parental_Pop) %>%
  summarize(var(BPM))

#Bartlett Test
bartlett.test(BPM ~ Exposure_Level_μM, data = Data.NBH.SCO.KC)
bartlett.test(BPM ~ Parental_Pop, data = Data.NBH.SCO.KC)


#Two Way Anova 
twoANOVA <- aov(BPM ~ Exposure_Level_μM * Parental_Pop, data = Data.NBH.SCO.KC)
summary(twoANOVA) 
TukeyHSD(twoANOVA)
DunnettTest(x = Data.NBH.SCO.KC$BPM, g = Data.NBH.SCO.KC$Exposure_Level_μM)

#Scheirer-Ray-Hare 

SRH <- scheirerRayHare(BPM ~ Exposure_Level_μM * Parental_Pop, data = Data.NBH.SCO.KC)
library(FSA)
dunnTest(BPM ~ Exposure_Level_μM, data = Data.NBH.SCO.KC, method = "bonferroni")
DunnettTest()

#Just NBH 
Data.NBH <- subset(Data, Parental_Pop == "NBH F1")
#Visualize Data
ggplot(Data.NBH, aes(x = Exposure_Level_μM,y = BPM)) + 
  geom_boxplot(aes(group = Exposure_Level_μM))

#Visualize Normal Distribution 
ggplot(Data.NBH, aes(x = BPM)) +
  geom_histogram() + 
  facet_wrap(~Exposure_Level_μM, ncol = 1)

#Shapiro Wilk Test for Normal Distrubution 
ShW.0 <- subset(Data.NBH, Exposure_Level_μM == 0)
shapiro.test(ShW.0$BPM)

ShW.5 <- subset(Data.NBH, Exposure_Level_μM == 5)
shapiro.test(ShW.5$BPM)

ShW.20 <- subset(Data.NBH, Exposure_Level_μM == 20)
shapiro.test(ShW.20$BPM)


#Test for Equal Variance 
Data.NBH %>%
  group_by(Exposure_Level_μM) %>%
  summarize(var(BPM))

bartlett.test(BPM ~ Exposure_Level_μM, data = Data.NBH)

#Kruskal-Wallis Test 
KW.test <- kruskal.test(Data.NBH$BPM, Data.NBH$Exposure_Level_μM)
print(KW.test)

#Post hoc = Dunnet Test 
DunnettTest(x = Data.NBH$BPM, g = Data.NBH$Exposure_Level_μM)
DunnettTest(x = Data.NBH$BPM, g = Data.NBH$Exposure_Level_μM) %>% plot()

#Just SCO 
Data.SCO <- subset(Data, Parental_Pop == "SCO")
#Visualize Data
ggplot(Data.SCO, aes(x = Exposure_Level_μM,y = BPM)) + 
  geom_boxplot(aes(group = Exposure_Level_μM))

#Visualize Normal Distribution 
ggplot(Data.SCO, aes(x = BPM)) +
  geom_histogram() + 
  facet_wrap(~Exposure_Level_μM, ncol = 1)

#Shapiro Wilk Test for Normal Distrubution 
ShW.0 <- subset(Data.SCO, Exposure_Level_μM == 0)
shapiro.test(ShW.0$BPM)

ShW.5 <- subset(Data.SCO, Exposure_Level_μM == 5)
shapiro.test(ShW.5$BPM)

ShW.20 <- subset(Data.SCO, Exposure_Level_μM == 20)
shapiro.test(ShW.20$BPM)


#Test for Equal Variance 
Data.SCO %>%
  group_by(Exposure_Level_μM) %>%
  summarize(var(BPM))

bartlett.test(BPM ~ Exposure_Level_μM, data = Data.SCO)

#Kruskal-Wallis Test 
KW.test <- kruskal.test(Data.SCO$BPM, Data.SCO$Exposure_Level_μM)
print(KW.test)

#Post hoc = Dunnet Test 
DunnettTest(x = Data.SCO$BPM, g = Data.SCO$Exposure_Level_μM)
DunnettTest(x = Data.SCO$BPM, g = Data.SCO$Exposure_Level_μM) %>% plot()

#anova? 

aov.sco <- aov(BPM ~ Exposure_Level_μM, data = Data.SCO)
summary(aov.sco)
TukeyHSD(aov.sco)
