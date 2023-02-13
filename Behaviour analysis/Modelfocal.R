library(multcomp)
library(ggplot2)
library(readxl)
library(ggsignif)
library(ggpubr)

femalesfocal <- read_excel("//NAS/home/Antoine/Projects/Social closure/Focal observations/Data_observations_focal.xlsx")
femalesfocal <- femalesfocal[femalesfocal$treatment %in% c("Control", "Intrapopulation", "Interpopulation"), ]
femalesfocal$treatment <- factor (femalesfocal$treatment, levels = c("Control", "Intrapopulation", "Interpopulation"))

### Plot the behaviors by treatment ###
femalesfocal$Rprop <- femalesfocal$R/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Rprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Resting") +
  theme_bw() +
  geom_bracket(xmin = 1, xmax = 3, y.position = 1.2, label = "***") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 1.1, label = "***")

femalesfocal$Wprop <- femalesfocal$W/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Wprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Walking") +
  theme_bw() +
  geom_bracket(xmin = 1, xmax = 3, y.position = 1.2, label = "***") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 1.1, label = "***")

femalesfocal$Dprop <- femalesfocal$D/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Dprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Digging") +
  theme_bw() +
  geom_bracket(xmin = 1, xmax = 3, y.position = 1.2, label = "***")

femalesfocal$Fprop <- femalesfocal$F/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Fprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Feeding") +
  theme_bw() +
  geom_bracket(xmin = 1, xmax = 3, y.position = 1.2, label = "***") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 1.1, label = "***")

femalesfocal$Cprop <- femalesfocal$C/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Cprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Cleaning") +
  theme_bw()

femalesfocal$Sprop <- femalesfocal$S/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Sprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Shuffling") +
  theme_bw()

femalesfocal$Gprop <- femalesfocal$G/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Gprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Grooming") +
  theme_bw()

femalesfocal$Copprop <- femalesfocal$Cop/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Cprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Copulating") +
  theme_bw()

femalesfocal$Canprop <- femalesfocal$Can/femalesfocal$total
ggplot (femalesfocal, aes (x=treatment, y=Canprop)) +
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of time", title = "Cannibalising") +
  theme_bw()

### GLMs with binomial data ###
restbin <- cbind (femalesfocal$R, femalesfocal$total-femalesfocal$R)
modelrest <- glm(restbin ~ femalesfocal$treatment, binomial)
summary(modelrest)
summary (glm( femalesfocal$Rprop ~ femalesfocal$treatment))
pairwise.wilcox.test(femalesfocal$Rprop, femalesfocal$treatment)

walkbin <- cbind(femalesfocal$W, femalesfocal$total-femalesfocal$W)
modelwalk <- glm (walkbin~treatment, binomial)
summary (modelwalk)

digbin <- cbind (femalesfocal$D, femalesfocal$total-femalesfocal$D)
modeldig <- glm (digbin~treatment, binomial)
summary (modeldig)

feedbin <- cbind (femalesfocal$F, femalesfocal$total-femalesfocal$F)
modelfeed <- glm (feedbin~treatment, binomial)
summary (modelfeed)

cleanbin <- cbind (femalesfocal$C, femalesfocal$total-femalesfocal$C)
modelclean <- glm (cleanbin~treatment, binomial)
summary(modelclean)

shufflebin <- cbind (femalesfocal$S, femalesfocal$total-femalesfocal$S)
modelshuffle <- glm (shufflebin~treatment, binomial)
summary (modelshuffle)

groombin <- cbind (femalesfocal$G, femalesfocal$total-femalesfocal$G)
modelgroom <- glm (groombin~treatment, binomial)
summary (modelgroom)

cannibalisebin <- cbind (femalesfocal$Can, femalesfocal$total-femalesfocal$Can)
modelcannibalise <- glm (cannibalisebin~treatment, binomial)
summary (modelcannibalise)


### GLM w Same population as baseline ###
treatment <- factor (femalesfocal$treatment, levels = c("Intrapopulation", "Control", "Interpopulation"))

restbin <- cbind (femalesfocal$R, femalesfocal$total-femalesfocal$R)
modelrest <- glm(restbin~treatment, binomial)
summary(modelrest)

walkbin <- cbind(femalesfocal$W, femalesfocal$total-femalesfocal$W)
modelwalk <- glm (walkbin~treatment, binomial)
summary (modelwalk)

digbin <- cbind (femalesfocal$D, femalesfocal$total-femalesfocal$D)
modeldig <- glm (digbin~treatment, binomial)
summary (modeldig)

feedbin <- cbind (femalesfocal$F, femalesfocal$total-femalesfocal$F)
modelfeed <- glm (feedbin~treatment, binomial)
summary (modelfeed)

cleanbin <- cbind (femalesfocal$C, femalesfocal$total-femalesfocal$C)
modelclean <- glm (cleanbin~treatment, binomial)
summary(modelclean)

shufflebin <- cbind (femalesfocal$S, femalesfocal$total-femalesfocal$S)
modelshuffle <- glm (shufflebin~treatment, binomial)
summary (modelshuffle)

groombin <- cbind (femalesfocal$G, femalesfocal$total-femalesfocal$G)
modelgroom <- glm (groombin~treatment, binomial)
summary (modelgroom)

cannibalisebin <- cbind (femalesfocal$Can, femalesfocal$total-femalesfocal$Can)
modelcannibalise <- glm (cannibalisebin~treatment, binomial)
summary (modelcannibalise)