library(ggplot2)
library(readxl)
library(ggpubr)

### Open dataframe ###

condition24h <- read_excel("//NAS/home/Antoine//Projects/Social closure/Condition after 24h/After_24h_21.02.08.xlsx")
condition24h$treatment <- factor (condition24h$treatment, levels = c("Control", "Intrapopulation", "Interpopulation", "Interspecies", "total_saxesenii"))

### Fisher's exact test ###

survival_SaxGerm <- as.matrix( c(74, 23, 1, 5))
dim(survival_SaxGerm) <- c(2, 2)
fisher.test(survival_SaxGerm)

survival_controlGerm <- as.matrix(c(26,23,0,5))
dim(survival_controlGerm) <- c(2,2)
fisher.test(survival_controlGerm)

dispersal_controlintrapop <- as.matrix( c(23, 20, 3, 5))
dim(dispersal_controlintrapop) <- c(2,2)
fisher.test(dispersal_controlintrapop)

dispersal_controlinterpop <- as.matrix (c (23, 14, 3, 10))
dim(dispersal_controlinterpop) <- c(2,2)
fisher.test(dispersal_controlinterpop)

dispersal_controlinterspe <- as.matrix (c (23, 26, 3, 2))
dim(dispersal_controlinterspe) <- c(2,2)
fisher.test(dispersal_controlinterspe)

### Define variables for GLMs ###
dispersal_in <- cbind (condition24h$in_total, condition24h$total)
dispersal_out <- cbind (condition24h$out_total, condition24h$total)

survival_alive <- cbind (condition24h$alive_total, condition24h$total)
survival_dead <- cbind (condition24h$dead_total, condition24h$total)


### GLM for dispersal and survival ###
dispersalmodel <- glm(dispersal_out~treatment, binomial)
summary(dispersalmodel)

survivalmodel <- glm(survival_alive~treatment, binomial)
summary(survivalmodel)

### plot dipsersal and survival ###

After_24h_numbers_cat_21_02_08 <- read_excel("//NAS/home/Antoine/Projects/Social closure//Condition after 24h/After_24h_21.02.08.xlsx", sheet = "Categories")
After_24h_numbers_cat_21_02_08_observed <- After_24h_numbers_cat_21_02_08[!After_24h_numbers_cat_21_02_08$condition=="not_visible",]
treatment_plot <- factor (After_24h_numbers_cat_21_02_08_observed$treatment, levels = c("Control", "Intrapopulation", "Interpopulation", "Interspecies"))

ggplot (After_24h_numbers_cat_21_02_08_observed, aes(fill=condition, y=value, x=treatment_plot)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_x_discrete(limits=c("Control","Intrapopulation","Interpopulation", "Interspecies"), labels=c("Same population" = "Intrapopulation", "Other population" = "Interpopulation", "Other species" = "Interspecies")) + 
  xlab ("Treatment") + 
  ylab ("Number of females")+
  scale_fill_manual(values=c("#F8766D", "#FF2D2D", "#619CFF", "#2575FF"),name="Condition", labels=c("Not dispersed (alive)", "Not dispersed (dead)", "Dispersed (alive)", "Dispersed (dead)"))

###
ggplot(condition24h, aes(y=in_total, x=treatment)) + 
  geom_bar(stat = "identity", fill = "#F8766D") + 
  scale_x_discrete(limits=c("Control","Intrapopulation","Interpopulation", "Interspecies"), labels=c("Same population" = "Intrapopulation", "Other population" = "Interpopulation", "Other species" = "Interspecies")) + 
  xlab ("Treatment") + 
  ylab ("Number of females") +
  geom_bracket(  xmin = 1, xmax = 3, y.position = 28, label = "*") +
  labs(title = "Dispersal")


ggplot(condition24h, aes(y=alive_total, x=treatment)) + 
  geom_bar(stat = "identity", fill = "#619CFF") + 
  scale_x_discrete(limits=c("Control","Intrapopulation","Interpopulation", "Interspecies"), labels=c("Same population" = "Intrapopulation", "Other population" = "Interpopulation", "Other species" = "Interspecies")) + 
  xlab ("Treatment") + 
  ylab ("Number of females") +
  geom_bracket(xmin = 2, xmax = 4, y.position = 29.5, label = "**") +
  geom_bracket(xmin = 1, xmax = 3, y.position = 28, label = "") +
  labs(title = "Survival")

ggplot(condition24h, aes(y=alive_total, x=treatment)) + 
  geom_bar(stat = "identity", fill = "#619CFF") +
  xlab ("Species") + 
  ylab ("Number of females") +
  labs(title = "Survival")
