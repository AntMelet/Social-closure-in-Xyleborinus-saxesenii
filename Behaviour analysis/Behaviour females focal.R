library(readxl)
library(ggplot2)

# read and attach dataframe
behavfoc <- read_excel("//NAS/home/Antoine/Projects/Social closure/Focal observations/Focalobs_stackedbarplot.xlsx")
behavfoc <- behavfoc[behavfoc$treatment %in% c("Control", "Intrapopulation", "Interpopulation"), ]

behavfoc$treatment <- factor (behavfoc$treatment, levels = c("Control", "Intrapopulation", "Interpopulation"))
behavfoc$behavior <- factor (behavfoc$behavior, levels = c("rest", "walk", "dig","feed", "clean", "shuffle", "groom", "copulate", "cannibalise"))

ggplot (behavfoc, aes(fill=behavior, y=value, x=treatment)) + geom_bar(position = "fill", stat = "identity")+
    ylab(label = "")
