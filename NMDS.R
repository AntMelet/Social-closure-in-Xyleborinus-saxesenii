library(readxl)
library(ggplot2)
library(vegan)

NMDS <- read_excel("//NAS/home/Antoine/Projects/Social closure/Chemical analysis/NMDS.xlsx")
NMDS <- NMDS[,-1]
NMDS <- t(NMDS)

grouping_info<-factor (c("Steinbachtal","Steinbachtal","Steinbachtal","Steinbachtal","Steinbachtal", "Bavarian forest","Bavarian forest","Bavarian forest","Bavarian forest","Bavarian forest"))

sol<-metaMDS(NMDS,distance = "bray", k = 2, trymax = 50)
solpoints <- as.data.frame(sol$points)

NMDS_graph=data.frame(NMDS1=sol$point[,1],NMDS2=sol$point[,2],Group=as.factor(grouping_info))
ggplot()+
  geom_point(data=NMDS_graph,aes(x=NMDS1 ,y=NMDS2, colour=Group, shape = Group), size=4)+
  scale_shape_manual(name="Population", values=c("Bavarian forest"=16, "Steinbachtal"=18))+
  scale_color_manual(name="Population", values=c("red", "blue"))


dist.mat <- vegdist(NMDS, method="bray")

PMV <- adonis2(dist.mat ~ Group, data = NMDS_graph, permutations = 999, method = 'bray')
PMV