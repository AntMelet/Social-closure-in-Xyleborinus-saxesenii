library(readxl)
library(randomForest)
library(lme4)

Chemical_data <- read_excel("//NAS/home//Antoine/Projects/Social closure/Chemical analysis/Chemical data_for R.xlsx")
Chemical_data <- Chemical_data[,-2]

str(Chemical_data)
Chemical_data$Population = as.factor(Chemical_data$Population)

# Run the random forest model
rf <- randomForest(Chemical_data$Population~., data=Chemical_data, ntree = 500)
print(rf)

# Find the optimal mtry value
mtry <- tuneRF(Chemical_data[-1],Chemical_data$Population, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Build model based of best mtry value
rf <-randomForest(Chemical_data$Population~.,data=Chemical_data, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
importance(rf)
varImpPlot(rf, main= "Random Forest model")
varImpPlot(rf, main= "Random Forest model", labels=c("16-; 15-; 14-; 13-MeC34", "13,21-; 12,22-; 11,21-diMeC34", "15-; 13-; 11-MeC31", "x,y,z-triMeC34", "C31", "17-; 15-; 13-; 11-MeC33", "C29", "C27", "11,17,23-triMeC35", "11,21-; 11,23-diMeC35", "5,11-; 5,13-diMeC31", "11,17,23-triMeC33", "15-; 13-MeC35", "11,21-diMeC33", "11,19-diMeC31","C33"))

# Wilcoxon test for each coumpond
C27 <- wilcox.test(Chemical_data$C27~Chemical_data$Population)
C29 <- wilcox.test(Chemical_data$C29~Chemical_data$Population)
C31 <- wilcox.test(Chemical_data$C31~Chemical_data$Population)
C33 <- wilcox.test(Chemical_data$C33~Chemical_data$Population)
MeC31 <- wilcox.test(Chemical_data$MeC31~Chemical_data$Population)
MeC33 <- wilcox.test(Chemical_data$MeC33~Chemical_data$Population)
MeC34 <- wilcox.test(Chemical_data$MeC34~Chemical_data$Population)
MeC35 <- wilcox.test(Chemical_data$MeC35~Chemical_data$Population)
diMeC31_1 <- wilcox.test(Chemical_data$diMeC31_1~Chemical_data$Population)
diMeC31_2 <- wilcox.test(Chemical_data$diMeC31_2~Chemical_data$Population)
diMeC34 <- wilcox.test(Chemical_data$diMeC34~Chemical_data$Population)
diMeC35 <- wilcox.test(Chemical_data$diMeC35~Chemical_data$Population)
diMeC33 <- wilcox.test(Chemical_data$diMeC33~Chemical_data$Population)
triMeC33 <- wilcox.test(Chemical_data$triMeC33~Chemical_data$Population)
triMeC34 <- wilcox.test(Chemical_data$triMeC34~Chemical_data$Population)
triMeC35 <- wilcox.test(Chemical_data$triMeC35~Chemical_data$Population)

Chemical_data$total <- rowSums (Chemical_data[,2:17])

Chemical_data$alkanes <- Chemical_data$C27 + Chemical_data$C29 + Chemical_data$C31 + Chemical_data$C33
Chemical_data$alkanespercent <- Chemical_data$alkanes/Chemical_data$total*100

Chemical_data$monomethylalkanes <- Chemical_data$MeC31 + Chemical_data$MeC33 + Chemical_data$MeC34 + Chemical_data$MeC35
Chemical_data$monomethylalkanespercent <- Chemical_data$monomethylalkanes/Chemical_data$total*100

Chemical_data$dimethylalkanes <- Chemical_data$diMeC31_1 + Chemical_data$diMeC31_2 + Chemical_data$diMeC33 + Chemical_data$diMeC34 + Chemical_data$diMeC35
Chemical_data$dimethylalkanespercent <- Chemical_data$dimethylalkanes/Chemical_data$total*100

Chemical_data$trimethylalkanes <- Chemical_data$triMeC33 + Chemical_data$triMeC34 + Chemical_data$triMeC35
Chemical_data$trimethylalkanespercent <- Chemical_data$trimethylalkanes/Chemical_data$total*100

proportionofalkanes <- summary (lm (Chemical_data$alkanespercent ~ Chemical_data$Population))
by (Chemical_data$alkanespercent, Chemical_data$Population, mean)

proportionofmonomethylalkanes <- summary (lm (Chemical_data$monomethylalkanespercent ~ Chemical_data$Population))
by (Chemical_data$monomethylalkanespercent, Chemical_data$Population, mean)

proportionofdimethylalkanes <- summary (lm (Chemical_data$dimethylalkanespercent ~ Chemical_data$Population))
by (Chemical_data$dimethylalkanespercent, Chemical_data$Population, mean)

proportionoftrimethylalkanes <- summary (lm (Chemical_data$trimethylalkanespercent ~ Chemical_data$Population))
by (Chemical_data$trimethylalkanespercent, Chemical_data$Population, mean)

Chemical_data$weightedaveragechainlength <- (Chemical_data$C27*27 + Chemical_data$C29*29 + Chemical_data$C31*31 + Chemical_data$C33*33) / Chemical_data$alkanes
by (Chemical_data$weightedaveragechainlength, Chemical_data$Population, mean)
chisq.test(Chemical_data$weightedaveragechainlength)
