rm(list=ls())

#optional - install packages
# install.packages(dplyr)
# install.packages(ROCR)

#load libraries
library(dplyr)
library(ROCR)

#set working directory - please select the folder where you saved script and data files
setwd("C:/UBS/Dev/Students/Working folder/")


# C1: read the data
Data <- read.delim("DataPD.txt", stringsAsFactors = F)
CriteriaSummary <- read.delim("Description.txt", stringsAsFactors = F)

# C2: set useful variables
VarNames <- CriteriaSummary[, "Criteria"]

# C3: look into the data
View(Data)
# or
head(Data)
#what can we observe?

# and the data description - criteria have economical interpretation, grouped into four categories
CriteriaSummary

# C4 Check histograms

plot(density(Data[, VarNames[1]], na.rm = T), main = VarNames[1])

#the same command but in pipeline notation - from dplyr package
Data[, VarNames[1]] %>% density(na.rm = T) %>% plot(main = VarNames[1])

#maybe several histograms in one picture
par(mfrow = c(3,4))
for(i in 1:length(VarNames)){
  Data[, VarNames[i]] %>% density(na.rm = T) %>% plot(main = VarNames[i])
}

#what do we observe? is that extreme data concentration?

# C5 check variable distribution parameters
quantile(Data[, VarNames[1]], seq(0, 1, by = 0.1), na.rm = T)
#what do we conclude? what about maximums?
DataQuantiles <- matrix(nrow = length(VarNames), ncol = 12)
#anyone eager to write a loop summarizing quantiles for all variables in a matrix?

# C6 remove outliers - an idea to obtain reasonable histograms
par(mfrow = c(3,4))
for(i in 1:length(VarNames)){
  LowerBound <- quantile(Data[, VarNames[i]], 0.025, na.rm = T)
  UpperBound <- quantile(Data[, VarNames[i]], 0.975, na.rm = T)
  Data[Data[, VarNames[i]] >= LowerBound & Data[, VarNames[i]] <= UpperBound , VarNames[i]] %>% density(na.rm = T) %>% plot(main = VarNames[i])
}

# C7 Check AUC for variables 
Pred <- prediction(Data[,VarNames[1]],Data[,"deflag"])
AUC <- as.numeric(performance(Pred,"auc")@y.values)

#anyone eager to write a loop?

#there is another way to do so - sapply command
sapply(VarNames, function(x) as.numeric(performance(prediction(Data[,x],Data[,"deflag"]),"auc")@y.values))

# C8 First probit regressions
Regress <- glm(formula= "deflag ~ var1_AQ +  var2_AQ",na.action=na.exclude,family=binomial("probit"), data = Data)
summary(Regress)
#what is null and residual deviance, AIC? are large or small values desired?

#different approach for Regression coding
CriteriaCodes <- VarNames[c(1,2)]
Formula <- as.formula(paste("deflag ~ ", gsub(", "," + ",toString(CriteriaCodes))))# The regression formula
Regress <- glm(formula=Formula,na.action=na.exclude,family=binomial("probit"), data = Data)
Regress$coefficients
summary(Regress)

#check of DP - one step more than in case of univariate analysis - we must produce model score for each observation
FittedScores <- predict.glm(Regress)
Pred <- prediction(FittedScores,Data$deflag)
AUC <- as.numeric(performance(Pred,"auc")@y.values)

#check different combination
CriteriaCodes2 <- VarNames[c(3,4)]
Formula2 <- as.formula(paste("deflag ~ ", gsub(", "," + ",toString(CriteriaCodes2))))# The regression formula
Regress2 <- glm(formula=Formula2,na.action=na.exclude,family=binomial("probit"), data = Data)
Regress2$coefficients
summary(Regress2)

FittedScores2 <- predict.glm(Regress2)
Pred2 <- prediction(FittedScores2,Data$deflag)
AUC2 <- as.numeric(performance(Pred2,"auc")@y.values)

# the rest is your own invention 

