library(Lahman)
library(tidyr)
library("dplyr")
library("ggplot2")

#Train

batting <- Batting %>%
  filter(yearID == 2010) %>%
  left_join(select(Salaries, playerID, yearID, teamID, salary), 
            by=c("playerID", "yearID", "teamID"))
str(batting)

batting %>%
  filter(! is.na(salary))

names(batting)

salarybatting = subset(batting, select = -c(playerID, teamID, lgID))
salarybatting %>%
  filter(! is.na(salary))
salarybatting[is.na(salarybatting)] <- 0

#Test

nextyear <- Batting %>%
  filter(yearID == 2011) %>%
  left_join(select(Salaries, playerID, yearID, teamID, salary), 
            by=c("playerID", "yearID", "teamID"))
str(nextyear)

nextyear %>%
  filter(! is.na(salary))

names(nextyear)

salarynextyear = subset(nextyear, select = -c(playerID, teamID, lgID))
salarynextyear[is.na(salarynextyear)] <- 0


#Tree
library(rpart) #Cart method
library(rpart.plot)
library(rsample)
library(tree)
library(caret)
library(vip)
library(pdp)
library(partykit)
library(yardstick)
library(forecast)

salfit <- rpart(salary ~ ., data = salarybatting, method = "anova")
salfit
rpart.plot(salfit)
plotcp(salfit)
summary(salfit)
vip(salfit, geom = "point")

#Predict
salpred <- predict(salfit, salarynextyear)
RMSE(pred = salpred, obs = salarynextyear$salary)
accuracy(salpred, salarynextyear$salary)


#Prune
printcp(salfit)
plotcp(salfit)

salprune <- prune(salfit, cp = salfit$cptable[which.min(salfit$cptable[,"xerror"]),"CP"])
salprune
rpart.plot(salprune)
vip(salprune, geom = "point")

prunesalpred <- predict(salprune, salarynextyear)
accuracy(prunesalpred, salarynextyear$salary)


#Bagging
library(doParallel)
library(foreach)
library(ipred)
library(vip)
set.seed(1)
salbag <- bagging(
  formula = salary ~ .,
  data = salarybatting,
  nbagg = 100,
  coob = TRUE
)
salbag

bagregpred <- predict(salbag, salarynextyear)
bagregpred
accuracy(bagregpred, salarynextyear$salary)

#Random Forest
library(randomForest)
salarybatting[is.na(salarybatting)] <- 0
salarynextyear[is.na(salarynextyear)] <- 0

rfsalary <- randomForest(salary ~ ., data = salarybatting, na.action = na.omit)
rfsalary

rfregpred <- predict(rfsalary, salarynextyear)
rfregpred
accuracy(rfregpred, salarynextyear$salary)


#Boosted Methods

#ORT
