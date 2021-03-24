library(Lahman)
library(tidyr)
library("dplyr")
library("ggplot2")
library(yardstick)
library(forecast)

#Train
ggplot(Salaries, aes(x = factor(yearID), y = salary/1e5)) +
  geom_boxplot(fill = "lightblue", outlier.size = 1) +
  labs(x = "Year", y = "Salary ($100,000)") +
  coord_flip()

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

#Linear Regression Check
reglm <- lm(salary/1e6 ~ ., data = salarybatting)
reglm
summary(reglm)

step(reglm, scale = summary(reglm)$sigma^2)
reglmstep <- lm(formula = salary/1e+06 ~ stint + G + H + X3B + HR + RBI + 
                  BB + SO + IBB + SH, data = salarybatting)
summary(reglmstep)

reglmpred <- predict(reglm, salarynextyear[, -20])
reglmpred
regsteppred <- predict(reglmstep, salarynextyear[, -20])
regsteppred
accuracy(reglmpred, salarynextyear$salary)
accuracy(regsteppred, salarynextyear$salary)

ggplot() +
  geom_point(aes(regsteppred, salarynextyear$salary), colour = 'blue') +
  geom_abline(slope = 1, intercept = 0, colour = 'red')

#Tree
library(rpart) #Cart method
library(rpart.plot)
library(rsample)
library(tree)
library(caret)
library(vip)
library(pdp)
library(partykit)


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
library(xgboost)
library(Matrix)
library(data.table)


set.seed(1)
regxg <- xgboost(data = as.matrix(salarybatting[, -20]),
                 label = as.numeric(salarybatting[, 20]),
                 objective = "reg:squarederror",
                 nrounds = 10,
                 verbose = FALSE,
                 predictions = TRUE)
regxg

regboostpred <- predict(regxg, as.matrix(salarynextyear[, -20]))
regboostpred
accuracy(regboostpred, salarynextyear$salary)

#ORT
salX <- salarybatting[, 1:19]
salY <- salarybatting[, 20]
nextsalX <- salarynextyear[, 1:19]
nextsalY <- salarynextyear[, 20]

library(iai)

set.seed(1)
reggrid <- iai::grid_search(
  iai::optimal_tree_regressor(
    random_seed = 1,
  ),
  max_depth = 1:10,
)
iai::fit(reggrid, salX, salY)
iai::get_learner(reggrid)

ortsalpred <- iai::predict(reggrid, nextsalX)
ortsalpred
accuracy(ortsalpred, nextsalY)

set.seed(1)
reggrid2 <- iai::grid_search(
  iai::optimal_tree_regressor(
    random_seed = 1,
    hyperplane_config = list(sparsity = "all"),
  ),
  max_depth = 1:5,
)
iai::fit(reggrid2, salX, salY)
iai::get_learner(reggrid2)

ortsalpred <- iai::predict(reggrid, nextsalX)
ortsalpred
accuracy(ortsalpred, nextsalY)

detach("package:iai", unload = TRUE)

