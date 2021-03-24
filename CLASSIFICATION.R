library(Lahman)
library("dplyr")
library("ggplot2")

teamseason <- Lahman::Teams
head(teamseason)

#list(teamseason$teamID)
list(teamseason$lgID)


#TRAIN


alplayoff <- filter(teamseason, teamseason$lgID =="AL")
head(alplayoff)
is.factor(alplayoff)
alplayoff[alplayoff =="N"] = 0
alplayoff[alplayoff =="Y"] = 1
head(alplayoff)
alplayoff %>% filter(!is.na(LgWin))
alplayoff1 = subset(alplayoff, select = -c(Rank, park, WSWin, 
                                           name, teamID, franchID, teamIDBR,
                                           teamIDlahman45, teamIDretro))
alplayoff1$LgWin <- as.factor(alplayoff1$LgWin)
alplayoff1 %>% filter(!is.na(LgWin))
str(alplayoff1)

alplayoff1$LgWin <- as.factor(alplayoff1$LgWin)
alplayoff1[is.na(alplayoff1)] <- 0
head(alplayoff1)


#TEST


nlplayoff <- filter(teamseason, teamseason$lgID =="NL")
head(nlplayoff)
is.factor(nlplayoff)
nlplayoff[nlplayoff =="N"] = 0
nlplayoff[nlplayoff =="Y"] = 1
head(nlplayoff)
nlplayoff %>% filter(!is.na(LgWin))
nlplayoff1 = subset(nlplayoff, select = -c(Rank, park, WSWin, 
                                           name, teamID, franchID, teamIDBR,
                                           teamIDlahman45, teamIDretro))
str(nlplayoff1)
head(nlplayoff1)
nlplayoff1 %>%
  filter(yearID >= 1901)
as.factor(nlplayoff1$LgWin)
nlplayoff1[is.na(nlplayoff1)] <- 0
head(nlplayoff1)


#tree plot


library(rpart) #Cart method
library(rpart.plot)
library(rsample)
library(tree)
library(caret)
library(vip)
library(pdp)
library(partykit)
library(yardstick)
library(cvms)

lgwinfit <- rpart(LgWin ~ ., data = alplayoff1)
lgwinfit
rpart.plot(lgwinfit)
plotcp(lgwinfit)
summary(lgwinfit)
vip(lgwinfit, geom = "point")
lgwinfit$cptable

printcp(lgwinfit)
plotcp(lgwinfit)


#prune


lgprune <- prune(lgwinfit, 
                 cp = lgwinfit$cptable[which.min(lgwinfit$cptable[,"xerror"]),
                                       "CP"])
lgprune
rpart.plot(lgprune)
vip(lgprune, geom = "point")




nlwinpred <- predict(lgwinfit, nlplayoff1, type = 'class')
nlwinprob <- predict(lgwinfit, nlplayoff1, type = 'prob')

table_nl <- table(nlplayoff1$LgWin, nlwinpred)
table_nl
accuracy_Test <- sum(diag(table_nl)) / sum(table_nl)
print(paste('Accuracy for test', accuracy_Test))



prunenlpred <- predict(lgprune, nlplayoff1, type = "class")
prunenlprob <- predict(lgprune, nlplayoff1, type = "prob")

nl_prune <- table(nlplayoff1$LgWin, prunenlpred)
nl_prune
accuracy_prune <- sum(diag(nl_prune)) / sum(nl_prune)
print(paste('Accuracy for test', accuracy_prune))


#ROC

library(ROCit)

ROCclass <- rocit(nlwinprob[, 2], nlplayoff1$LgWin)

plot(ROCclass)
ciAUC(ROCclass)

ROCprune <- rocit(prunenlprob[, 2], nlplayoff1$LgWin)

plot(ROCprune)
ciAUC(ROCprune)


plot(ROCclass, col = 1, legend = FALSE, YIndex = FALSE)
lines(ROCprune$TPR ~ ROCprune$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2),
       c("CART", "Pruned CART"), lwd = 2, cex = .8)


#Different Size of Nodes

class5min50 <- rpart(LgWin ~ ., data = alplayoff1, 
                     control = rpart.control(minsplit = 50, maxdepth = 5))
class5min50
rpart.plot(class5min50)

class10min40 <- rpart(LgWin ~ ., data = alplayoff1, 
                      control = rpart.control(minsplit = 40, maxdepth = 10))
class10min40
rpart.plot(class10min40)

class10min20 <- rpart(LgWin ~ ., data = alplayoff1, 
                      control = rpart.control(minsplit = 20, maxdepth = 10))
class10min20
rpart.plot(class10min20)

class10min15 <- rpart(LgWin ~ ., data = alplayoff1, 
                      control = rpart.control(minsplit = 15, maxdepth = 10))
class10min15
rpart.plot(class10min15)

class10min10 <- rpart(LgWin ~ ., data = alplayoff1, 
                      control = rpart.control(minsplit = 10, maxdepth = 10))
class10min10
rpart.plot(class10min10)

classpred550 <- predict(class5min50, nlplayoff1, type = 'class')
classprob550 <- predict(class5min50, nlplayoff1, type = 'prob')

classpred1040 <- predict(class10min40, nlplayoff1, type = 'class')
classprob1040 <- predict(class10min40, nlplayoff1, type = 'prob')

classpred1020 <- predict(class10min20, nlplayoff1, type = 'class')
classprob1020 <- predict(class10min20, nlplayoff1, type = 'prob')

classpred1015 <- predict(class10min15, nlplayoff1, type = 'class')
classprob1015 <- predict(class10min15, nlplayoff1, type = 'prob')

classpred1010 <- predict(class10min10, nlplayoff1, type = 'class')
classprob1010 <- predict(class10min10, nlplayoff1, type = 'prob')

table550 <- table(nlplayoff1$LgWin, classpred550)
accuracy550 <- sum(diag(table550)) / sum(table550)
print(paste('Accuracy for test', accuracy550))

table1040 <- table(nlplayoff1$LgWin, classpred1040)
accuracy1040 <- sum(diag(table1040)) / sum(table1040)
print(paste('Accuracy for test', accuracy1040))

table1020 <- table(nlplayoff1$LgWin, classpred1020)
accuracy1020 <- sum(diag(table1020)) / sum(table1020)
print(paste('Accuracy for test', accuracy1020))

table1015 <- table(nlplayoff1$LgWin, classpred1015)
accuracy1015 <- sum(diag(table1015)) / sum(table1015)
print(paste('Accuracy for test', accuracy1015))

table1010 <- table(nlplayoff1$LgWin, classpred1010)
accuracy1010 <- sum(diag(table1010)) / sum(table1010)
print(paste('Accuracy for test', accuracy1010))

ROC550 <- rocit(classprob550[, 2], nlplayoff1$LgWin)
ROC1040 <- rocit(classprob1040[, 2], nlplayoff1$LgWin)
ROC1020 <- rocit(classprob1020[, 2], nlplayoff1$LgWin)
ROC1015 <- rocit(classprob1015[, 2], nlplayoff1$LgWin)
ROC1010 <- rocit(classprob1010[, 2], nlplayoff1$LgWin)

ciAUC(ROC550)
ciAUC(ROC1040)
ciAUC(ROC1020)
ciAUC(ROC1015)
ciAUC(ROC1010)

plot(ROC550, col = 1, legend = FALSE, YIndex = FALSE)
lines(ROC1040$TPR ~ ROC1040$FPR, col = 2, lwd = 2)
lines(ROC1020$TPR ~ ROC1020$FPR, col = 3, lwd = 2)
lines(ROC1015$TPR ~ ROC1015$FPR, col = 4, lwd = 2)
lines(ROC1010$TPR ~ ROC1010$FPR, col = 6, lwd = 2)
legend("bottomright", col = c(1,2,3,4,6),
       c("Min Split = 50", "Min Split = 40", 
         "Min Split = 20", "Min Split = 15", "Min Split = 10"), 
       lwd = 2, cex = .8)


#Information Gain

lgwininfo <- rpart(LgWin ~ ., data = alplayoff1, 
                   parms=list(split="information"))
rpart.plot(lgwininfo)
plotcp(lgwininfo)
infoprune<-prune(lgwininfo, 
                 cp=lgwininfo$cptable[which.min(lgwininfo$cptable[,"xerror"])])
infoprune

infonlpred <- predict(lgwininfo, nlplayoff1, type = "class")
infonlprob <- predict(lgwininfo, nlplayoff1, type = "prob")

ROCinfo <- rocit(infonlprob[, 2], nlplayoff1$LgWin)
plot(ROCinfo)
ciAUC(ROCinfo)
infotable <- table(nlplayoff1$LgWin, infonlpred)
infotable
accuracy_info <- sum(diag(infotable)) / sum(infotable)
print(paste('Accuracy for test', accuracy_info))

giniinfo <- table(infonlpred, nlwinpred)
giniinfo
accuracy_giniinfo <- sum(diag(giniinfo)) / sum(giniinfo)
print(paste('Accuracy for test', accuracy_giniinfo))

plot(ROCclass, col = 1, legend = FALSE, YIndex = FALSE)
lines(ROCinfo$TPR ~ ROCinfo$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2),
       c("Gini", "Information gain"), 
       lwd = 2, cex = .8)

#Bagging

library(doParallel)
library(foreach)
library(ipred)
library(vip)

alplayoff1$LgWin <- as.factor(alplayoff1$LgWin)

set.seed(1)
lgbag <- bagging(formula = LgWin ~ ., data = alplayoff1, nbagg = 100)

bagclasspred <- predict(lgbag, nlplayoff1)
bagclasspred
bagclassprob <- predict(lgbag, nlplayoff1, type = "prob")
bagclassprob
bag_nl <- table(nlplayoff1$LgWin, bagclasspred)
bag_nl
accuracy_Bag <- sum(diag(bag_nl)) / sum(bag_nl)
print(paste('Accuracy for test', accuracy_Bag))

ROCbag <- rocit(bagclassprob[, 2], nlplayoff1$LgWin)
plot(ROCbag)



#RF

library(randomForest)

#RF Bagging

rfbag <- randomForest(LgWin ~ ., data = alplayoff1, mtry = 38, importance=TRUE)
rfbag
rfbagpred <- predict(rfbag, nlplayoff1, type = 'class')
rfbagprob <- predict(rfbag, nlplayoff1, type = 'prob')
rfb_nl <- table(nlplayoff1$LgWin, rfbagpred)
accuracy_Rbag <- sum(diag(rfb_nl)) / sum(rfb_nl)
print(paste('Accuracy for test', accuracy_Rbag))

ROCrbag <- rocit(rfbagprob[, 2], nlplayoff1$LgWin)
plot(ROCrbag)

ciAUC(ROCbag)
ciAUC(ROCrbag)

plot(ROCbag, col = 1, legend = FALSE, YIndex = FALSE)
lines(ROCrbag$TPR ~ ROCrbag$FPR, col = 3, lwd = 2)
legend("bottomright", col = c(1,3),
       c("Bagging", "Bagged RF"), lwd = 2, cex = .8)


#Random Forest

set.seed(1)
rflg <- randomForest(LgWin ~ ., data = alplayoff1,
                     importance=TRUE, proximity=TRUE, na.action = na.omit)
rflg
summary(rflg)

rfclasspred <- predict(rflg, nlplayoff1, type = 'class')
rfclasspred
rfclassprob <- predict(rflg, nlplayoff1, type = "prob")
rfclassprob

rf_nl <- table(nlplayoff1$LgWin, rfclasspred)
rf_nl
accuracy_RF <- sum(diag(rf_nl)) / sum(rf_nl)
print(paste('Accuracy for test', accuracy_RF))

ROCrf <- rocit(rfclassprob[, 2], nlplayoff1$LgWin)
plot(ROCrf)
ciAUC(ROCrf)



#AdaBoost

library(adabag)
library(caret)

set.seed(1)
adamodel <- boosting(LgWin ~ ., data = alplayoff1, boos = TRUE, mfinal = 100,
                     coeflearn = 'Breiman')

print(names(adamodel))

print(adamodel$trees[1])

adaclassprediction <- predict(adamodel, nlplayoff1)
adaclassprediction

names(adaclassprediction)

ada_nl <- adaclassprediction$confusion
ada_nl
accuracy_ada <- sum(diag(ada_nl)) / sum(ada_nl)
print(paste('Accuracy for test', accuracy_ada))

rocada <- rocit(adaclassprediction$prob[, 2], nlplayoff1$LgWin)
plot(rocada)
ciAUC(rocada)



#xgbTree using CARET

library(xgboost)
library(Matrix)
library(data.table)

set.seed(1)
xgmodel <- train(LgWin ~ ., data = alplayoff1, method = "xgbTree",
                 trControl = trainControl("cv", number = 10))
xgmodel

varImp(xgmodel)

xgclasspred <- predict(xgmodel, nlplayoff1, type = 'raw')
xgclasspred
xgclassprob <- predict(xgmodel, nlplayoff1, type = "prob")
xgclassprob

xg_nl <- table(nlplayoff1$LgWin, xgclasspred)
xg_nl
accuracy_xg <- sum(diag(xg_nl)) / sum(xg_nl)
print(paste('Accuracy for test', accuracy_xg))

rocxg <- rocit(xgclassprob[, 2], nlplayoff1$LgWin)
plot(rocxg)
ciAUC(rocxg)

play_x <- alplayoff1[, c(-3,-8,-9,-10)]
play_y <- alplayoff1[, 10]
testwin_x <- nlplayoff1[, c(-3,-8,-9,-10)]
testwin_y <- nlplayoff1[, 10]

boostclass <- xgboost(data = as.matrix(play_x[, -2]),
                      label = as.numeric(play_y)-1,
                      objective = "binary:logistic",
                      nrounds = 10,
                      verbose = FALSE,
                      prediction = TRUE)
boostclass

predict(boostclass, as.matrix(testwin_x[, -2])) %>%
  as_tibble() %>%
  mutate(prediction = round(value),
         label = as.numeric(testwin_y)-1) %>%
  count(prediction, label)

boostpred <- predict(boostclass, as.matrix(testwin_x[, -2]))
boostpredround <- round(boostpred)
boost_nl <- table(boostpredround, testwin_y)
boost_nl
accuracy_boost <- sum(diag(boost_nl))/sum(boost_nl)
print(paste('Accuracy for test', accuracy_boost))

boostroc <- rocit(boostpred, nlplayoff1$LgWin)
plot(boostroc)
ciAUC(boostroc)

#IAI OCT

library(iai)

grid <- iai::grid_search(
  iai::optimal_tree_classifier(
    random_seed = 1,
  ),
  max_depth = 10,
)
iai::fit(grid, play_x, play_y)
iai::get_learner(grid)

iai::predict(grid, testwin_x)
iai::score(grid, play_x, play_y, criterion = "misclassification")
iai::score(grid, testwin_x, testwin_y, criterion = "auc")
iai::roc_curve(grid, testwin_x, testwin_y, positive_label="1")

playoffprob <- iai::predict_proba(grid, testwin_x)
iairoc <- rocit(playoffprob[, 2], nlplayoff1$LgWin)
plot(iairoc)



#OCT Bag

require(data.table)
bagging_data=data.table(alplayoff1)
bagging_test=data.table(nlplayoff1)

n_model=100
optimized_models=list()
nnmodel = matrix(0,nrow(testwin_x),n_model)
probmodel = matrix(0,nrow(testwin_x),n_model)
for (i in 1:n_model){
  new_sample=sample(1:nrow(play_x), size=round(nrow(play_x)), replace=T)
  optimized_models = c(optimized_models, list(iai::fit(grid, play_x[new_sample,], play_y[new_sample])))
  nnmodel[,i] = as.integer(iai::predict(grid, testwin_x))
  playoffprob <- iai::predict_proba(grid, testwin_x)
  probmodel[,i] = playoffprob[, 2]
}

optimized_models
nnmodel
probmodel

bag_pred = rowMeans(nnmodel)
bag_pred

bag_prob = rowMeans(probmodel)
bag_prob

iabagprob <- rocit(bag_prob, nlplayoff1$LgWin)
plot(iabagprob)
iabagprob

iai::fit(grid, play_x, play_y)
one_pred = as.integer(iai::predict(grid, testwin_x))

table(bag_pred > 0.5, one_pred)
table(testwin_y, bag_pred > 0.5)
table(testwin_y, one_pred)

baseiai <- table(one_pred, nlplayoff1$LgWin)
baseiai

acciai <- sum(diag(baseiai)) / sum(baseiai)
acciai


bagvia100 <- table(bag_pred > 0.5, one_pred)
iabag100 <- table(testwin_y, bag_pred > 0.5)
sum(diag(bagvia100))/sum(bagvia100)
sum(diag(iabag100))/sum(iabag100)

rociabag <- rocit(bag_prob, nlplayoff1$LgWin)

plot(iairoc, col = 1, legend = FALSE, YIndex = FALSE)
lines(rociabag$TPR ~ rociabag$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2),
       c("OCT Model", "Bagged OCT Model"), 
       lwd = 2, cex = .8)

#Messing around with gridsearch
grid2 <- iai::grid_search(
  iai::optimal_tree_classifier(
    random_seed = 1,
    criterion = "gini",
  ),
  max_depth = 5,

)

iai::fit(grid2, play_x, play_y)
iai::get_learner(grid2)

iaginipred <- iai::predict(grid2, testwin_x)

playoffprobgini <- iai::predict_proba(grid2, testwin_x)
iaiginiroc <- rocit(playoffprobgini[, 2], nlplayoff1$LgWin)
plot(iaiginiroc)
ciAUC(iaiginiroc)

giniiai <- table(iaginipred, testwin_y)
giniiai
sum(diag(giniiai))/sum(giniiai)

plot(iairoc, col = 1, legend = FALSE, YIndex = FALSE)
lines(iaiginiroc$TPR ~ iaiginiroc$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2),
       c("OCT Misclassification", "OCT Gini"), 
       lwd = 2, cex = .8)



grid3 <- iai::grid_search(
  iai::optimal_tree_classifier(
    random_seed = 1,
    max_depth = 2,
    hyperplane_config = list(sparsity = "all"),
  ),
)
iai::fit(grid3, play_x, play_y)
iai::get_learner(grid3)

iahyp <- iai::predict(grid3, testwin_x)

playoffprobhyp <- iai::predict_proba(grid3, testwin_x)
iaihyproc <- rocit(playoffprobhyp[, 2], nlplayoff1$LgWin)
plot(iaihyproc)
ciAUC(iaihyproc)

giniiai <- table(iaginipred, testwin_y)
giniiai
sum(diag(giniiai))/sum(giniiai)



plot(iairoc, col = 1, legend = FALSE, YIndex = FALSE)
lines(iaiginiroc$TPR ~ iaiginiroc$FPR, col = 2, lwd = 2)
lines(rociabag$TPR ~ rociabag$FPR, col = 3, lwd = 2)
lines(iaihyproc$TPR ~ iaihyproc$FPR, col = 4, lwd = 2)
legend("bottomright", col = c(1,2,3,4),
       c("OCT Misclassification", "OCT Gini", "Bagged OCT", "OCT Hyperplane"), 
       lwd = 2, cex = .8)





detach("package:iai", unload = TRUE)

plot(ROCclass, col = 1, legend = FALSE, YIndex = FALSE)
lines(ROCprune$TPR ~ ROCprune$FPR, col = 2, lwd = 2)
lines(ROCinfo$TPR ~ ROCinfo$FPR, col = 3, lwd = 2)
lines(ROCbag$TPR ~ ROCbag$FPR, col = 4, lwd = 2)
lines(ROCrbag$TPR ~ ROCrbag$FPR, col = 5, lwd = 2)
lines(ROCrf$TPR ~ ROCrf$FPR, col = 6, lwd = 2)
lines(rocada$TPR ~ rocada$FPR, col = 7, lwd = 2)
lines(rocxg$TPR ~ rocxg$FPR, col = 8, lwd = 2)
lines(boostroc$TPR ~ boostroc$FPR, col = "darkgreen", lwd = 2)
lines(iairoc$TPR ~ iairoc$FPR, col = "darkblue", lwd = 2)
lines(iaiginiroc$TPR ~ iaiginiroc$FPR, col = "blueviolet", lwd = 2)
lines(rociabag$TPR ~ rociabag$FPR, col = "coral", lwd = 2)
lines(iaihyproc$TPR ~ iaihyproc$FPR, col = "bisque", lwd = 2)
legend("bottomright", 
       col = c(1,2,3,4,5,6,7,8,
              "darkgreen","darkblue","blueviolet","coral","bisque"),
       c("CART","Prune","Information Gain","Bagging","RF Bagging",
         "Random Forest","AdaBoost","xgBoost - Train", "xgBoost",
         "OCT", "OCT Gini", "Bagged OCT", "OCT Hyperplane"), 
       lwd = 2, cex = .5)


