library(randomForest)

combi$FamilyID2 <- 'No'
combi$FamilyID2[combi$FamilySize > 4] <- 'Large'
combi$FamilyID2[combi$FamilySize <= 4] <- 'Small'
combi$FamilyID2[combi$FamilySize == 1] <- 'Alone'
combi$FamilyID2[combi$FamilySize == 2] <- 'Duo'

combi$FamilyID2 <- factor(combi$FamilyID2)

#Split train & test data
train4train <- combi[1:500,]
train4test <- combi[501:891,]
test <- combi[892:1309,]
#train4test <- train4test[ , -which(names(train4test) %in% c("Survived"))]
colnames(train4test)[2] <- "Survived_orig"

set.seed(415)
rffit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2 +
                      Deck + Mom + Child + CabinNo2 + CabinNo3,
                      data=train4train, 
                      importance=TRUE, 
                      ntree=2000)

varImpPlot(rffit)

Prediction <- predict(fit, train4test,OOB=TRUE, type = "response")
result_rf <- data.frame(PassengerId = train4test$PassengerId, 
                                Survived_Orig = train4test$Survived_orig,
                                Survived = Prediction)

#Check Improvement
result_rf$chk <- 0
result_rf$chk[result_rf$Survived == result_rf$Survived_Orig] <- 1
prop.table(table(result_rf$chk))

#Real Train data
train <- combi[1:891,]
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2 +
                      Deck + Mom + Child + CabinNo2 + CabinNo3,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
#Predict real test data
Prediction <- predict(fit, test)
rfsubmit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = Prediction)
write.csv(rfsubmit, file = "myrandomforest.csv", row.names = FALSE)

#Try cforest w new feature
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID2 +
                 Deck + Mom + Child + CabinNo2 + CabinNo3,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

#Predict real test data
Prediction <- predict(fit, test,OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = Prediction)
write.csv(submit, file = "mycforest5.csv", row.names = FALSE)




############ RANDOM FOREST V2

#Real Train data
train <- combi[1:891,]
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2 +
                      Deck + CabinNo2 + CabinNo3,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
#Predict real test data
Prediction <- predict(fit, test)
rfsubmit <- data.frame(PassengerId = test$PassengerId, 
                       Survived = Prediction)
write.csv(rfsubmit, file = "myrandomforest_v2.csv", row.names = FALSE)

##################### CFOREST AGAIN
#Real Train data
train <- combi[1:891,]
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID + Child + 
                 Deck + CabinNo2 + CabinNo3,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

#Predict real test data
Prediction <- predict(fit, test,OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = Prediction)
write.csv(submit, file = "mycforest6.csv", row.names = FALSE)