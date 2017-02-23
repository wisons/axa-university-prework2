#Mom & Child
combi$Mom <- 0
combi$Mom[combi$Sex=='female' & combi$Parch>0 & combi$Age>18 & combi$Title!='Miss'] <- 1
combi$Child <- 0
combi$Child[combi$Parch>0 & combi$Age<=18] <- 1

combi$Deck <- NA
combi$Deck <- substr(combi$Cabin,1,1)
prop.table(table(combi$Deck, combi$Pclass),2) #check prop
combi$Deck[is.na(combi$Deck) & combi$Pclass == 1] <- 
  sample(c("A", "B", "C", "D", "E", "T"), size = 1, replace = TRUE, prob = c(0.08593750, 0.25390625, 0.36718750, 0.15625000, 0.13281250, 0.00390625))
combi$Deck[is.na(combi$Deck) & combi$Pclass == 2] <- 
  sample(c("D", "E", "F"), size = 1, replace = TRUE, prob = c(0.26086957, 0.17391304, 0.56521739))
combi$Deck[is.na(combi$Deck) & combi$Pclass == 3] <- 
  sample(c("E", "F", "G"), size = 1, replace = TRUE, prob = c(0.18750000, 0.50000000, 0.31250000))

combi$CabinNo <- NA
combi$CabinNo <- sapply(combi$Cabin, FUN=function(x) {strsplit(x, split=' ')[[1]][1]})
combi$CabinNo <- substr(combi$CabinNo,2,nchar(combi$CabinNo))
combi$CabinNo <- as.numeric(combi$CabinNo)
combi$CabinNo2 <- ''
combi$CabinNo2[combi$CabinNo > 0] <- '1-10'
combi$CabinNo2[combi$CabinNo > 10] <- '11-20'
combi$CabinNo2[combi$CabinNo > 20] <- '21-30'
combi$CabinNo2[combi$CabinNo > 30] <- '31-40'
combi$CabinNo2[combi$CabinNo > 40] <- '41-50'
combi$CabinNo2[combi$CabinNo > 50] <- '51-60'
combi$CabinNo2[combi$CabinNo > 60] <- '61-70'
combi$CabinNo2[combi$CabinNo > 70] <- '71-80'
combi$CabinNo2[combi$CabinNo > 80] <- '81-90'
combi$CabinNo2[combi$CabinNo > 90] <- '91-100'
combi$CabinNo2[combi$CabinNo > 100] <- '101+'
prop.table(table(combi$CabinNo2, combi$Survived),1) #check prop
combi$CabinNo3 <- 'No Cabin'
combi$CabinNo3[combi$CabinNo > 0] <- 'Has Cabin'

#prop.table(table(combi$CabinNo2[combi$Pclass==1], combi$Survived[combi$Pclass==1]),1)

#Factor
combi$Mom <- factor(combi$Mom)
combi$Child <- factor(combi$Child)
combi$Deck <- factor(combi$Deck)
combi$CabinNo2 <- factor(combi$CabinNo2)
combi$CabinNo3 <- factor(combi$CabinNo3)


#Split train & test data
train4train <- combi[1:500,]
train4test <- combi[501:891,]
test <- combi[892:1309,]
#train4test <- train4test[ , -which(names(train4test) %in% c("Survived"))]
colnames(train4test)[2] <- "Survived_orig"

#Predict forest of conditional inference trees
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID + 
                 Deck + CabinNo2 + CabinNo3,
               data = train4train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, train4test,OOB=TRUE, type = "response")
result_train4test <- data.frame(PassengerId = train4test$PassengerId, 
                     Survived_Orig = train4test$Survived_orig,
                     Survived = Prediction)

#Check Improvement
result_train4test$chk <- 0
result_train4test$chk[result_train4test$Survived == result_train4test$Survived_Orig] <- 1
#View(result_train4test)
prop.table(table(result_train4test$chk))

#Real Train data
train <- combi[1:891,]
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID + Deck + Mom + Child,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

#Predict real test data
Prediction <- predict(fit, test,OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = Prediction)
write.csv(submit, file = "myforest4_w_deck.csv", row.names = FALSE)