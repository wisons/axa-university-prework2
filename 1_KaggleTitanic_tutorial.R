# Set working directory and import datafiles
mWorkPath <<- "C:/Users/KTB_User/Desktop/Data Transformation/AXA University/Kaggle"
setwd(mWorkPath)

#Load File
library(readr)
train <- read_csv(paste(mWorkPath,"/train.csv",sep=""))
#View(train)
test <- read_csv(paste(mWorkPath,"/test.csv",sep=""))
#View(test)

#Check Target & Some variables
prop.table(table(train$Survived))
table(train$Sex)
prop.table(table(train$Sex, train$Survived),1)
prop.table(table(train$Sex, train$Survived),2)

#combi Train & Test
test$Survived <- NA
combi <- rbind(train, test)
View(combi)

#Create new feature
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
#combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
#combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

#Clear missing age and others
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'


famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
which(is.na(combi$Embarked))
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Factor
combi$Pclass <- factor(combi$Pclass)
combi$Sex <- factor(combi$Sex)
combi$Embarked <- factor(combi$Embarked)
combi$Title <- factor(combi$Title)

#Split train & test data
train4train <- combi[1:500,]
train4test <- combi[501:891,]
test <- combi[892:1309,]
#train4test <- train4test[ , -which(names(train4test) %in% c("Survived"))]
colnames(train4test)[2] <- "Survived_orig"
View(train4test)
summary(train4train)



#Predict forest of conditional inference trees
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train4train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, train4test,OOB=TRUE)
submit <- data.frame(PassengerId = train4test$PassengerId, 
                     Survived_Orig = train4test$Survived_orig,
                     Survived = Prediction)

#Check Improvement
submit$chk <- 0
submit$chk[submit$Survived == submit$Survived_orig] <- 1
View(submit)
prop.table(table(submit$chk))

#Predict real test data
Prediction <- predict(fit, test,OOB=TRUE)
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = Prediction)
write.csv(submit, file = "myforest.csv", row.names = FALSE)


