# Dev: Jamensson L. Moura
# Date: 08/10/2016
# Description: Noob

#Importando arquivos CSV
install.packages('RCurl')
library(RCurl)

train_file <- getURL('https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/train.csv?sv=2012-02-12&se=2016-10-18T23%3A46%3A14Z&sr=b&sp=r&sig=pQ6ueKJLHW3eDjOFNXrWFvY7pfyAyvNShdtUN5P4BYs%3D', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
train <- read.csv(textConnection(train_file))

test_file <- getURL('https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/test.csv?sv=2012-02-12&se=2016-10-18T23%3A48%3A12Z&sr=b&sp=r&sig=2ZGlvLa4%2F%2BKcWKggCtGoJRhm1D5zIk8mMHyVNkhxgKw%3D', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
test <- read.csv(textConnection(test_file))

#Adicionando coluna de Sobrevivente na amostra de Teste
test$Survived <- rep(0, 418)

#Submissão número 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Titanic/theyallperish.csv", row.names = FALSE)



#Analisando variável Gênero
prop.table(table(train$Sex))
summary(train$Sex)

prop.table(table(train$Sex,train$Survived),1)

#Alterando regra de sobrevivência na amostra de teste
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#Submissão número  2
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Titanic/theyallperish_2.csv", row.names = FALSE)

#Analisando variável idade
summary(train$Age)

train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Titanic/theyallperish_3.csv", row.names = FALSE)

library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)

 install.packages('rattle')
 install.packages('rpart.plot')
 install.packages('RColorBrewer')
 library(rattle)
 library(rpart.plot)
 library(RColorBrewer)
 
 fancyRpartPlot(fit)
 
 
 Prediction <- predict(fit, test, type = "class")
 submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
 write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
 
 table(Prediction)
 
 
 fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=train,
                method="class", 
                control=rpart.control(minsplit=45, cp=0))
 fancyRpartPlot(fit)
 
 ?rpart.control
 
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=train,
                method="class",
                control=rpart.control(minsplit=20, cp=0 ))
  new.fit <- prp(fit,snip=TRUE)$obj
  fancyRpartPlot(new.fit)
 
  
test$Survived <- NA
test$Fare2 <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split='[,.]')

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))

famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)


train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanic/myfirstdtree_2.csv", row.names = FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Title + FamilySize + FamilyID,
             data=train, 
             method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanic/myfirstdtree_3.csv", row.names = FALSE)

summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)

which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)


 combi$FamilyID2 <- combi$FamilyID
 combi$FamilyID2 <- as.character(combi$FamilyID2)
 combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
 combi$FamilyID2 <- factor(combi$FamilyID2)
 
 install.packages('randomForest')
 library(randomForest)
 set.seed(415)
 
 
 train <- combi[1:891,]
 test <- combi[892:1309,]
 
 fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                       Embarked + Title + FamilySize + FamilyID2,
                     data=train, 
                     importance=TRUE, 
                     ntree=2000)
 
 varImpPlot(fit)
 
  Prediction <- predict(fit, test)
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(submit, file = "titanic/firstforest.csv", row.names = FALSE)
 
 fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare +
                       Embarked + Title + FamilySize + FamilyID2,
                     data=train, 
                     importance=TRUE, 
                     ntree=2000)
 
 varImpPlot(fit)
 
 Prediction <- predict(fit, test)
 submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
 write.csv(submit, file = "titanic/firstforest_2.csv", row.names = FALSE)
 
 
 install.packages('party')
 library(party)
 
  set.seed(415)
    fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID,
                    data = train, 
                    controls=cforest_unbiased(ntree=2000, mtry=3))
    
    Prediction <- predict(fit, test, OOB=TRUE, type = "response")
    
    submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
    write.csv(submit, file = "titanic/firstforest_3.csv", row.names = FALSE)
    
    set.seed(415)
    fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age +  Fare +
                     Embarked + Title + FamilySize + FamilyID,
                   data = train, 
                   controls=cforest_unbiased(ntree=2000, mtry=3))
    
    Prediction <- predict(fit, test, OOB=TRUE, type = "response")
    
    submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
    write.csv(submit, file = "titanic/firstforest_4.csv", row.names = FALSE)
    
    
    set.seed(415)
    fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                     Embarked + Title + FamilySize + FamilyID,
                   data = train, 
                   controls=cforest_unbiased(ntree=5000, mtry=10))
    
    Prediction <- predict(fit, test, OOB=TRUE, type = "response")
    
    submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
    write.csv(submit, file = "titanic/firstforest_5.csv", row.names = FALSE)
    
    
    