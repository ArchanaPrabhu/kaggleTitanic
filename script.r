> setwd("C:/Users/Vidya Prabhu/Downloads/titanic")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
table(train$Survived)
prop.table(table(train$Survived))
test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
 write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
 summary(train$Sex)
table(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1)
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "womenSurvive.csv", row.names = FALSE)
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
  write.csv(submit, file = "subm3.csv", row.names = FALSE)
  
  library(rpart)
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data=train,
               method="class")
  #representing using graphics
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
   
   fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=train,
                method="class", 
                control=rpart.control(minsplit=2, cp=0))
   fancyRpartPlot(fit)
   
   fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=train,
                method="class",
                control=rpart.control( your controls ))
   new.fit <- prp(fit,snip=TRUE)$obj
    fancyRpartPlot(new.fit)
    
    #read the train data set again
    
    train <- read_csv("C:/Users/Vidya Prabhu/Downloads/titanic/train.csv")
   train$Name[1]
   test$Survived <- NA
   combi <- rbind(train, test)
   str(test$Survived)
   
   combi$Name <- as.character(combi$Name)
   combi$Name[1]
   #splitting the string to get to know the designation
   strsplit(combi$Name[1], split='[,.]')
   strsplit(combi$Name[1], split='[,.]')[[1]][2]
   combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})  
   combi$Title <- sub(' ', '', combi$Title)   
   table(combi$Title)
   
   
   #combine french and english salutations
   
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
   sample(1:10, replace = TRUE)
   
   
   #replacing the missing age values
   
   Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                   data=combi[!is.na(combi$Age),], 
                   method="anova")
   combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
   
   #replace the null value of embarked 
   which(combi$Embarked == '')
   combi$Embarked[c(62,830)] = "S"
   combi$Embarked <- factor(combi$Embarked)
   
   
   #Fare
   
   summary(combi$Fare)
   which(is.na(combi$Fare))
   combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
   
   
   combi$FamilyID2 <- combi$FamilyID
   combi$FamilyID2 <- as.character(combi$FamilyID2)
   combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
   combi$FamilyID2 <- factor(combi$FamilyID2)
   
   
   train <- combi[1:891,]
   test <- combi[892:1309,]
   
   
   install.packages('randomForest')
 library(randomForest)
   
   set.seed(415)
   
   fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare
                         + Embarked + Title + FamilySize + FamilyID2,
                                            data=train, 
                                           importance=TRUE, 
                                       ntree=2000)
   train$Sex <- factor(train$Sex)
   
   
   
   varImpPlot(fit)
   
   
   
   #The accuracy one tests to see how worse the model 
   #performs without each variable, so a high decrease 
   #in accuracy would be expected for very predictive variables.
   
   
   install.packages('party')
   library(party) 
   
   set.seed(415)
   fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID,
                    data = train, 
                    controls=cforest_unbiased(ntree=2000, mtry=3))