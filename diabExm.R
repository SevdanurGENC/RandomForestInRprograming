getwd() 
setwd("C:/Users/Nano/Desktop/RFinR")

#divade dataset
diabet <- read.csv("Diabetes.csv")
View(diabet)
str(diabet)
dim(diabet)
head(diabet)
summary(diabet)
hist(diabet$BloodPressure)

set.seed(2)
id <- sample(2,nrow(diabet),
             prob=c(0.7,0.3),
             replace=TRUE)
diabet_train <- diabet[id==1,]
diabet_test <- diabet[id==2,]

#Implement Model
install.packages("randomForest")
library(randomForest)
diabet$Outcome <- as.factor(diabet$Outcome)
diabet_train$Outcome <- as.factor(diabet_train$Outcome)

bestmtry <- tuneRF(diabet_train, diabet_train$Outcome,
                   stepFactor = 1.2, improve = 0.01,
                   trace = T, plot = T)
library(randomForest)
diabet_forest <- randomForest(Outcome~.,data = diabet_train)
diabet_forest
plot(diabet_forest)

diabet_forest$importance #gini index(priority of variables)
 
#Visualize
importance(diabet_forest)#gini index(priority of variables)
varImpPlot(diabet_forest)#gini index(priority of variables)

#Model Validation
pred1_diabet <- predict(diabet_forest,newdata = diabet_test,
                        type = "class")
pred1_diabet
library(caret)
confusionMatrix(table(pred1_diabet,diabet_test$Outcome))
