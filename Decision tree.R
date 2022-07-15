Iris = read.csv("A:/Sparks Internship/Task 6/Iris.csv")
#View(Iris)
Iris= Iris[,-1]

sum(is.na(Iris))
library(caret)
library(rpart.plot)


#train & Test Splitting
#60% Train
#40% Test
set.seed(52307831)
data_60_ = floor((nrow(Iris)*0.6))
train_index_ = sample(nrow(Iris),data_60_)
Iris_train = Iris[train_index_,]
Iris_test = Iris[-train_index_,]
dim(Iris_train)
dim(Iris_test)
dim(Iris)

set.seed(81754603)
crtl = trainControl(method = "repeatedcv",number = 5,repeats = 10)
Decision_tree = train(factor(Species)~.,data = Iris_train,method = "rpart",
                      trControl = crtl,
                      metric = "Accuracy",parms = list(split = "information"),tuneLength = 100)
Decision_tree_fit = predict(Decision_tree,newdata = Iris_test)
confusionMatrix(table(Decision_tree_fit,Iris_test$Species))

plot(Decision_tree)
plot(Decision_tree$finalModel)
text(Decision_tree$finalModel)


rpart.plot(Decision_tree$finalModel)

Decision_tree$bestTune
