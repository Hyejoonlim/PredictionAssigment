## The required packages

library(lattice)
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rattle)


set.seed(1234)

## Downloading the files 

trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if (!file.exists('./pml-testing.csv') & !file.exists('./pml-training.csv')){
  download.file(testingUrl,'./pml-testing.csv', mode = 'wb')
  download.file(trainingUrl,'./pml-training.csv', mode = 'wb')  
}

## Loading the files

train <- read.csv("pml-training.csv", na.strings = c("NA","#DIV/0!",""))
test <- read.csv("pml-testing.csv", na.strings = c("NA","#DIV/0!",""))

## Deleting columns with missing values only

train<-train[,colSums(is.na(train)) == 0]
test<-test[,colSums(is.na(test)) == 0]

## Removing non-relevant variables

train <- train[,-c(1:7)]
test <-test[,-c(1:7)]


## Partitioning the training set 

inTrain <- createDataPartition(y=train$classe, p = 0.6, list = FALSE)
trainTrain <- train [inTrain, ]
trainTest <- train[-inTrain, ]

## Visualizing the training set
classe <- trainTrain$classe
ggplot(data.frame(classe), aes (x = classe)) + geom_bar()

## Decision Tree model with its plot and prediction

modFit1 <- rpart(classe ~ ., data=trainTrain, method = "class")
fancyRpartPlot(modFit1)
prediction1 <- predict(modFit1, trainTest, type = "class")

## Plot the prediction in comparison to the actual value 

p1 <- data.frame(class = prediction1)
cla <- data.frame(class = trainTest$classe)
p1$type <- 'predicted'
cla$type <- 'actual'
v <- rbind(p1, cla)
ggplot(v, aes(class, fill = type))+ geom_histogram(alpha = 0.7,
                                                   stat = "count", position = 'identity')
## Checking if the predictions and classe variables are same level factors

str(prediction1)
str(trainTest$classe)

## Turning the classe variable into appropriate factor
factoredClasse <- factor(trainTest$classe, levels = c ("A", "B", "C", "D", "E"))

## Testing the prediction
confusionMatrix(prediction1, factoredClasse)

## Random forest model with its prediction 

modFit2 <- randomForest(as.factor(classe) ~. , data = trainTrain)
prediction2 <- predict(modFit2, trainTest, type = "class")

## Plot the prediction in comparison to the actual value 

p2 <- data.frame(class = prediction2)
p2$type <- 'predicted'
v2 <- rbind(p2, cla)
ggplot(v2, aes(class, fill = type))+ geom_histogram(alpha = 0.7,
                                                   stat = "count", position = 'identity')

## Testing the result 

confusionMatrix(prediction2, factoredClasse)



