---
title: "Prediction Analysis"
author: "Hyejoon Lim"
date: '2020 8 30 '
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Background
The script "predictionPro.R" predicts how people exercise by using the data colled from fitness tracking devices.  
This prediction is based on five different classes of how six young participants perform one set of 10 repetitions of
the Unilateral Dumbbell Biceps Curl:  

1. Class A : Exactly accoriding to the specification
2. Class B: Throwing the elbows to the front 
3. Class C: Lifting the dumbbell only halfway
4. Class D: Lowering the dumbbell only halfway
5. Class E: Throwing the hips to the front 

More information about the experiment that help collect the data can be found on:  
<http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>  

The training data for the prediction model:  
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The testing data:  
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv> 

## Procedure

1. Loading the required packages for the script:  
    
```{r}
library(ggplot2)
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rattle)
```  

2. Downloading the data files, only once, into the working directory using download.file ()

```{r}
trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if (!file.exists('./pml-testing.csv') & !file.exists('./pml-training.csv')){
  download.file(testingUrl,'./pml-testing.csv', mode = 'wb')
  download.file(trainingUrl,'./pml-training.csv', mode = 'wb')  
}
```

3. Loading the files to the workspace with read.csv () 

```{r}
train <- read.csv("pml-training.csv", na.strings = c("NA","#DIV/0!",""))
test <- read.csv("pml-testing.csv", na.strings = c("NA","#DIV/0!",""))
```

4. Cleaning the data frames   
   a. Removing column with NA by preserving only the columns for which the sum of NA values is null.  
```{r}
train <- train[,colSums(is.na(train)) == 0]
test <- test[,colSums(is.na(test)) == 0]
```

   b. Removing column 1 through 7 as they are not required for this prediction
```{r}
train <- train[,-c(1:7)]
test <-test[,-c(1:7)]
``` 

5. Partitioning the training set into a "trainTrain" set on which the model will be built and "trainTest" 
 that will be used for verification
```{r}
inTrain <- createDataPartition(y=train$classe, p = 0.6, list = FALSE)
trainTrain <- train [inTrain, ]
trainTest <- train[-inTrain, ]
```

6. Plotting an histogram of the "classe" variable of "trainTrain" 
```{r}
classe <- trainTrain$classe
ggplot(data.frame(classe), aes (x = classe)) + geom_bar()
```
   
7. Predict Using Decision Tree Model 
```{r}
modFit1 <- rpart(classe ~ ., data=trainTrain, method = "class")
prediction1 <- predict(modFit1, trainTest, type = "class")
fancyRpartPlot(modFit1)
```

8. Predict Using Random Forest Model

## Result 

1. Decision Tree Prediction 
```{r}
## Checking if the predictions and classe variables are same level factors
str(prediction1)
str(trainTest$classe)

## Turning class into the appropriate factor and computing the confusion matrix
factoredClasse <- factor(trainTest$classe, levels = c ("A", "B", "C", "D", "E"))
confusionMatrix(prediction1, factoredClasse)
```

Comparison Histogram:  
```{r}
p1 <- data.frame(class = prediction1)
cla <- data.frame(class = trainTest$classe)
p1$type <- 'predicted'
cla$type <- 'actual'
v <- rbind(p1, cla)
ggplot(v, aes(class, fill = type))+ geom_histogram(alpha = 0.7,
                                                   stat = "count", position = 'identity')
```

2. Random Forest Predict 
```{r}
modFit2 <- randomForest(as.factor(classe) ~. , data = trainTrain)
prediction2 <- predict(modFit2, trainTest, type = "class")
confusionMatrix(prediction2, factoredClasse)
```

Comparison Histogram:  
```{r}
 p2 <- data.frame(class = prediction2)
p2$type <- 'predicted'
v2 <- rbind(p2, cla)
ggplot(v2, aes(class, fill = type))+ geom_histogram(alpha = 0.7,
                                                   stat = "count", position = 'identity')
```

## Conclusion

Based on the results "Random Forest" is the best prediction model for this study because it has an accurary of 0.994 that 
is the highest value. 