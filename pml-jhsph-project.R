library(caret)
library(corrplot)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)




fileUrlTrain = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileUrlTest = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

fileNameTrain = "train.csv"
fileNameTest = "test.csv"


if(!file.exists(fileNameTrain)){
download.file(fileUrlTrain, fileNameTrain, mode = "wb")    
}
if(!file.exists(fileNameTest)){
download.file(fileUrlTest, fileNameTest, mode = "wb")    
}

trainData = read.csv(fileNameTrain)
testData = read.csv(fileNameTest)

## clean data

train = trainData[,8:length(trainData)]
nzv = nearZeroVar(train)
train = train[,-nzv]
nas = apply(train,2, function(x) mean(is.na(x))) > .95
train = train[, -which(nas, nas==FALSE)]
y_train = train[,53]



## split into test and cv set

inTrain = createDataPartition(train$classe, p=0.7, list=FALSE)
training = train[inTrain,]
cv = train[-inTrain,]

##
plot_variables <- training[,-53] %>%
        gather(x, freq)
ggplot(plot_variables, aes(x = freq)) +
        facet_wrap(~x, scales="free_x") +
        geom_histogram()

## random forest
set.seed(1984)
control <- trainControl(method = "cv", number = 3, verboseIter=FALSE)
modelRF <- train(classe ~ ., data = train1, method = "rf", trControl = control)
modelRF$finalModel
predictRF <- predict(modelRF, cv)
confMatRF <- confusionMatrix(predictRF, cv$classe)
confMatRF


## decision tree

set.seed(1984)
modelDT <- rpart(classe ~ ., data = train, method = "class")
fancyRpartPlot(modelDT)
predictDT <- predict(modelDT, cv, type = "class")
confMatDT <- confusionMatrix(predictDT, cv$classe)
confMatDT


## predict test set
cols <- colnames(train[,-53])
test <- testData %>% select(cols)
predictTest <- predict(modelDT, test, type = "class")

#############
modFit <- train(classe~.,data=training, method="rf",prox=TRUE)
modFit

