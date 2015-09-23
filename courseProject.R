## getting data, loading packages etc

library(caret)

if (!file.exists("pml-training.csv")) {
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
}


alldata <- read.csv("pml-training.csv")
alldata$classe <- as.factor(alldata$classe)

## set seed, split

set.seed(1980)
trainingIndex <- createDataPartition(alldata$classe, p=0.7, list=FALSE)
train <- alldata[trainingIndex,]
test <- alldata[-trainingIndex,]

## Data manipulation on train

train <- train[, -1:-7]

nzv <- nearZeroVar(train)
train <- train[,-nzv]

mostly_filled <- apply(train,2,function(x) {sum(is.na(x))})
remove <- which(mostly_filled>0)

train <- train[,-remove]

## apply the same on test

test <- test[,-1:-7]
test <- test[,-nzv]
test <- test[,-remove]



## Train random forest model

modelFit <- train(classe ~ ., method="rf", data=train, trControl=trainControl(method="cv", number=20))

## check fit on training data

confusionMatrix(train$classe, predict(modelFit, train))

## Check fit on testing data

confusionMatrix(test$classe, predict(modelFit, test))

## 99.44% accurate on test sample


