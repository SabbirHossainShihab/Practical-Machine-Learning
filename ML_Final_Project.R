
# Getting Started
library(caret)
setwd("C:/Users/Mike/OneDrive/MOOCs/ML/project")


# Load datasets and convert missing data into N/A
training <- read.csv("pml-training.csv", na.strings=c("","NA", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("","NA", "#DIV/0!"))

# Remove columns that are not predictors

#the first five columns include non-predicting values (names, timestamps, etc.)
trainingPs <- training[,-(1:5)]

# remove predictors with data that does not vary (all values are roughly the same)
trainingPs <- trainingPs[,-nearZeroVar(trainingPs, saveMetrics = FALSE)]

# remove columns that have NAa 
rem.columns <- names(which(colSums(is.na(trainingPs))>0))
trainingPs <- trainingPs[, !(names(trainingPs) %in% rem.columns)]

# Split the training dataset into a training and validation sets
inTrain <- createDataPartition(y=trainingPs$classe, p=.7, list= FALSE)
trainingSet <- trainingPs[inTrain,]
validationSet <- trainingPs[-inTrain,]

CrossValSummary <- rbind(Original_data = dim(trainingPs), traingin_subset = dim(trainingSet), validation_subset = dim(validationSet))
colnames(CrossValSummary) <- c("observations", "predictors")
CrossValSummary

# fit a random forest model (Ths takes a LONG time!)
modFit <- train(classe~., data=trainingSet, method="rf", prox=TRUE)
modFit_gbm <- train(classe~., data=trainingSet, method="gbm", verbose=FALSE)


# predict results on the validation model to test accuracy...
Predict_rf <- predict(modFit, validationSet)
CM_RF <- confusionMatrix(Predict_rf, validationSet$classe)

Predict_gbm <- predict(modFit_gbm, validationSet)
CM_GBM <- confusionMatrix(Predict_gbm, validationSet$classe)

# predict using the test data set provided
modPredict <- predict(modFit, testing)
