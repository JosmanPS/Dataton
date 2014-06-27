# Constructing de data.
library(randomForest)
library(caret)

# reading-in data
data <- read.csv("C:/Users/hp/Dropbox/Sibila/Projects/Dataton/Output/Data/data_pred.csv")
set.seed(1234321)

# Remove zero variance predictors.
data <- data[, -c(19, 18, 15, 10, 9, 8, 7)]
data$crimen <- as.factor(data$crimen)
# Segmentate the data in training and test.
inTrain <- createDataPartition(y = data$crimen, p =  .75,
                               list = F)
train.data <- data[inTrain,]
test.data <- data[-inTrain,]

# random forests

forest.fit <-  randomForest(crimen~., data = train.data, ntree = 1000,
                         importance = T)
forest.fit$type
varImpPlot(forest.fit)

# party kit
library(rpart)
g_rpart <- rpart(crimen~., data = train.data)

g_party <- as.party(g_rpart)
plot(g_party, "simple")

# boosting algorithm
gbmGrid <- expand.grid(.interaction.depth = seq(1, 7, by = 2),
                       .n.trees = seq(100, 1000, by = 50),
                       .shrinkage = c(0.01, .1))
gbmTune <- train(crimen ~., data = train.data, method = "gbm",
                 tuneGrid = gbmGrid,
                 verbose = FALSE)
#
### results
gbmTune
ggplot(gmTune)
#fileConn <- file("C:/Users/hp/Desktop/results dataton/data/results_boosting.txt")
#writeLines(gbmTune, fileConn)
#close(fileConn)

# prediction
gbm.pred <- predict(gbmTune, test.data)
confusionMatrix(gbm.pred, test.data$crimen)

roc(test.data$crimen,gbm.pred)

# random forest
forest.pred <- predict(forest.fit, newdata = test.data)
mean((forest.pred!=test.data$crimen))

rpart1a <- as.party(forest.fit[[1]])
plot(rpart1a)



