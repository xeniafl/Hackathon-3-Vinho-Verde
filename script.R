library(readr)
library(ggplot2)
library(corrplot)
library(caret)
winedf <- read_csv("C:/Users/xenia/Desktop/Ubiqum/Hackathon/Hackathon 3 - Vinho Verde/data/training.csv", col_types = cols(X1 = col_skip()))
validation <- read_csv("C:/Users/xenia/Desktop/Ubiqum/Hackathon/Hackathon 3 - Vinho Verde/data/validation.csv", col_types = cols(X1 = col_skip()))

str(winedf)
summary(winedf)

ggplot(winedf, aes(x = quality)) + geom_bar()
table(winedf$quality)

boxplot(winedf)

ggplot(winedf, aes(x = quality, y = fixed.acidity)) + geom_point()
ggplot(winedf, aes(x = quality, y = volatile.acidity)) + geom_point()
ggplot(winedf, aes(x = quality, y = citric.acid)) + geom_point()
ggplot(winedf, aes(x = quality, y = residual.sugar)) + geom_point()
ggplot(winedf, aes(x = quality, y = chlorides)) + geom_point()
ggplot(winedf, aes(x = quality, y = free.sulfur.dioxide)) + geom_point()
ggplot(winedf, aes(x = quality, y = total.sulfur.dioxide)) + geom_point()
ggplot(winedf, aes(x = quality, y = density)) + geom_point()
ggplot(winedf, aes(x = quality, y = pH)) + geom_point()
ggplot(winedf, aes(x = quality, y = sulphates)) + geom_point()
ggplot(winedf, aes(x = quality, y = alcohol)) + geom_point()

correlationmatrix <- cor(winedf)
corrplot(correlationmatrix, type = "upper", method = "number",
         number.cex = .6, tl.cex = 0.5)
corrplot(correlationmatrix, type = "upper", number.cex = .6, tl.cex = 0.5)

ggplot(winedf, aes(x = fixed.acidity)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = volatile.acidity)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = citric.acid)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = residual.sugar)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = chlorides)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = free.sulfur.dioxide)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = total.sulfur.dioxide)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = density)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = pH)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = sulphates)) + geom_density() + facet_wrap(~ quality)
ggplot(winedf, aes(x = alcohol)) + geom_density() + facet_wrap(~ quality)

winedfCOR <- as.data.frame(cbind(winedf$pH, winedf$alcohol, winedf$density, 
                     winedf$total.sulfur.dioxide, winedf$free.sulfur.dioxide, winedf$quality))
colnames(winedfCOR) <- c("pH", "alcohol", "density", "total.sulfur.dioxide", "free.sulfur.dioxide", "quality")

winedf$residual.sugar <- NULL

set.seed(573)
inTrain <- createDataPartition(y = winedf$quality, p = .80, list = FALSE)
training <- winedf[inTrain,]
testing <- winedf[-inTrain,]

# models <- c("svmLinear", "kknn", "rf", "lm")
#   
# resumCOR2 <- c()
# resumseed <- c()
# 
# 
#   
# for (j in models) {
#    
#   fit <- train(quality ~ ., data = training, method = j)
#   predictedquality <- predict(fit, testing)
#   hola <- postResample(pred = predictedquality, obs = testing$quality)
#   resumCOR2 <- cbind(hola, resumCOR2)
#   
# }
# 
# colnames(resumCOR2) <- models

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(quality ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit
plot(knnFit)

knnPredict <- predict(knnFit, newdata = testing)
knnPredict <- round(knnPredict, digits = 0)
postResample(pred = knnPredict, obs = testing$quality)

knnPredict <- as.factor(knnPredict)
testing$quality <- as.factor(testing$quality)

confusionMatrix(knnPredict, testing$quality)

#########################################################


ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(quality ~ ., data = winedf, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit
plot(knnFit)

validation$prediction <- predict(knnFit, newdata = validation)
validation$prediction <- round(validation$prediction, digits = 0)

validation$quality <- NULL

write.csv(validation, file = "submission1.csv", row.names = F)

