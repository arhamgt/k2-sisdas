library(class)
library(caret)


set.seed(123)

data_train = read.csv("train_hp.csv")
data_test = read.csv("test_hp.csv")

#######
data_train = read.csv("train_hp.csv")
dim(data_train)

index = createDataPartition(data_train$price_range, p = .8, list = F)

trainData = data_train[index, ]
testData = data_train[-index, ]

dim(trainData)
dim(testData)

train = data.matrix(trainData[, 1:20])
test = data.matrix(testData[, 1:20])

summary(train)
summary(test)

train_label = factor(trainData[, "price_range"])
test_label = factor(testData[, "price_range"])
#######

#######
codeBook = lvqinit(train, train_label, size = 100)

buildCodeBook = olvq1(train, train_label, codeBook, niter=100, alpha=0.2)

predict = lvqtest(buildCodeBook, test)

confusionMatrix(test_label, predict)
#######