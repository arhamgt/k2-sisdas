library(class)
library(caret)
library(ggplot2)


set.seed(123)

data_train = read.csv("train_hp.csv")
data_test = read.csv("test_hp.csv")

#######
data_train = read.csv("train_hp.csv")
dim(data_train)

index = createDataPartition(data_train$price_range, p = .8, list = F)

trainData = data_train[index, ]
testData = data_train[-index, ]

write.csv(trainData,"trainingData.csv",row.names = FALSE)
write.csv(trainData,"testData.csv",row.names = FALSE)

dim(trainData)
dim(testData)

head(trainData)
head(testData)

train = data.matrix(trainData[, 1:20])
test = data.matrix(testData[, 1:20])

summary(train)
summary(test)

train_label = factor(trainData[, "price_range"])
test_label = factor(testData[, "price_range"])
#######



banyak_epoch <- 30

accuracy_vektor <- c()
epoch_vektor <- c()
#######
for(i in seq(1600 , banyak_epoch*1600 , 1600)){
  codeBook = lvqinit(train, train_label, size = 100)
  buildCodeBook = olvq1(train, train_label, codeBook, niter=i, alpha=0.2)
  predict = lvqtest(buildCodeBook, test)
  confusionMatrix(test_label, predict)
  conmat <- confusionMatrix(test_label, predict)
  
  accuracy_vektor <- c(accuracy_vektor,conmat$overall["Accuracy"])
  epoch_vektor <- c(epoch_vektor,i/1600)
}
#######
data_plot           <- cbind(epoch_vektor,accuracy_vektor)
data_plot           <- data.frame(data_plot)
rownames(data_plot) <- NULL

ggplot(data_plot , aes(x = epoch_vektor ,y=accuracy_vektor))+
  geom_line()


print(paste("Epoch Maksimum",data_plot[which.max(data_plot$accuracy_vektor),]))
