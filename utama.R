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


awal_epoch   <- 1
akhir_epoch <- 100000

accuracy_vektor <- c()
epoch_vektor <- c()
alpha_vektor <- c()
#######
for(i in seq(100*awal_epoch , akhir_epoch*100 , 100)){
  for(j in seq(0.1, 1, 0.2)){
    codeBook = lvqinit(train, train_label, size = 100)
    buildCodeBook = olvq1(train, train_label, codeBook, niter=i, alpha=j)
    predict = lvqtest(buildCodeBook, test)
    confusionMatrix(test_label, predict)
    conmat <- confusionMatrix(test_label, predict)
    
    accuracy_vektor <- c(accuracy_vektor,conmat$overall["Accuracy"])
    epoch_vektor <- c(epoch_vektor,i/100)
    alpha_vektor <- c(alpha_vektor,j)
  }
}
#######
data_plot           <- cbind(epoch_vektor,accuracy_vektor, alpha_vektor)
data_plot           <- data.frame(data_plot)
rownames(data_plot) <- NULL

ggplot(data_plot , aes(x = epoch_vektor ,y=accuracy_vektor))+
  geom_line()


print("Epoch Maksimum")
print(data_plot[which.max(data_plot$accuracy_vektor),])
