# Melakukan import library
library(class)
library(caret)
library(ggplot2)

# Melakukan seeding, seeding dilakukan sehingga tidak terjadi kejadian acak pada fungsi,
# sehingga hasil akan selalu sama jika mempunyai input yang sama.
set.seed(123)

####### MEMBACA DAN PROSES DATA

# Membuka file data dan membacanya ke program
dataTrain = read.csv("train_hp.csv")
dim(dataTrain) # [1] 2000   21

# Membagi data menjadi data training dan data test
index = createDataPartition(dataTrain$price_range, p = .8, list = F)
trainData = dataTrain[index, ]
testData = dataTrain[-index, ]
dim(trainData) # [1] 1600   21
dim(testData) # [1] 400  21

# Mengeprint 5 data teratas dari dataframe data train dan test
head(trainData)
head(testData)

# Mengubah dataframe menjadi matrix, sehingga bisa dilakukan proses selanjutnya,
# dan juga menghilangkan label klasifikasi pada dataframe test dan train
trainDataMatrix = data.matrix(trainData[, 1:20])
testDataMatrix = data.matrix(testData[, 1:20])

# Mengecek apakah data sudah benar
summary(trainDataMatrix)
summary(testDataMatrix)

# Membuat vector yang menampung label data training dan test
train_label = factor(trainData[, "price_range"])
test_label = factor(testData[, "price_range"])




####### MELAKUKAN BRUTE FORCE
##### Brute force dilakukan untuk menentukan epoch dan alpha yang memiliki akurasi paling maksimal

### Melakukan pengaturan terhadap range yang perlu di bruteforce

# Mengatur range epoch yang perlu di bruteforce
awalEpoch   <- 1
akhirEpoch <- 10
incrementEpoch <- 100

# Mengatur range alpha yang perlu di bruteforce
awalAlpha   <- 0.1
akhirAlpha <- 1
incrementAlpha <- 0.2

# Membuat dataframe kosong untuk menampung akurasi, epoch, dan alpha untuk masing-masing loop
dataPlot <- data.frame(
  accuracyVektor = c(),
  epochVektor =  c(),
  alphaVektor =  c()
)




# Melakukan looping untuk mencari epoch(niter) dan alpha yang memiliki akurasi paling baik
for(niter in seq(awalEpoch*incrementEpoch , akhirEpoch*incrementEpoch , incrementEpoch)){
  # niter = 100, 200, 300, 400, ...
  for(alpha in seq(awalAlpha, akhirAlpha, incrementAlpha)){
    # alpha = 0.1, 0.3, 0.5, ...
    
    #### Melakukan LVQ Utama
    # Membuat codebook LVQ, codebook digunakan untuk memformat data sehingga bisa masuk 
    # ke fungsi selanjutnya
    codeBook = lvqinit(trainDataMatrix, train_label, size = 100)
    
    # Melakukan fungsi LVQ utama, disini kita memasukan codebook yang sudah dibuat sebelumnya,
    # kita juga memasukan parameter epoch (niter) dan alpha awal. Fungsi ini menghasilkan 
    # model hasil dari LVQ, berisi klasifikasi yang dihasilkan dari masing-masing data
    classifiedCodebook = olvq1(trainDataMatrix, train_label, codeBook, niter=niter, alpha=alpha)
    
    # Melakukan testing model yang sudah dibuat terhadap data test. Menghasilkan variabel
    # yang berisi hasil klasifikasi yang dilakukan.
    predict = lvqtest(classifiedCodebook, testDataMatrix)
    
    # Mencocokan hasil klasifikasi yang dihasilkan dengan label test, kemudian
    # membuat confusion matrix dari perbandingan tersebut.
    conmat <- confusionMatrix(test_label, predict)

    # Menambahkan baris baru pada dataframe akurasi dan mencatat epoch serta alpha.
    newDataPlot = data.frame(
      accuracyVektor = c(conmat$overall["Accuracy"]),
      epochVektor = c(niter),
      alphaVektor = c(alpha)
    )
    dataPlot <- rbind(dataPlot,newDataPlot)
  }
}
#######

####### MENCARI ALPHA DAN EPOCH DENGAN AKURASI TERTINGGI

# Menghilangkan rowname sehingga lebih rapih
rownames(dataPlot) <- NULL
# Melakukan plotting pengaruh epoch terhadap akurasi
ggplot(dataPlot , aes(x = epochVektor ,y=accuracyVektor))+
  geom_line()
# Melakukan plotting pengaruh alpha terhadap akurasi
ggplot(dataPlot , aes(x = alphaVektor ,y=accuracyVektor))+
  geom_line()

# Mencari baris dataplot yang mempunyai akurasi yang paling besar
print("Epoch Maksimum")
print(dataPlot[which.max(dataPlot$accuracyVektor),])
#######
