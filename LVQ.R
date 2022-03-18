# Melakukan import library
library(class)
library(caret)
library(ggplot2)

LVQ <- function(niter, alpha){
  # Fungsi ini menerima parameter niter dan alpha, kemudian melakukan operasi LVQ dengan 
  # niter dan alpha sesuai parameter terhadap data yang dimiliki. Kemudian membalikan 
  # confusion matrix hasil perbandingan hasil klasifikasi model LVQ terhadap data test.
  
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
  ####### 
  
  
  
  ####### MELAKUKAN LVQ
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
  return(conmat)
  #######
}