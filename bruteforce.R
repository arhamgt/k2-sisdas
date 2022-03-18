# Melakukan import fungsi LVQ yang didapat pada file LVQ.R
source('LVQ.R')

####### MELAKUKAN BRUTE FORCE
##### Brute force dilakukan untuk menentukan epoch dan alpha yang memiliki akurasi paling maksimal
### Melakukan pengaturan terhadap range yang perlu di bruteforce
# Mengatur range epoch yang perlu di bruteforce
awalEpoch   <- 1
akhirEpoch <- 100000
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
    
    # Melakukan fungsi LVQ utama buatan kita yang sudah kita import. Dengan parameter niter 
    # dan alpha yang sedang kita bruteforce.
    conmat <- LVQ(niter, alpha)

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

print(dataPlot)
# Mencari baris dataplot yang mempunyai akurasi yang paling besar
print("Epoch Maksimum")
print(dataPlot[which.max(dataPlot$accuracyVektor),])
#######
