# Kita mendapatkan bahwa 
# niter = xxx
# alpha = xxx
# merupakan parameter yang menghasilkan akurasi tertinggi
niter = 1000
alpha = 0.3
cat(sprintf("Matriks perbandingan dari hasil LVQ dengan niter = %d dan alpha = %f\n", niter, alpha))
conmat <- LVQ(1000, 0.3)
print(conmat)