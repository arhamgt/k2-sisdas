# Melakukan import fungsi LVQ yang didapat pada file LVQ.R
source('LVQ.R')

# Kita mendapatkan bahwa 
# niter = 3000
# alpha = 0.9
# merupakan parameter yang menghasilkan akurasi tertinggi
niter = 3000
alpha = 0.9
cat(sprintf("Matriks perbandingan dari hasil LVQ dengan niter = %d dan alpha = %f\n", niter, alpha))
conmat <- LVQ(niter, alpha)
print(conmat)