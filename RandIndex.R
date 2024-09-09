setwd("C:/Users/jleinba/Documents/ASOR_Thesis/Results/6-POS/R1.2-R2.2/EPS_10e-6")
file.names <- list.files("C:/Users/jleinba/Documents/ASOR_Thesis/Results/6-POS/R1.2-R2.2/EPS_10e-6")
file.order <- c(1741,1801,1066,1170,1215,1348,1415,1534,1556,1558,1605,
                1611,1660,1662,1688,1697,1704,1707,1805,1815,
                1945,2022,43,597,731)

install.packages("MixSim")
library(MixSim)

N <- c(331, 377, 174, 110, 55, 50, 10, 55, 39, 25, 61, 46, 27) # (164, 96, 10, 15) for Hebrews + 1, 2, 3 John

BIC_r1.2_r2.2 <- numeric()
for (i in 1:25) {
  rds <- readRDS(R1.2_R2.2[i])
  BIC_r1.2_r2.2[i] <- round(rds$BIC,2)
}

id_df <- data.frame(matrix(NA, ncol = length(file.names), nrow = sum(N)))
for (i in 1:25) {
  rds <- readRDS(file.names[order(BIC_r1.2_r2.2)][i])
  ids <- rds$id
  vec_iter <- numeric()
  for (j in 1:13) {
    vec_book <- ids[j,1:N[j]]
    vec_iter <- c(vec_iter, vec_book)
  }
  id_df[,i] <- vec_iter
}

RI <- data.frame(matrix(NA, ncol = 25, nrow = 25), row.names = file.order[order(BIC_r1.2_r2.2)])

for (i in 1:25) {
  for (j in 1:25) {
    RI[i,j] <- round(RandIndex(id_df[,i], id_df[,j])$AR, 3)
  }
}
colnames(RI) <- file.order[order(BIC_r1.2_r2.2)]
