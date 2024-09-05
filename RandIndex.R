setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/6-POS/R1.2-R2.2/EPS_10e-6")
file.names <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/6-POS/R1.2-R2.2/EPS_10e-6")
install.packages("MixSim")
library(MixSim)

N <- c(331, 377, 174, 110, 55, 50, 10, 55, 39, 25, 61, 46, 27)

id_df <- data.frame(matrix(NA, ncol = length(file.names), nrow = sum(N)))
for (i in 1:25) {
  rds <- readRDS(file.names[i])
  ids <- rds$id
  vec_iter <- numeric()
  for (j in 1:13) {
    vec_book <- ids[j,1:N[j]]
    vec_iter <- c(vec_iter, vec_book)
  }
  id_df[,i] <- vec_iter
}


combinations <- combn(1:25, 2)

RI <- numeric()

for (i in 1:300) {
  RI[i] <- RandIndex(id_df[,combinations[1,i]], id_df[,combinations[2,i]])$AR
}


