setwd("C:/Users/jleinba/Documents/ASOR_Thesis/Results/6-POS/R1.2-R2.2/EPS_10e-6")
file.names <- list.files("C:/Users/jleinba/Documents/ASOR_Thesis/Results/6-POS/R1.2-R2.2/EPS_10e-6")
file.order <- c(1741,1801,1066,1170,1215,1348,1415,1534,1556,1558,1605,
                1611,1660,1662,1688,1697,1704,1707,1805,1815,
                1945,2022,43,597,731)

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

best_ri <- combinations[,order(RI, decreasing = T)[1:5]]

ri_tables <- list(5)
for (i in 1:5) {
  ri_tables[i] <- paste0("RI.",i,".","_",file.order[best_ri[1,i]],".",file.order[best_ri[2,i]])
}

for (i in 1:5) {
  t1t2 <- data.frame(table(id_df[,best_ri[1,i]], id_df[,best_ri[2,i]]))
  ri_tables[[i]] <- t1t2
  names(ri_tables)[i] <- paste0("RI.",i,".","_",file.order[best_ri[1,i]],".",file.order[best_ri[2,i]])
}

saveRDS(ri_tables, "RI_tables")
