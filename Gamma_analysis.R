POS <- factor(c("AD", "I", "N", "P", "RA", "V"))
N <- c(331, 377, 174, 110, 55, 50, 10, 55, 39, 25, 61, 46, 27,164, 96)

file.names <- list.files("/Users/josiahleinbach/Downloads/ASOR_Thesis-main/Results/HJ_R1.3-R2.2","EMR") #"C:/Users/jleinba/Documents/ASOR_Thesis/Results/HJ123_R1.2-R2.2/EPS_10e-6"
setwd("/Users/josiahleinbach/Downloads/ASOR_Thesis-main/Results/HJ_R1.3-R2.2")

file.order <- c(1066,1170,1215,1348,1415,1534,1556,1588,1605,1611,1660,1662,1688,
                1697,1704,1707,1741,1801,1805,1815,1945,2022,43,597,731)

BIC_vec <- numeric()
for (i in 1:length(R1.2_R2.2)) {
  rds <- readRDS(R1.2_R2.2[i])
  BIC_vec[i] <- round(rds$BIC,2)
}


r_dist.final <- vector(mode = "list", length = 25)

for (i in 1:25) {
  rds <- readRDS(R1.2_R2.2[order(BIC_vec)][i])
  r_trans <- rds$Gamma
  r_list <- vector(mode = "list", length = 3)
  r_dist <- matrix(data = NA, nrow = 3, ncol = 36)
  for (j in 1:3) {
    r_list[[j]] <- round(r_trans[j,,], 3)
    colnames(r_list[[j]]) <- POS
    rownames(r_list[[j]]) <- POS
    r_dist[j,] <- as.vector(r_list[[j]])
  }
  r_dist.final[[i]] <- dist(r_dist, method = "euclidean")
  assign(paste0("r_", file.num[order(BIC_vec)][i]), r_list)
}

names(r_dist.final) <- file.num[order(BIC_vec)]


# Within style contrasts
r_1662[[2]] - r_1662[[3]]
r_2022[[2]] - r_2022[[3]]

r_2022[[1]] - r_2022[[2]]


r_1662[[2]] - r_1662[[4]]
r_2022[[2]] - r_2022[[4]]

# Across style contrasts
r_1662[[2]] - r_2022[[2]]
r_1662[[3]] - r_2022[[3]]
r_1662[[4]] - r_2022[[4]]


dist(rbind(as.vector(r_1662[[2]]),as.vector(r_2022[[2]])), method = "euclidean")
dist(rbind(as.vector(r_1662[[3]]),as.vector(r_2022[[3]])), method = "euclidean")
dist(rbind(as.vector(r_1662[[4]]),as.vector(r_2022[[4]])), method = "euclidean")


paul_pos.list.1 <- paul_pos.list[-c(16,17)]


rds_1662 <- readRDS(file.names[order(BIC_vec)][1])
id_1662 <- rds_1662$id


pos_mat <- matrix(data = 0, nrow = 4, ncol = 6)

for (i in 1:4) {
  for (j in 1:6) {
    for (k in 1:15) {
      paul_book <- paul_pos.list[[k]]
      id_book <- id_1662[k,1:N[k]]
      id_style <- id_book == i
      paul_book.style <- paul_book[id_style]
      style.tot <- unlist(paul_book.style)
      pos_mat[i,j] <- pos_mat[i,j] + sum(style.tot == POS[j])
    }
  }
  colnames(pos_mat) <- POS
}

pos_totals <-round(colSums(pos_mat)/sum(pos_mat),3)
round((r_2022[[2]][,3]*pos_totals[3])/pos_totals[2],3)


round(pos_mat/rowSums(pos_mat),3)


# N.1 <- c(110,50,377,174,331,50,10,25,39,55,27,61,46)
