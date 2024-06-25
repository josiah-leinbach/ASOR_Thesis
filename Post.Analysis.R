# Import RDS files
rds_1.1 <- readRDS("EMR-R1.1-R2.1-9pos.rds")
rds_1.2 <- readRDS("EMR-R1.1-R2.2-9pos.rds")
rds_2.1 <- readRDS("EMR-R1.2-R2.1-9pos.rds")
rds_2.2 <- readRDS("EMR-R1.2-R2.2-9pos.rds")
rds_3.1 <- readRDS("EMR-R1.3-R2.1-9pos.rds")
rds_3.2 <- readRDS("EMR-R1.3-R2.2-9pos.rds")
rds_3.3 <- readRDS("EMR-R1.3-R2.3-9pos.rds")

# Build matrix
BIC_mat <- matrix(data = NA, nrow = 3, ncol = 3)
BIC_mat[1,1] <- rds_1.1$BIC
BIC_mat[1,2] <- rds_1.2$BIC
BIC_mat[2,1] <- rds_2.1$BIC
BIC_mat[2,2] <- rds_2.2$BIC
BIC_mat[3,1] <- rds_3.1$BIC
BIC_mat[3,2] <- rds_3.2$BIC
BIC_mat[3,3] <- rds_3.3$BIC

rownames(BIC_mat) <- c("R1 = 1", "R1 = 2", "R1 = 3")
colnames(BIC_mat) <- c("R2 = 1", "R2 = 2", "R2 = 3")

# Build table for (R1 = 2, R2 = 1)
styles_mat <- matrix(data = 0, nrow = 13, ncol = 3)
id <- rds_2.1$id

for (i in 1:13) {
  for (j in 1:3) {
    styles_mat[i,j] <- sum(id[i,] == j, na.rm = T)
  }
}
