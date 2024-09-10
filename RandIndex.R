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

#### Accounting for ambiguities ####
BIC_r1.2_r2.2 <- numeric()
for (i in 1:25) {
  rds <- readRDS(file.names[i])
  BIC_r1.2_r2.2[i] <- round(rds$BIC,2)
}

amb_func <- function(x) {
  z <- numeric()
  y <- order(x, decreasing = T)
  if (sum(x > 0.7) == 1) {
    z <- as.character(y[1])
  } else if (y[1] == 1 & y[2] == 2) {
    z <- as.character("5") # 1 and 2
  } else if (y[1] == 2 & y[2] == 1) {
    z <- as.character("5")
  } else if (y[1] == 1 & y[2] == 3) {
    z <- as.character("6") # 1 and 3
  } else if (y[1] == 3 & y[2] == 1) {
    z <- as.character("6")
  } else if (y[1] == 4 & y[2] == 1){
    z <- as.character("7")
  } else if (y[1] == 2 & y[2] == 3){
    z <- as.character("8") # 2 and 3
  } else if (y[1] == 3 & y[2] == 2){
    z <- as.character("8")
  }
  return(z)
}

id_df <- data.frame(matrix(NA, ncol = length(file.names), nrow = sum(N)))

for (w in 1:25) {
  main_df <- data.frame()
  rds <- readRDS(file.names[order(BIC_r1.2_r2.2)][w])
  rds_gamma <- rds$gamma
  for (i in 1:13) { # 17 for additional books
    gamma_book <- round(rds_gamma[i,1:N[i],],2)
    gamma_prob <- gamma_book > 0.7
    gamma_order <- t(apply(gamma_book, MARGIN = 1, FUN = order))
    results_vec <- factor(apply(gamma_book, MARGIN = 1, FUN = amb_func))
    book_vec <- rep(books[i], N[i])
    sent_vec <- c(1:N[i])
    main_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, ID = results_vec)
    main_df <- rbind(main_df, main_df.sub)
  }
  main_df$ID <- as.numeric(main_df$ID)
  id_df[,w] <- main_df$ID
}

RI <- data.frame(matrix(NA, ncol = 25, nrow = 25), row.names = file.order[order(BIC_r1.2_r2.2)])

for (i in 1:25) {
  for (j in 1:25) {
    RI[i,j] <- round(RandIndex(id_df[,i], id_df[,j])$AR, 3)
  }
}
colnames(RI) <- file.order[order(BIC_r1.2_r2.2)]

#### Level-switching ####
amb_func.2 <- function(x) {
  z <- numeric()
  y <- order(x, decreasing = T)
  if (sum(x > 0.7) == 1) {
    z <- as.character(y[1])
  } else if (y[1] == 1 & y[2] == 2) {
    z <- as.character("5") # 1 and 2
  } else if (y[1] == 2 & y[2] == 1) {
    z <- as.character("5")
  } else if (y[1] == 1 & y[2] == 3) {
    z <- as.character("6") # 1 and 3
  } else if (y[1] == 3 & y[2] == 1) {
    z <- as.character("6")
  } else if (y[1] == 4 & y[2] == 1){
    z <- as.character("7")
  } else if (y[1] == 2 & y[2] == 3){
    z <- as.character("8") # 2 and 3
  } else if (y[1] == 3 & y[2] == 2){
    z <- as.character("8")
  }
  return(z)
}
id_df <- data.frame(matrix(NA, ncol = length(file.names), nrow = sum(N)))


for (w in 1:25) {
  main_df <- data.frame()
  rds <- readRDS(file.names[order(BIC_r1.2_r2.2)][w])
  rds_gamma <- rds$gamma
  for (i in 1:13) { # 17 for additional books
    gamma_book <- round(rds_gamma[i,1:N[i],],2)
    gamma_prob <- gamma_book > 0.7
    gamma_order <- t(apply(gamma_book, MARGIN = 1, FUN = order))
    results_vec <- factor(apply(gamma_book, MARGIN = 1, FUN = amb_func.2))
    book_vec <- rep(books[i], N[i])
    sent_vec <- c(1:N[i])
    main_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, ID = results_vec)
    main_df <- rbind(main_df, main_df.sub)
  }
  main_df$ID <- as.numeric(main_df$ID)
  id_df[,w] <- main_df$ID
}

colnames(id_df) <-  file.order[order(BIC_r1.2_r2.2)]
