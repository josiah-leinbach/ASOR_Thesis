#### BIC Matrix ####
BIC_mat <- matrix(data = NA, ncol = 2, nrow = 3)

### R1.1-R2.1 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.1_R2.1")
R1.1_R2.1 <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.1_R2.1","EMR")

BIC_r1.1_r2.1 <- numeric(length(R1.1_R2.1))
for (i in 1:length(R1.1_R2.1)) {
  EMR <- readRDS(R1.1_R2.1[i])
  BIC_r1.1_r2.1[i] <- EMR$BIC
}

BIC_mat[1,1] <- min(BIC_r1.1_r2.1)


### R1.2-R2.1 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.2_R2.1")
R1.2_R2.1 <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.2_R2.1","EMR")

BIC_r1.2_r2.1 <- numeric(length(R1.2_R2.1))
for (i in 1:length(R1.2_R2.1)) {
  EMR <- readRDS(R1.2_R2.1[i])
  BIC_r1.2_r2.1[i] <- EMR$BIC
}

BIC_mat[2,1] <- min(BIC_r1.2_r2.1)


### R1.2-R2.2 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.2_R2.2")
R1.2_R2.2 <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.2_R2.2","EMR")
R1.2_R2.2 <- R1.2_R2.2[-c(5)]

BIC_r1.2_r2.2 <- numeric(length(R1.2_R2.2))
for (i in 1:length(R1.2_R2.2)) {
  EMR <- readRDS(R1.2_R2.2[i])
  BIC_r1.2_r2.2[i] <- EMR$BIC
}

BIC_mat[2,2] <- min(BIC_r1.2_r2.2)


### R1.3-R2.1 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.3_R2.1")
R1.3_R2.1 <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.3_R2.1","EMR")

BIC_r1.3_r2.1 <- numeric(length(R1.3_R2.1))
for (i in 1:length(R1.3_R2.1)) {
  EMR <- readRDS(R1.3_R2.1[i])
  BIC_r1.3_r2.1[i] <- EMR$BIC
}

BIC_mat[3,1] <- min(BIC_r1.3_r2.1)


### R1.3-R2.2 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.3_R2.2")
R1.3_R2.2 <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.3_R2.2","EMR")

BIC_r1.3_r2.2 <- numeric(length(R1.3_R2.2))
for (i in 1:length(R1.3_R2.2)) {
  EMR <- readRDS(R1.3_R2.2[i])
  BIC_r1.3_r2.2[i] <- EMR$BIC
}

BIC_mat[3,2] <- min(BIC_r1.3_r2.2)


### R1.1-R2.2 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.1_R2.2")
R1.1_R2.2 <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.1_R2.2","EMR")

BIC_r1.1_r2.2 <- numeric(length(R1.1_R2.2))
for (i in 1:length(R1.1_R2.2)) {
  EMR <- readRDS(R1.1_R2.2[i])
  BIC_r1.1_r2.2[i] <- EMR$BIC
}

BIC_mat[1,2] <- min(BIC_r1.1_r2.2)


#### BIC Matrix: Hebrews + Johannine books ####
BIC.HJ123_mat <- matrix(data = NA, ncol = 2, nrow = 3)

### R1.1-R2.1 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.1_R2.1")
R1.1_R2.1_HJ <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.1_R2.1","EMR")

BIC_r1.1_r2.1_hj <- numeric(length(R1.1_R2.1_HJ))
for (i in 1:length(R1.1_R2.1_HJ)) {
  EMR <- readRDS(R1.1_R2.1_HJ[i])
  BIC_r1.1_r2.1_hj[i] <- EMR$BIC
}

BIC.HJ123_mat[1,1] <- min(BIC_r1.1_r2.1_hj)


### R1.2-R2.1 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.2_R2.1")
R1.2_R2.1_HJ <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.2_R2.1","EMR")

BIC_r1.2_r2.1_hj <- numeric(length(R1.2_R2.1_HJ))
for (i in 1:length(R1.2_R2.1_HJ)) {
  EMR <- readRDS(R1.2_R2.1_HJ[i])
  BIC_r1.2_r2.1_hj[i] <- EMR$BIC
}

BIC.HJ123_mat[2,1] <- min(BIC_r1.2_r2.1_hj)


### R1.2-R2.2 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.2_R2.2")
R1.2_R2.2_HJ <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.2_R2.2","EMR")

BIC_r1.2_r2.2_hj <- numeric(length(R1.2_R2.2_HJ))
for (i in 1:length(R1.2_R2.2_HJ)) {
  EMR <- readRDS(R1.2_R2.2_HJ[i])
  BIC_r1.2_r2.2_hj[i] <- EMR$BIC
}

BIC.HJ123_mat[2,2] <- min(BIC_r1.2_r2.2_hj)


### R1.3-R2.1 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.3_R2.1")
R1.3_R2.1_HJ <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.3_R2.1","EMR")

BIC_r1.3_r2.1_hj <- numeric(length(R1.3_R2.1_HJ))
for (i in 1:length(R1.3_R2.1_HJ)) {
  EMR <- readRDS(R1.3_R2.1_HJ[i])
  BIC_r1.3_r2.1_hj[i] <- EMR$BIC
}

BIC.HJ123_mat[3,1] <- min(BIC_r1.3_r2.1_hj)


### R1.3-R2.2 ###

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.3_R2.2")
R1.3_R2.2_HJ <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.3_R2.2","EMR")

BIC_r1.3_r2.2_hj <- numeric(length(R1.3_R2.2_HJ))
for (i in 1:length(R1.3_R2.2_HJ)) {
  EMR <- readRDS(R1.3_R2.2_HJ[i])
  BIC_r1.3_r2.2_hj[i] <- EMR$BIC
}

BIC.HJ123_mat[3,2] <- min(BIC_r1.3_r2.2_hj)


### R1.1-R2.2 ###
setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.1_R2.2")
R1.1_R2.2_HJ <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.1_R2.2","EMR")

BIC_r1.1_r2.2_hj <- numeric(length(R1.1_R2.2_HJ))
for (i in 1:length(R1.1_R2.2_HJ)) {
  EMR <- readRDS(R1.1_R2.2_HJ[i])
  BIC_r1.1_r2.2_hj[i] <- EMR$BIC
}

BIC.HJ123_mat[1,2] <- min(BIC_r1.1_r2.2_hj)

######## Use (R1 = 2, R2 = 1) for Pauline canon and for test books ########
#### Build plots ####
# Pauline corpus

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.2_R2.1")
R1.2_R2.1 <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.2_R2.1","EMR")

L <- 13
N <- numeric(0)
for (i in 1:L) {
  x <- readRDS(R1.2_R2.1[1])
  y <- x$id
  z <- y[i,]
  N[i] <- sum(!is.na(z))
}

sent_ids <- list()
for (i in 1:length(R1.2_R2.1)) {
  EMR <- readRDS(R1.2_R2.1[i])
  sent_ids[[i]] <- EMR$id
}

sent_array <- list()
for (i in 1:L) {
  sent_mat <- data.frame(matrix(NA, nrow = 25, ncol = N[i]))
  for (j in 1:25) {
    sent_book.iter <- sent_ids[[j]][i,1:N[i]]
    sent_mat[j,] <- sent_book.iter 
  }
  sent_array[[i]] <- sent_mat
  
}

library(ggplot2)
library(dplyr)

book_prop <- list()
books <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", 
           "Col", "Thes.2", "Tim.1", "Tim.2", "Tit")

for (i in 1:L) {
  book_mat <- sent_array[[i]]
  book_total <- data.frame(matrix(NA, nrow = 3, ncol = N[i]))
  for (j in 1:N[i]) {
    for (k in 1:3) {
      book_total[k,j] <- sum(book_mat[,j] == k)
    }
  }
  book_prop[[i]] <- t(book_total / 25)
  colnames(book_prop[[i]]) <- factor(c("R1","R2","R3"))
  rownames(book_prop[[i]]) <- c(1:N[i])
  book_sub <- data.frame(book_prop[[i]])
  book_sub <- book_sub %>%
    mutate(Book = factor(rep(books[i],N[i]))) %>%
    mutate(Sentence = 1:N[i]) %>%
    relocate(Book, .before = R1) %>%
    relocate(Sentence, .before = R1)
  book_prop[[i]] <- book_sub
}

saveRDS(book_prop, "Book_prop.Paul")

# Pauline corpus + Hebrews and Johannine epistles
setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.2_R2.1")
R1.2_R2.1_HJ <- list.files("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.2_R2.1","EMR")

L <- 17
N <- numeric(0)
for (i in 1:L) {
  x <- readRDS(R1.2_R2.1_HJ[1])
  y <- x$id
  z <- y[i,]
  N[i] <- sum(!is.na(z))
}

sent_ids <- list()
for (i in 1:length(R1.2_R2.1_HJ)) {
  EMR <- readRDS(R1.2_R2.1_HJ[i])
  sent_ids[[i]] <- EMR$id
}

sent_array <- list()
for (i in 1:L) {
  sent_mat <- data.frame(matrix(NA, nrow = 16, ncol = N[i]))
  for (j in 1:16) {
    sent_book.iter <- sent_ids[[j]][i,1:N[i]]
    sent_mat[j,] <- sent_book.iter 
  }
  sent_array[[i]] <- sent_mat
  
}

book_prop <- list()
books <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", 
           "Col", "Thes.2", "Tim.1", "Tim.2", "Tit",
           "Heb", "John.1", "John.2", "John.3")

for (i in 1:L) {
  book_mat <- sent_array[[i]]
  book_total <- data.frame(matrix(NA, nrow = 3, ncol = N[i]))
  for (j in 1:N[i]) {
    for (k in 1:3) {
      book_total[k,j] <- sum(book_mat[,j] == k)
    }
  }
  book_prop[[i]] <- t(book_total / 16)
  colnames(book_prop[[i]]) <- factor(c("R1","R2","R3"))
  rownames(book_prop[[i]]) <- c(1:N[i])
  book_sub <- data.frame(book_prop[[i]])
  book_sub <- book_sub %>%
    mutate(Book = factor(rep(books[i],N[i]))) %>%
    mutate(Sentence = 1:N[i]) %>%
    relocate(Book, .before = R1) %>%
    relocate(Sentence, .before = R1)
  book_prop[[i]] <- book_sub
}
