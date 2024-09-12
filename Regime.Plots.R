library(ggplot2)

R1.2_R2.2 <- list.files("C:/Users/jleinba/Documents/ASOR_Thesis/Results/HJ123_R1.2-R2.1/EPS_10e-6","EMR") #"C:/Users/jleinba/Documents/ASOR_Thesis/Results/HJ123_R1.2-R2.2/EPS_10e-6"
setwd("C:/Users/jleinba/Documents/ASOR_Thesis/Results/HJ123_R1.2-R2.1/EPS_10e-6")
N <- c(331, 377, 174, 110, 55, 50, 10, 55, 39, 25, 61, 46, 27,164, 96, 10, 15) # for Hebrews + 1, 2, 3 John
books <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", "Col", "Thes.2", "Tim.1", "Tim.2", "Tit",
           "Heb", "John.1", "John.2", "John.3")

BIC_r1.2_r2.2 <- numeric()
for (i in 1:25) {
  rds <- readRDS(R1.2_R2.2[i])
  BIC_r1.2_r2.2[i] <- round(rds$BIC,2)
}
prop_amb <- numeric(25)

file.num <- c(1066,1170,1215,1348,1415,1534,1556,1588,1605,1611,1660,1662,1688,
              1697,1704,1707,1741,1801,1805,1815,1945,2022,43,597,731)

amb_func <- function(x) {
  z <- numeric()
  y <- order(x, decreasing = T)
  if (sum(x > 0.7) == 1) {
    z <- as.character(y[1])
  } else if (y[1] == 1 & y[2] == 2) {
    z <- as.character("1_2") # 1 and 2
  } else if (y[1] == 2 & y[2] == 1) {
    z <- as.character("1_2")
  } else if (y[1] == 1 & y[2] == 3) {
    z <- as.character("1_3") # 1 and 3
  } else if (y[1] == 3 & y[2] == 1) {
    z <- as.character("1_3")
  } else if (y[1] == 1 & y[2] == 4) {
    z <- as.character("1_4") # 1 and 4
  } else if (y[1] == 4 & y[2] == 1){
    z <- as.character("1_4")
  } else if (y[1] == 2 & y[2] == 3){
    z <- as.character("2_3") # 2 and 3
  } else if (y[1] == 3 & y[2] == 2){
    z <- as.character("2_3")
  } else if (y[1] == 2 & y[2] == 4){
    z <- as.character("2_4") # 2 and 4
  } else if (y[1] == 4 & y[2] == 2){
    z <- as.character("2_4")
  } else if (y[1] == 3 & y[2] == 4){
    z <- as.character("3_4") # 3 and 4
  } else if (y[1] == 4 & y[2] == 3){
    z <- as.character("3_4")
  }
  return(z)
}

book_long <- numeric(13) # 17
for (i in 1:13) { # 17
  book_long[i] <- paste0(books[i],".","long")
}

for (w in 1:25) {
  main_df <- data.frame()
  rds <- readRDS(R1.2_R2.2[w])
  rds_gamma <- rds$gamma
  for (i in 1:17) { # 17 for additional books
    gamma_book <- round(rds_gamma[i,1:N[i],],2)
    gamma_prob <- gamma_book > 0.7
    gamma_order <- t(apply(gamma_book, MARGIN = 1, FUN = order))
    results_vec <- factor(apply(gamma_book, MARGIN = 1, FUN = amb_func))
    book_vec <- rep(books[i], N[i])
    sent_vec <- c(1:N[i])
    main_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, ID = results_vec)
    main_df <- rbind(main_df, main_df.sub)
  }
  main_df$ID <- factor(main_df$ID, levels = c("1", "1_2", "2",
                                              "1_3", "1_4", "2_3", "2_4", 
                                              "3", "3_4", "4"))
  main_df$Book <- factor(main_df$Book, levels = c("John.3", "John.2", "John.1", "Heb",
    "Tim.2","Tim.1","Tit","Eph","Col",
    "Thes.2","Phm","Php","Rom","Cor.2",
    "Cor.1", "Thes.1", "Gal"))
  assign(paste0("Seed_", file.num[w]), main_df)
}

for (i in 1:13) { # 17 for additional books
  book_df <- data.frame()
  for (j in 1:25) {
    rds <- readRDS(R1.2_R2.2[j])
    rds_gamma <- rds$gamma
    gamma_book <- round(rds_gamma[i,1:N[i],],2)
    gamma_prob <- gamma_book > 0.7
    gamma_order <- t(apply(gamma_book, MARGIN = 1, FUN = order))
    results_vec <- factor(apply(gamma_book, MARGIN = 1, FUN = amb_func))
    book_vec <- rep(books[i], N[i])
    sent_vec <- c(1:N[i])
    book_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, Regime = results_vec)
    book_df <- rbind(book_df, book_df.sub)
  }
  assign(book_long[i], book_df)
}


#### ID assignments: absolute ####
for (i in 1:25) {
  rds <- readRDS(R1.2_R2.2[i])
  ids <- rds$id
  main_df <- data.frame()
  for (j in 1:17) { # 17
    sent_vec <- c(1:N[j])
    id_vec <- ids[j,1:N[j]]
    book_vec <- rep(books[j],N[j])
    main_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, ID = id_vec)
    main_df <- rbind(main_df, main_df.sub)
  }
  main_df$ID <- factor(main_df$ID, levels = c("1", "1_2", "2",
                                              "1_3", "1_4", "2_3", "2_4", 
                                              "3", "3_4", "4"))
  main_df$Book <- factor(main_df$Book, levels = c("John.3", "John.2", "John.1", "Heb",
                                                  "Tim.2","Tim.1","Tit","Eph","Col",
                                                  "Thes.2","Phm","Php","Rom","Cor.2",
                                                  "Cor.1", "Thes.1", "Gal"))
  assign(paste0("Seed_", file.num[i]), main_df)
}

################## Sequence plots ################## 
#### R1.2_R2.1
R1.2_R2.2[order(BIC_r1.2_r2.2)] # Find order of BIC
ggplot(Seed_731, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[25])) +
  geom_tile(width = 1, height = 1) + 
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

ggplot(Seed_1697, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[14])) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

ggplot(Seed_1215, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[3])) +
  geom_tile(width = 1, height = 1) + 
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))


#### R1.2_R2.2
R1.2_R2.2[order(BIC_r1.2_r2.2)] # Find order of BIC
ggplot(Seed_1660, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[11])) +
  geom_tile(width = 1, height = 1) + 
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

ggplot(Seed_1741, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[17])) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

ggplot(Seed_1605, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[9])) +
  geom_tile(width = 1, height = 1) + 
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))


#### R1.2_R2.2 HJ
R1.2_R2.2[order(BIC_r1.2_r2.2)]
order(BIC_r1.2_r2.2)# Find order of BIC
ggplot(Seed_1741, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus + Hebrews and 1, 2, 3 John",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[17])) +
  geom_tile(width = 1, height = 1) + 
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

ggplot(Seed_1534, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[6])) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

#### R1.2_R2.1 HJ
R1.2_R2.2[order(BIC_r1.2_r2.2)]
order(BIC_r1.2_r2.2)# Find order of BIC
BIC_r1.2_r2.2[order(BIC_r1.2_r2.2)]
ggplot(Seed_1707, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus + Hebrews and 1, 2, 3 John",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[16])) +
  geom_tile(width = 1, height = 1) + 
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

ggplot(Seed_1611, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus + Hebrews and 1, 2, 3 John",
          subtitle = paste0("BIC = ", BIC_r1.2_r2.2[8])) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red"))

### Cumulative classification sequence plots ###
library(tidyr)
library(dplyr)
# 1 Timothy
Tim.1_long <- Tim.1.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Tim.1_long$Regime <- factor(Tim.1_long$Regime, levels = c("1", "1_2", "2",
                                                          "1_3", "1_4", "2_3", "2_4", 
                                                          "3", "3_4", "4"))
names(Tim.1_long)[3] <- "Total"

ggplot(Tim.1_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red")) +
  labs(title = "1 Timothy (13 Book Pauline Corpus)")

# 2 Timothy
Tim.2_long <- Tim.2.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Tim.2_long$Regime <- factor(Tim.2_long$Regime, levels = c("1", "1_2", "2",
                                                          "1_3", "1_4", "2_3", "2_4", 
                                                          "3", "3_4", "4"))
names(Tim.2_long)[3] <- "Total"

ggplot(Tim.2_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red")) +
  labs(title = "2 Timothy (13 Book Pauline Corpus)")

# Titus
Tit_long <- Tit.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Tit_long$Regime <- factor(Tit_long$Regime, levels = c("1", "1_2", "2",
                                                          "1_3", "1_4", "2_3", "2_4", 
                                                          "3", "3_4", "4"))
names(Tit_long)[3] <- "Total"

ggplot(Tit_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red")) +
  labs(title = "Titus (13 Book Pauline Corpus)")


# Ephesians
Eph_long <- Eph.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Eph_long$Regime <- factor(Eph_long$Regime, levels = c("1", "1_2", "2",
                                                      "1_3", "1_4", "2_3", "2_4", 
                                                      "3", "3_4", "4"))
names(Eph_long)[3] <- "Total"

ggplot(Eph_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red")) +
  labs(title = "Ephesians (13 Book Pauline Corpus)")

# Colossians
Col_long <- Col.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Col_long$Regime <- factor(Col_long$Regime, levels = c("1", "1_2", "2",
                                                      "1_3", "1_4", "2_3", "2_4", 
                                                      "3", "3_4", "4"))
names(Col_long)[3] <- "Total"

ggplot(Col_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red")) +
  labs(title = "Colossians (13 Book Pauline Corpus)")

# 2 Thessalonians
Thes.2_long <- Thes.2.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Thes.2_long$Regime <- factor(Thes.2_long$Regime, levels = c("1", "1_2", "2",
                                                      "1_3", "1_4", "2_3", "2_4", 
                                                      "3", "3_4", "4"))
names(Thes.2_long)[3] <- "Total"

ggplot(Thes.2_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "blue", "2" = "lightblue", "1_2" = "dodgerblue",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red")) +
  labs(title = "2 Thessalonians (13 Book Pauline Corpus)")
