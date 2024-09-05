library(ggplot2)

R1.2_R2.1 <- list.files("C:/Users/jleinba/Documents/ASOR_Thesis/Results/6-POS/R1.2-R2.1/EPS_10e-6","EMR")
N <- c(331, 377, 174, 110, 55, 50, 10, 55, 39, 25, 61, 46, 27)
books <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", "Col", "Thes.2", "Tim.1", "Tim.2", "Tit")

prop_amb <- numeric(25)
BIC_r1.2_r2.1 <- round(bic_vec, 2)

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

book_long <- numeric(13)
for (i in 1:13) {
  book_long[i] <- paste0(books[i],".","long")
}

for (w in 1:25) {
  main_df <- data.frame()
  rds <- readRDS("EMR-R1.2-R2.2_NEW_731.rds")
  rds_gamma <- rds$gamma
  for (i in 1:13) {
    gamma_book <- round(rds_gamma[i,1:N[i],],2)
    gamma_prob <- gamma_book > 0.7
    gamma_order <- t(apply(gamma_book, MARGIN = 1, FUN = order))
    results_vec <- factor(apply(gamma_book, MARGIN = 1, FUN = amb_func))
    book_vec <- rep(books[i], N[i])
    sent_vec <- c(1:N[i])
    main_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, ID = results_vec)
    main_df <- rbind(main_df, main_df.sub)
  }
  assign("Seed_731", main_df)
  prop_amb[w] <- length(which(main_df$ID == "1_2" |
                                main_df$ID == "2_3" |
                                main_df$ID == "1_3")) / sum(N)
}

for (i in 1:13) {
  book_df <- data.frame()
  for (j in 1:25) {
    rds <- readRDS(R1.2_R2.1[j])
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

bic_df <- data.frame(BIC = BIC_r1.2_r2.1, Prop_Amb = prop_amb)
plot(bic_df$BIC, bic_df$Prop_Amb, xlab =  "BIC", ylab = "Prop_Amb", 
     main = "Proportion of Ambiguous Sentences by BIC")

summary(lm(Prop_Amb ~ BIC, data = bic_df[-c(11,12,20)])) # Removing high leverage points

# Sequence plots
R1.2_R2.1[order(BIC_r1.2_r2.1)] # Find order of BIC
ggplot(Seed_43, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus") + 
  geom_tile(width = 1, height = 1)

ggplot(Seed_597, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus") +
  geom_tile(width = 1, height = 1)

ggplot(Seed_731, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus") +
  geom_tile(width = 1, height = 1)


### Cumulative classification sequence plots ###
library(tidyr)
# 1 Timothy
Tim.1_long <- Tim.1.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Tim.1_long$Regime <- factor(Tim.1_long$Regime, levels = c("1_3", "2_3", "3", "1_2", "2", "1"))
names(Tim.1_long)[3] <- "Total"

ggplot(Tim.1_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", 
                               "3" = "orange", "1_2" = "forestgreen",
                               "1_3" = "yellow", "2_3" = "violet")) +
  labs(title = "1 Timothy (13 Book Pauline Corpus)")

# Ephesians
Eph_long <- Eph.long %>%
  count(Sentence, Regime) %>%
  complete(Sentence, Regime, fill = list(n = 0))

Eph_long$Regime <- factor(Eph_long$Regime, levels = c("1_3", "2_3", "3", "1_2", "2", "1"))
names(Eph_long)[3] <- "Total"

ggplot(Eph_long, aes(x = Sentence, y = Total, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", 
                               "3" = "orange", "1_2" = "forestgreen",
                               "1_3" = "yellow", "2_3" = "violet")) +
  labs(title = "Ephesians (13 Book Pauline Corpus)")
