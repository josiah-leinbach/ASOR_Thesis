library(ggplot2)

rds_2.1 <- readRDS("EMR-R1.2-R2.1-9pos.rds")
id <- rds_2.1$id

rds.HJ1_2.2 <- readRDS("EMR.HJ-R1.2-R2.2-9pos.rds")
id.HJ1 <- rds.HJ1_2.2$id

### Baseline for 13 book Pauline Corpus ###
N <- numeric(13)
for (i in 1:13) {
  N[i] <- sum(!is.na(id[i,]))
}
books <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", "Col", "Thes.2", "Tim.1", "Tim.2", "Tit")
main_df <- data.frame()

for (i in 1:13) {
  book_vec <- rep(books[i], N[i])
  sent_vec <- c(1:N[i])
  ID_vec <- id[i,1:N[i]]
  book_df <- data.frame(Book = book_vec, Sentence = sent_vec, ID = ID_vec)
  main_df <- rbind(main_df, book_df)
}

# Sequence plot 
ggplot(main_df, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus", subtitle = "R1 = 2, R2 = 1") +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", "3" = "orange"))

### Hebrews + 1 John ###
N.2 <- numeric(15)
for (i in 1:15) {
  N.2[i] <- sum(!is.na(id.HJ1[i,]))
}

books_HJ <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", 
              "Col", "Thes.2", "Tim.1", "Tim.2", "Tit", "Heb", "John.1")
main_df.2 <- data.frame()

for (i in 1:15) {
  book_vec <- rep(books_HJ[i], N.2[i])
  sent_vec <- c(1:N.2[i])
  ID_vec <- id.HJ1[i,1:N.2[i]]
  book_df <- data.frame(Book = book_vec, Sentence = sent_vec, ID = ID_vec)
  main_df.2 <- rbind(main_df.2, book_df)
}

# Sequence plot 
ggplot(main_df.2, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus + Hebrews and 1 John", subtitle = "R1 = 2, R2 = 2") +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", "3" = "orange", "4" ="gold4"))


