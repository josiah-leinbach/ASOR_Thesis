library(ggplot2)

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/R1.2_R2.1")
rds_2.1 <- readRDS("EMR-R1.2-R2.1_731.rds") # 1611 second-lowest
id <- rds_2.1$id

setwd("C:/Users/jleinba/Documents/ASOR/ASOR_Thesis/Results/HJ123_R1.2_R2.1")
rds.HJ_2.1 <- readRDS("EMR.HJ123-R1.2-R2.1-1348.rds") # 1707 second-lowest
id.HJ <- rds.HJ_2.1$id

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
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus", subtitle = "R1 = 2, R2 = 1. BIC = 118,855.6") +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", "3" = "orange"))

### Hebrews + Johannine ###
N.2 <- numeric(17)
for (i in 1:17) {
  N.2[i] <- sum(!is.na(id.HJ[i,]))
}

books_HJ <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", 
              "Col", "Thes.2", "Tim.1", "Tim.2", "Tit", "Heb", "John.1", "John.2", "John.3")
main_df.2 <- data.frame()

for (i in 1:17) {
  book_vec <- rep(books_HJ[i], N.2[i])
  sent_vec <- c(1:N.2[i])
  ID_vec <- id.HJ[i,1:N.2[i]]
  book_df <- data.frame(Book = book_vec, Sentence = sent_vec, ID = ID_vec)
  main_df.2 <- rbind(main_df.2, book_df)
}

# Sequence plot 
ggplot(main_df.2, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus + Hebrews and Johnannine Letters", subtitle = "R1 = 2, R2 = 1") +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", "3" = "orange", "4" ="gold4"))


### Cumulative classification sequence plots ###
library(tidyr)
# 1 Timothy
Tim.1_long <- sent_array.2[[11]][,-1] %>%
  gather(key = "Regime", value = "Prop", -Sentence)

Tim.1_long$Regime <- factor(Tim.1_long$Regime, levels = c("Amb", "R3", "R2", "R1"))

ggplot(Tim.1_long, aes(x = Sentence, y = Prop, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = "1 Timothy (13 Book Pauline Corpus)")

# Ephesians
Eph_long <- sent_array.2[[8]][,-1] %>%
  gather(key = "Regime", value = "Prop", -Sentence)

Eph_long$Regime <- factor(Eph_long$Regime, levels = c("Amb", "R3", "R2", "R1"))

ggplot(Eph_long, aes(x = Sentence, y = Prop, fill = Regime)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Ephesians (13 Book Pauline Corpus)")
