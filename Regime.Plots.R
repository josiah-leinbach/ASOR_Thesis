library(ggplot2)

R1.2_R2.1 <- list.files("/Users/josiahleinbach/Downloads/R1.2_R2.1","EMR")
N <- c(331, 377, 174, 110, 55, 50, 10, 55, 39, 25, 61, 46, 27)
books <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", "Col", "Thes.2", "Tim.1", "Tim.2", "Tit")

prop_amb <- numeric(25)
BIC_r1.2_r2.1 <- round(BIC_r1.2_r2.1, 2)

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
  } else if (y[1] == 2 & y[2] == 3) {
    z <- as.character("2_3") # 2 and 3
  } else if (y[1] == 3 & y[2] == 2) {
    z <- as.character("2_3")
  } else if (y[1] == 1 & y[2] == 3) {
    z <- as.character("1_3") # 1 and 3
  } else {
    z <- as.character("1_3")
  }
  return(z)
}

for (w in 1:25) {
  main_df <- data.frame()
  rds <- readRDS(R1.2_R2.1[w])
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
  assign(paste0("Seed_",file.num[w]), main_df)
  prop_amb[w] <- length(which(main_df$ID == "1_2" |
                                main_df$ID == "2_3" |
                                main_df$ID == "1_3")) / sum(N)
}

bic_df <- data.frame(BIC = BIC_r1.2_r2.1, Prop_Amb = prop_amb)
plot(bic_df$BIC, bic_df$Prop_Amb, xlab =  "BIC", ylab = "Prop_Amb", 
     main = "Proportion of Ambiguous Sentences by BIC")

summary(lm(Prop_Amb ~ BIC, data = bic_df[-c(11,12,20)])) # Removing high leverage points

# Sequence plots
R1.2_R2.1[order(BIC_r1.2_r2.1)] # Find order of BIC
ggplot(Seed_731, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus", 
          subtitle = paste0("Seed_", file.num[25], "(", "BIC = ", BIC_r1.2_r2.1[25], ")")) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", 
                               "3" = "orange", "1_2" = "forestgreen",
                               "1_3" = "yellow", "2_3" = "violet"))

ggplot(Seed_1688, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus", 
          subtitle = paste0("Seed_", file.num[13], "(", "BIC = ", BIC_r1.2_r2.1[13], ")")) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", 
                               "3" = "orange", "1_2" = "forestgreen",
                               "1_3" = "yellow", "2_3" = "violet"))

ggplot(Seed_1662, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Regime Distributions for 13 Book Claimed Pauline Corpus", 
          subtitle = paste0("Seed_", file.num[12], "(", "BIC = ", BIC_r1.2_r2.1[12], ")")) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "blue", 
                               "3" = "orange", "1_2" = "forestgreen",
                               "1_3" = "yellow", "2_3" = "violet"))


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
