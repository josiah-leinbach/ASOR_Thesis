library(ggplot2)
library(tidyr)
library(dplyr)
library(paletteer)

# Create color palettes
paul_col <- paletteer_d("fishualize::Alosa_fallax")
non_paul_col <- paletteer_d("NatParksPalettes::Cuyahoga")

# Import files
file.names <- list.files("/Users/josiahleinbach/Downloads/HHJ-R1.4.R2.2","EMR")
setwd("/Users/josiahleinbach/Downloads/HHJ-R1.4.R2.2")
N <- c(331, 377, 174, 110, 55, 50, 10, 55, 39, 25, 61, 46, 27,164, 96)
books <- c("Rom", "Cor.1", "Cor.2", "Gal", "Php", "Thes.1", "Phm", "Eph", "Col", "Thes.2", "Tim.1", "Tim.2", "Tit",
          "Heb", "John.1")

# Record BICs for random starts
BIC_file.names <- numeric()
for (i in 1:25) {
  rds <- readRDS(file.names[i])
  BIC_file.names[i] <- round(rds$BIC,2)
}
BIC_file.names[order(BIC_file.names)] # Order of BICs

file.num <- c(1066,1170,1215,1348,1415,1534,1556,1588,1605,1611,1660,1662,1688,
              1697,1704,1707,1741,1801,1805,1815,1945,2022,43,597,731)

# Function to determine if sentence classification is ambiguous
amb_func <- function(x) {
  z <- numeric()
  y <- order(x, decreasing = T)
  if (sum(x > 0.7) == 1) { # Can use other value than 0.7
    z <- as.character(y[1])
  } else {
    z <- as.character("A")
  }
  return(z)
}


# Aggregate classification plots
book_long <- numeric(15) # 13
for (i in 1:15) { # 13
  book_long[i] <- paste0(books[i],".","long")
}

for (i in 1:15) { # 13
  book_df <- data.frame()
  for (j in 1:25) {
    rds <- readRDS(file.names[j])
    rds_gamma <- rds$gamma
    gamma_book <- round(rds_gamma[i,1:N[i],],2)
    gamma_prob <- gamma_book > 0.7
    gamma_order <- t(apply(gamma_book, MARGIN = 1, FUN = order))
    results_vec <- factor(apply(gamma_book, MARGIN = 1, FUN = amb_func))
    book_vec <- rep(books[i], N[i])
    sent_vec <- c(1:N[i])
    book_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, Style = results_vec)
    book_df <- rbind(book_df, book_df.sub)
  }
  assign(book_long[i], book_df)
}

#### ID assignments: Ambiguous ####

for (w in 1:25) {
  main_df <- data.frame()
  rds <- readRDS(file.names[w])
  rds_gamma <- rds$gamma
  for (i in 1:15) { # 13
    gamma_book <- round(rds_gamma[i,1:N[i],],2)
    gamma_prob <- gamma_book > 0.7
    gamma_order <- t(apply(gamma_book, MARGIN = 1, FUN = order))
    results_vec <- factor(apply(gamma_book, MARGIN = 1, FUN = amb_func))
    book_vec <- rep(books[i], N[i])
    sent_vec <- c(1:N[i])
    main_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, ID = results_vec)
    main_df <- rbind(main_df, main_df.sub)
  }
  main_df$ID <- factor(main_df$ID, levels = c("1","2","3","4","5", "6","A")) # Set main Pauline style as PS_1
  main_df$Book <- factor(main_df$Book, levels = c(#"John.3", "John.2", 
                                                  "John.1", "Heb",
    "Tim.2","Tim.1","Tit","Eph","Col",
    "Thes.2","Phm","Php","Rom","Cor.2",
    "Cor.1", "Thes.1", "Gal"))
  assign(paste0("Seed_", file.num[w], ".amb"), main_df)
}

#### ID assignments: Absolute ####
for (i in 1:25) {
  rds <- readRDS(file.names[i])
  ids <- rds$id
  main_df <- data.frame()
  for (j in 1:15) { # 17
    sent_vec <- c(1:N[j])
    id_vec <- ids[j,1:N[j]]
    book_vec <- rep(books[j],N[j])
    main_df.sub <- data.frame(Book = book_vec, Sentence = sent_vec, ID = id_vec)
    main_df <- rbind(main_df, main_df.sub)
  }
  main_df$ID <- factor(main_df$ID) # Set main Pauline style as PS_1
  main_df$Book <- factor(main_df$Book, levels = c(#"John.3", "John.2", 
                                                  "John.1", "Heb",
                                                  "Tim.2","Tim.1","Tit","Eph","Col",
                                                  "Thes.2","Phm","Php","Rom","Cor.2",
                                                  "Cor.1", "Thes.1", "Gal"))
  assign(paste0("Seed_", file.num[i], ".abs"), main_df)
}


#### Proportion ambiguous ####
N.1 <- data.frame(Sent = c(110,50,377,174,331,50,10,25,39,55,27,61,46,164,96),
                  Book = c("John.1","Heb","Tim.2","Tim.1","Tit","Eph","Col",
                    "Thes.2","Phm","Php","Rom","Cor.2","Cor.1","Thes.1","Gal"))
                  

Seed_1588.amb %>% # Change seed input. Must have .amb
  group_by(Book) %>%
  summarise(Tot_sent = length(ID),
            Perc_R1 = (sum(ID == "1") / Tot_sent),
            Perc_R2 = (sum(ID == "2") / Tot_sent),
            Perc_R3 = (sum(ID == "3") / Tot_sent),
            Perc_R4 = (sum(ID == "4") / Tot_sent),
            Perc_R5 = (sum(ID == "5") / Tot_sent),
            Perc_R6 = (sum(ID == "6") / Tot_sent),
            Perc_amb = (sum(ID == "A") / Tot_sent))

################## Sequence plots ################## 
file.names[order(BIC_file.names)] # Solutions by order of BIC

# Use either absolute (.abs) or ambiguous (.amb) in dataframe
ggplot(Seed_1588.amb, aes(x = Sentence, y = Book, fill = factor(ID))) +
  ggtitle("Style Distributions for 13 Book Claimed Pauline Corpus + Hebrews and 1 John",
          subtitle = paste0("BIC = ", BIC_file.names[order(BIC_file.names)][1])) +
  geom_tile(width = 1, height = 1) + 
  labs(color = "Style") +
  scale_fill_manual("Style", labels = c("PS_1","PS_2","PS_3","PS_4","NPS_1","NPS_2","Amb"), # Assumes reordered factor levels
                    values = c("2" = paul_col[3], "1" = paul_col[2], "3" = paul_col[1], "4" = paul_col[4],
                               "5" = non_paul_col[1], "6" = non_paul_col[2], "A" = "black")) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.position = "none")


### Aggregate classification sequence plot (if needed) ###
# 1 Timothy
Tim.1_long <- Tim.1.long %>%
  count(Sentence, Style) %>%
  complete(Sentence, Style, fill = list(n = 0))

Tim.1_long$Style <- factor(Tim.1_long$Style)

names(Tim.1_long)[3] <- "Total"

ggplot(Tim.1_long, aes(x = Sentence, y = Total, fill = Style)) +
  geom_col(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("1" = "blue", "2" = "orange", "1_2" = "violet",
                               "1_3" = "violet", "1_4" = "maroon2", "2_3" = "pink",
                               "2_4" = "purple", "3" = "orange", "3_4" = "forestgreen", "4" = "red")) +
  labs(title = "1 Timothy (13 Book Pauline Corpus)")
