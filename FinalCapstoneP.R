## Coursera: Data Science Capstone_W7Final Project
### server.R file

# Load the required packages
# Ignore the Startup Messages while loading the packages
suppressPackageStartupMessages({
        library(tidytext)
        library(tidyverse)
        library(stringr)
        library(knitr)
        library(wordcloud)
        library(ngram)
        library(plyr)
        library(dplyr)
        
})

# Download and unzip the Data
#if(!file.exists("Coursera-SwiftKey.zip")){
       # download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip")
       # unzip("Coursera-SwiftKey.zip")
#}

#setting working directory
setwd("~/Desktop/DatacapStone/final/en_US")

# Load datasets
#blog <- "en_US.blogs.txt"
#new <- "./final/en_US/en_US.news.txt"
#twitter <- "./final/en_US/en_US.twitter.txt"

# Read the all data in en_US folder
blogsfile <- readLines("en_US.blogs.txt", skipNul = TRUE, warn = TRUE, encoding="UTF-8")
newsfile <- readLines("en_US.news.txt", skipNul = TRUE, warn = TRUE, encoding="UTF-8")
twitterfile <- readLines("en_US.twitter.txt", skipNul = TRUE, warn = TRUE, encoding="UTF-8")

# Create Dataframes for the Data
blogsfile <- data_frame(text = blogsfile)
newsfile <- data_frame(text = newsfile)
twitterfile <- data_frame(text = twitterfile)

# Reduce the real dataset --> sample dataset=5% real one
set.seed(1008)
pop <- 0.05

sampblogs <- blogsfile %>% sample_n(., nrow(blogsfile)*pop)
sampnews <- newsfile %>% sample_n(., nrow(newsfile)*pop)
samptwitter <- twitterfile %>% sample_n(., nrow(twitterfile)*pop)

#Combine 3 data into one sample data set
sampset <- bind_rows(
        mutate(sampblogs, source = "blogsfile"),
        mutate(sampnews,  source = "newsfile"),
        mutate(samptwitter, source = "twitterfile"))

sampset$source <- as.factor(sampset$source)
rm(list = c("blogsfile","newsfile","twitterfile",
            "sampblogs", "sampews", "samptwitter", "pop"))

# Cleanning the sample dataset
data("stop_words")

# remove profanity/bad words with the source comming from http://www.bannedwordlist.com/
badwords <- read_delim("swearWords.csv", delim = "\n", col_names = FALSE)
badwords<- unnest_tokens(badwords, word, X1)

rep_nor <- "[^[:alpha:][:space:]]*"
rep_link <- "http[^[:space:]]*"
rep_non <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"

# final sample dataset
final_dt <-  sampset %>%
        mutate(text = str_replace_all(text, rep_nor, "")) %>%
        mutate(text = str_replace_all(text, rep_link, "")) %>%
        mutate(text = str_replace_all(text, rep_non, "")) %>%
        mutate(text = iconv(text, "ASCII//TRANSLIT"))


# Generate n-gram model dataset upto 7-words length
# uni/mono-gram
uni_dt <- final_dt %>%
        unnest_tokens(word, text, token = "ngrams", n=1) %>%
        anti_join(badwords) %>%
        anti_join(stop_words)

# Bi-gram

bi_dt <- final_dt %>%
        unnest_tokens(bi_gram, text, token = "ngrams", n = 2)

# Tri-gram
tri_dt <- final_dt %>%
        unnest_tokens(tri_gram, text, token = "ngrams", n = 3)

# Quad/tetra-gram
quad_dt <- final_dt %>%
        unnest_tokens(quad_gram, text, token = "ngrams", n = 4)

# Quint/pent-gram
quint_dt <- final_dt %>%
        unnest_tokens(quint_gram, text, token = "ngrams", n = 5)

# Sext/hex-grams
sext_dt<- final_dt %>%
        unnest_tokens(sext_gram, text, token = "ngrams", n = 6)

# Sept/hept-grams
hex_dt<- final_dt %>%
        unnest_tokens(sept_gram, text, token = "ngrams", n = 7)

# Reduce ngrams files down to top 10 -20 due to large processed data files
# 1
uni_20 <- uni_dt %>%
        dplyr::count(word, sort= TRUE) %>%
        filter(n > 20) %>%
        arrange(desc(n))
        
#2       
bi_20 <- bi_dt %>%
        dplyr::count(bi_gram, sort= TRUE) %>%
        filter(n > 20) %>%
        arrange(desc(n))
# 3
tri_20 <- tri_dt %>%
        dplyr::count(tri_gram, sort=TRUE) %>%
        filter(n > 20) %>%
        arrange(desc(n))
# 4
quad_20 <- quad_dt %>%
        dplyr::count(quad_gram, sort=TRUE) %>%
        filter(n > 20) %>%
        arrange(desc(n))
# 5
quint_10 <- quint_dt %>%
        dplyr::count(quint_gram, sort=TRUE) %>%
        filter(n > 10) %>%
        arrange(desc(n))
# 6
sext_10 <- sext_dt %>%
        dplyr::count(sext_gram, sort=TRUE) %>%
        filter(n > 10) %>%
        arrange(desc(n))

#7
hex_10 <- hex_dt %>%
        dplyr::count(sept_gram, sort=TRUE) %>%
        filter(n>10) %>%
        arrange(desc(n))

rm(list = c("bi_dt","tri_dt", "quad_dt", "quint_dt", "sext_dt", "hex_dt"))

# Separate words od n-gram model
uni_w <-uni_20 %>%
        separate(word, c("word1"), sep = " ")

bi_w <- bi_20 %>%
        separate(bi_gram, c("word1", "word2"), sep = " ")

tri_w <- tri_20 %>%
        separate(tri_gram, c("word1", "word2", "word3"), sep = " ")

quad_w <- quad_20 %>%
        separate(quad_gram, c("word1", "word2", "word3", "word4"), sep = " ")

quint_w <- quint_10 %>%
        separate(quint_gram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")

sext_w <- sext_10 %>%
        separate(sext_gram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")

hex_w <- hex_10 %>%
        separate(sept_gram, c("word1", "word2", "word3", "word4", "word5", "word6", "word7"), sep = " ")


# Save the data set for Shiny App
dir.create("n_gram_data", showWarnings = FALSE)

saveRDS(uni_w, "./n_gram_data/uniwordtop20.rds")
saveRDS(bi_w, "./n_gram_data/biwordtop20.rds")
saveRDS(tri_w, "./n_gram_data/triwordtop20.rds")
saveRDS(quad_w,"./n_gram_data/quadwordtop20.rds")
saveRDS(quint_w,"./n_gram_data/quintwordtop10.rds")
saveRDS(sext_w,"./n_gram_data/sextwordtop10.rds")
saveRDS(hex_w,"./n_gram_data/hexwordtop10.rds")

# Some n-gram pic for Shiny app

tri_20 %>%
        top_n(20, n) %>%
        mutate(tri_gram = reorder(tri_gram, n)) %>%
        ggplot(aes(tri_gram, n)) +
        theme_bw() +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        labs(y="Frequency", title="Top 20 common trigrams in text sample")
ggsave("./n_gram_data/tripic.png")  
quad_20 %>%
        top_n(10, n) %>%
        mutate(quad_gram = reorder(quad_gram, n)) %>%
        ggplot(aes(quad_gram, n)) +
        theme_bw() +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        labs(y="Frequency", title="Top 10 common quadgrams in text sample")
ggsave("./n_gram_data/quadpic.png")
bi_20 %>%
        top_n(20, n) %>%
        mutate(bi_gram = reorder(bi_gram, n)) %>%
        ggplot(aes(bi_gram, n)) +
        geom_col() +
        coord_flip() +
        theme(axis.title.y = element_blank()) +
        labs(y="Frequency", title="Top 20 common bigrams in text sample")
ggsave("./n_gram_data/bipic.png")                

