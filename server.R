## Data Science Capstone : Word Prediction Project

suppressPackageStartupMessages({
        library(tidyverse)
        library(tm)
        library(stringr)
        library(dplyr)
        library(shiny)
}
)
suppressWarnings(library(shiny))

# Load all data from ngram data folder
bi_w <- readRDS("biwordtop20.rds")
tri_w  <- readRDS("triwordtop20.rds")
quad_w <- readRDS("quadwordtop20.rds")
quint_w <- readRDS("quintwordtop10.rds")
sext_w <- readRDS("sextwordtop10.rds")
hex_w <- readRDS("hexwordtop10.rds")

# Create ngram Matching Functions
bi_gram <- function(input_no_words){
        num <- length(input_no_words)
        dplyr::filter(bi_w,
               word1==input_no_words[num]) %>%
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 2)) %>%
                as.character() -> outtext
        ifelse(outtext =="character(0)", "?", return(outtext))
}

tri_gram <- function(input_no_words){
        num <- length(input_no_words)
        dplyr::filter(tri_w,
               word1==input_no_words[num-1],
               word2==input_no_words[num])  %>%
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 3)) %>%
                as.character() -> outtext
        ifelse(outtext=="character(0)", bi_gram(input_no_words), return(outtext))
}

quad_gram <- function(input_no_words){
        num <- length(input_no_words)
        dplyr::filter(quad_w,
               word1==input_no_words[num-2],
               word2==input_no_words[num-1],
               word3==input_no_words[num])  %>%
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 4)) %>%
                as.character() -> outtext
        ifelse(outtext=="character(0)", tri_gram(input_no_words), return(outtext))
}

quint_gram <- function(input_no_words){
        num <- length(input_no_words)
        dplyr::filter(quint_w,
               word1==input_no_words[num-3],
               word2==input_no_words[num-2],
               word3==input_no_words[num-1],
               word4==input_no_words[num])  %>%
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 5)) %>%
                as.character() -> outtext
        ifelse(outtext=="character(0)", quad_gram(input_no_words), return(outtext))
}

sext_gram <- function(input_no_words){
        num <- length(input_no_words)
        dplyr::filter(sext_w,
               word1==input_no_words[num-4],
               word2==input_no_words[num-3],
               word3==input_no_words[num-2],
               word4==input_no_words[num-1],
               word5==input_no_words[num])  %>%
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 6)) %>%
                as.character() -> outtext            
        ifelse(outtext=="character(0)", quint_gram(input_no_words), return(outtext))
}

sept_gram <- function(input_no_words){
        num <- length(input_no_words)
        dplyr::filter(hex_w,
               word1==input_no_words[num-5],
               word2==input_no_words[num-4],
               word3==input_no_words[num-3],
               word4==input_no_words[num-2],
               word5==input_no_words[num-1],
               word6==input_no_words[num])  %>%
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 7)) %>%
                as.character() -> outtext           
        ifelse(outtext=="character(0)", sext_gram(input_no_words), return(outtext))
}
# input function and match_word function
ngrams <- function (input_text) {
        # Create a dataframe and clean input text
        input_text <- data_frame(text = input_text)
        rep_nor <- "[^[:alpha:][:space:]]*"
        input_text <- input_text %>%
                mutate(text= str_replace_all(text, rep_nor, ""))
    
        # Find word count, separate words, lower case
       input_count <- str_count(input_text, boundary("word")) 
       input_no_words <- unlist(str_split(input_text, boundary("word")))
       input_no_words <- tolower(input_no_words)

        # Matching functions with the output
        outtext <- ifelse(input_count == 0, "Please enter a word or words in the given textbox.",
                          ifelse (input_count == 1, bi_gram(input_no_words),
                                   ifelse (input_count == 2, tri_gram(input_no_words),
                                           ifelse (input_count == 3, quad_gram(input_no_words),
                                                   ifelse (input_count == 4, quad_gram(input_no_words),
                                                        ifelse (input_count == 5, quint_gram(input_no_words), 
                                                                ifelse (input_count == 6, sept_gram(input_no_words), sept_gram(input_no_words))))))))
        if(outtext == "?"){
               outtext = "The next expected word is not applicable due to limited size of the training data or miss-spelling" 
       }
        return(outtext)
}

shinyServer(function(input, output) {
        output$outtext <- renderPrint({
                outtext <-ngrams (input$inputString)
                outtext
                })
                
        output$text1 <- renderText({
                input$inputString
                });
        
}
)

