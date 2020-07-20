---
title: "Capstone Project"
author: "TEU"
date: "20 7 2020"
output: 
  html_document: 
    keep_md: yes
---



## Synopsis
This is my attempt on the capstone project of the coursera data science specialization. The goal of this project is to develop a text prediction algorithm that is implemented in a shiny app and allows for predicting the next word a user is going to write based on the current input.

## Libraries
According to the 'CRAN Task View for Natural Language Processing' there are a couple of packages available for text mining, each og which has different advantages and disadvanteges. I decided to work with the **quanteda** package due to its rich set of functions and fast performance.


```r
setwd("D:/R/Coursera Schulung/Kurs 10")

library(dplyr)
library(lubridate)
library(quanteda)
library(readtext)
library(ggplot2)
```

## Read Data

```r
# Reading blogs texts.
if(!exists("blogs.text")) {
  con <- file("data/en_US/en_US.twitter.txt", "rb")
  blogs.text <- readLines(con, encoding="UTF-8", skipNul=TRUE)
  close(con)
}

if(!exists("news.text")) {
  con <- file("data/en_US/en_US.twitter.txt", "rb")
  news.text <- readLines(con, encoding="UTF-8", skipNul=TRUE)
  close(con)
}

if(!exists("twitter.text")) {
  con <- file("data/en_US/en_US.twitter.txt", "rb")
  twitter.text <- readLines(con, encoding="UTF-8", skipNul=TRUE)
  close(con)
}
```

Memory on my machine is significantly limited, so I chose to run the project on a random 5% sample of each txt file.


```r
if(!exists("blogs.subdata")) blogs.subdata <- blogs.text[as.logical(rbinom(length(blogs.text), 1, 0.05))]
if(!exists("news.subdata")) news.subdata <- news.text[as.logical(rbinom(length(news.text), 1, 0.05))]
if(!exists("twitter.subdata")) twitter.subdata <- twitter.text[as.logical(rbinom(length(twitter.text), 1, 0.05))]
```

## Create a corpus
Now I save the 5% files to a data corpus. The main features of the thus gained dataset are presented below

```r
if(!exists("df.corpus")) {
  dir.create("tmp")

  write(blogs.subdata, "tmp/en_US.blogs.txt")
  write(news.subdata, "tmp/en_US.news.txt")
  write(twitter.subdata, "tmp/en_US.twitter.txt")
  
  df.text <- readtext("tmp/*.txt")
  unlink("tmp", recursive = TRUE)
  
  df.corpus <- corpus(df.text)
  df.summary <- summary(df.corpus)
  names(df.summary)[3] <- "Words"
}
df.summary
```

```
## Corpus consisting of 3 documents, showing 3 documents:
## 
##               Text Types   Words Sentences
##    en_US.blogs.txt 93366 1856255    129920
##     en_US.news.txt 92731 1843088    130003
##  en_US.twitter.txt 92916 1844913    129216
```

## Tokenize the Text
I decided not to remove english stop words while tokenizing the text. Since I want to predict the next word of a user input, and this word is very likely to be a stop word, I think they must stay in the pool. In addition, I convert all words to lower case and remove separators like spaces, commas, ect.

```r
if(!exists("df.tokens")) {
  df.tokens <- tokens(df.corpus, remove_punct = TRUE, 
                      remove_numbers = TRUE,
                      remove_separators = TRUE,
                      remove_twitter = TRUE,
                      what = "word1")
  df.tokens <- tokens_tolower(df.tokens)
}
```

Let's see what we got

```r
df.tokens[c(1,2,3)]
```

```
## Tokens consisting of 3 documents.
## en_US.blogs.txt :
##  [1] "yo"     "chick"  "she"    "so"     "thirst" "aye"    "i"      "really"
##  [9] "don't"  "know"   "what"   "else"  
## [ ... and 1,509,990 more ]
## 
## en_US.news.txt :
##  [1] "watch"     "your"      "mailbox"   "tommorows" "the"       "day"      
##  [7] "good"      "questions" "rt"        "your"      "brand"     "will"     
## [ ... and 1,498,344 more ]
## 
## en_US.twitter.txt :
##  [1] "time"     "to"       "shape"    "up"       "water"    "pavement"
##  [7] "weights"  "and"      "zija"     "are"      "going"    "to"      
## [ ... and 1,498,601 more ]
```

## Create Document-Feature Matrix
A Document-feature matrix (DFM) allows to easily analyse basic features of documents. like the number of word occurances.

```r
if(!exists("df.dfm")) df.dfm <- dfm(df.tokens)
```

So let's see, Which are the 20 most frequent words in the dataset?

```r
df.topfeatures <- topfeatures(df.dfm, 20)
temp <- data.frame(Words = names(df.topfeatures), Frequency = unname(df.topfeatures))

g <- ggplot(data = temp, aes(Words, Frequency)) +
  geom_col() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency")
print(g)
```

![](Course-10-PA_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Generate Bigram

```r
if(!exists("df.bigram")) df.bigram <- tokens_ngrams(df.tokens, n = 2L)
if(!exists("df.bigram.dfm")) df.bigram.dfm <- dfm(df.bigram)
head(df.bigram[[1]], 30)
```

```
##  [1] "yo_chick"       "chick_she"      "she_so"         "so_thirst"     
##  [5] "thirst_aye"     "aye_i"          "i_really"       "really_don't"  
##  [9] "don't_know"     "know_what"      "what_else"      "else_they're"  
## [13] "they're_saying" "saying_tho"     "tho_except"     "except_that"   
## [17] "that_lol"       "lol_if"         "if_you"         "you_have"      
## [21] "have_firefox"   "firefox_or"     "or_google"      "google_chrome" 
## [25] "chrome_type"    "type_in"        "in_let"         "let_it"        
## [29] "it_snow"        "snow_into"
```

So again, let's see, Which are the 20 most frequent bigrams in the dataset?

```r
df.bigram.topfeatures <- topfeatures(df.bigram.dfm, 20)
temp <- data.frame(Words = names(df.bigram.topfeatures), Frequency = unname(df.bigram.topfeatures))

g <- ggplot(data = temp, aes(Words, Frequency)) +
  geom_col() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Bigram Frequency")
print(g)
```

![](Course-10-PA_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Generate Trigram

```r
if(!exists("df.trigram")) df.trigram <- tokens_ngrams(df.tokens, n = 3L)
if(!exists("df.trigram.dfm")) df.trigram.dfm <- dfm(df.trigram)
head(df.trigram[[1]], 30)
```

```
##  [1] "yo_chick_she"        "chick_she_so"        "she_so_thirst"      
##  [4] "so_thirst_aye"       "thirst_aye_i"        "aye_i_really"       
##  [7] "i_really_don't"      "really_don't_know"   "don't_know_what"    
## [10] "know_what_else"      "what_else_they're"   "else_they're_saying"
## [13] "they're_saying_tho"  "saying_tho_except"   "tho_except_that"    
## [16] "except_that_lol"     "that_lol_if"         "lol_if_you"         
## [19] "if_you_have"         "you_have_firefox"    "have_firefox_or"    
## [22] "firefox_or_google"   "or_google_chrome"    "google_chrome_type" 
## [25] "chrome_type_in"      "type_in_let"         "in_let_it"          
## [28] "let_it_snow"         "it_snow_into"        "snow_into_google"
```

So again, let's see, Which are the 20 most frequent trigrams in the dataset?

```r
df.trigram.topfeatures <- topfeatures(df.trigram.dfm, 20)
temp <- data.frame(Words = names(df.trigram.topfeatures), Frequency = unname(df.trigram.topfeatures))

g <- ggplot(data = temp, aes(Words, Frequency)) +
  geom_col() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Trigram Frequency")
print(g)
```

![](Course-10-PA_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Next steps
In the next step I will start to work on the actual algorithm. I plan to use a dictionary with propabilities for every n-gram in my data and use these to predict the next word.
