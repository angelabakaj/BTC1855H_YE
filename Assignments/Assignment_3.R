#' Assignment 3
#' Author: Youssef Emam



# Defining a function which reads words from a .txt into a vector
# The words used in my file were taken from: https://github.com/Xethron/Hangman/blob/master/words.txt
#' @name get_words
#' @param filepath
#' @output Vector containing words from the file
get_words <- function (filepath){
  words <- read.table(file = filepath, header = F, strip.white = T, col.names = "Words")
  return(words)
}