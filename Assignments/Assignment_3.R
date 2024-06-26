#' Assignment 3
#' Author: Youssef Emam



# Defining a function which reads words from a .txt into a vector
# The words used in my file were taken from: https://github.com/Xethron/Hangman/blob/master/words.txt
#' @name get_words
#' @param filepath
#' @output Vector containing words from the file
get_words <- function (filepath){
  # Read the specified file and save it as words
  masterlist <- read.table(file = filepath, header = F, strip.white = T, col.names = "Words")
  
  #convert to a vector
  wordsvec <- masterlist$Words
  
  # Return the words object
  return(wordsvec)
}



#Read words from file, store as a vector
#TO DO: Create a function which takes a difficulty and then filters a word
wordbank <- get_words("words.txt")

#sample the word
word <- sample(wordbank,1)
