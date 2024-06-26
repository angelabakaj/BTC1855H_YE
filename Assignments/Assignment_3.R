#' Assignment 3
#' Author: Youssef Emam
#' To run: ensure that the wordbank txt is in the working directory



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
wordbank <- get_words("words.txt")

#sample the word
secretword <- sample(wordbank,1)

#set number of guesses and inform the user how many guesses
# RULE: User gets n+1 guesses for a word of length n, with a minimum of 5 guesses
word_len <- nchar(secretword)
num_guesses <- word_len + 1

#Inform the user of the length of the word, and number of guesses
cat("The secret word is", word_len, "letters long. You have", num_guesses, "tries to guess the word!\n")


#Create a vector the same size as the word with "_"s to represent missing letters
# We are going to replace the "_"s with the letters they guess correctly

progress <- 1:word_len

for (i in progress){
  progress[i] <- "_"
}

for (i in num_guesses){
  # Show progress
  cat(progress)
  
  #Take a guess from the user
  
  #determine if the guess is correct
  
  #Exit if correct
}


