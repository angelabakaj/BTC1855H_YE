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
### AB: Well-designed and impressive use of functions and code to log the wordbank into the game.
### AB: A less sophisticated way may save some time and code:
### AB: Consider: setwd("insert pathway here"), readLines("words.txt").

# Defining a function which takes a guess from the user
#' @name take_guess
#' @param none
#' @output returns the guess and whether the user guessed a letter or a whole word
take_guess <- function (){
  ### AB: Good use of the "function" function to define a function that will accept each letter/guess from the user.
  
  #Create a flag that will tell us what kind of guess the user returns
  guess_type <- NULL
  ### AB: Effective feature to include, satisfies the game requirements. 
  
  #string of characters that are not allowed | indicates OR
  not_allowed <- 
    ### AB: Effective feature to include, also helps to satisfy the game requirements.
    
    # Keep taking inputs until one is satisfactory
    repeat{
      #take an input
      #convert to lowercase since the word bank is all lowercase
      input <- tolower(readline("Please guess a letter, or guess the entire word: "))
      ### AB: Good use of the "tolower" and "readLine" function in tandem to ensure letters are converted to lowercase.
      
      # note: the [some character notation] is called a regular expression (regex)
      # [0-9] means any string with any digit 0-9 in it
      # [[:punct:]] means any string with punctuation/special characters
      #the grepl function checks for any whatever pattern you choose in the input
      ### AB: Excellent description of code, ensures and enhances user understanding.
      
      #check that it is not a number
      if (grepl("[0-9]", input)){
        cat("Numbers are not allowed: ")
        ### AB: Effective use of "grepl" and "cat" functions to exclude numbers from user inputs/guesses and to display this rule to the user, respectively.
        
        # Check that the user inputted something at all
      } else if (input == ""){
        cat("Please guess a letter or the entire word: ")
        ### AB: Good call to ensure user has left an input at all, effective use of "cat" function to display this rule to the user.
        
        #check for any special characters
      } else if (grepl("[[:punct:]]", input)){
        cat("Special characters are not allowed: ")
        ### AB: Effective use of "grepl" and "cat" functions to exclude special characters from user inputs/guesses and to display this rule to the user, respectively.
        
        #if all the checks are passed take the guess
      } else{
        
        #check if the user entered a single letter
        if (nchar(input) == 1){
          ### AB: Effective use of "nchar" function to limit user input to one singular letter only.
          #set the guess type to single
          guess_type <- "single"
          
          #break the loop to allow the function to return values
          break
          
          
          #check if the input is a whole word
          #we will assume that if the length is greater than 1, they are guessing a word
        } else{
          #set guess type to entire word
          guess_type <- "word"
          
          #break the loop to allow the function to return values
          break
        }
        ### AB: Effective use of variable and value assignment to specify whether the user is attempting to guess the entire word or not.
        
        #check if the input is a whole word
        #we will assume that if the length is greater than 1, they are guessing a word 
      } 
    }
  # return a vector where the first element is the type of guess
  # second element is the guess itself
  return(c(guess_type, input))
}


#Read words from file, store as a vector
wordbank <- get_words("words.txt")

#sample the word
secretword <- sample(wordbank,1)
### AB: Good use of "sample" function to provide the user with a new, singular word from the wordbank each time.

#set number of guesses and inform the user how many guesses
# RULE: User gets n+1 guesses for a word of length n, with a minimum of 5 guesses
word_len <- nchar(secretword)
num_guesses <- word_len + 1
### AB: Clear and functional way of setting a minimum and maximum number of attempts for the user.

#turn the secret word into a vector so it's easier to work with later
secretwordvec <- unlist(strsplit(secretword, split = ""))
### AB: Efficient use of code; pre-planning is evident and communicated.

#Inform the user of the length of the word, and number of guesses
cat("The secret word is", word_len, "letters long. You have", num_guesses, "tries to guess the word!\n")
### AB: Efficient and functional use of "cat" function to display the word length and specify the number of tries the user has to guess the word.

#Create a vector the same size as the word with "_"s to represent missing letters
# We are going to replace the "_"s with the letters they guess correctly

progress <- 1:word_len

for (i in progress){
  progress[i] <- "_"
}

### AB: Good stylistic choice in using underscores to display the missing letters of the secret word.

#create a bank to hold all the guesses
guesses <- vector()
### AB: Efficient way of logging the guesses to ensure user does not re-guess an already guessed letter.

#Set a variable to determine win status
win <- FALSE

for (i in 1:num_guesses){
  # Show progress
  cat(progress, " | ", "Guesses remaining: ", num_guesses, "\n")
  cat("Previous guesses", guesses, "\n\n")
  ### AB: Stylistically clear and impressive and functional way of displaying the user's progress, remaining guesses, and previous guesses. 
  
  #Take a guess from the user, and identify what kind of guess it is
  guess <- take_guess()
  
  #reduce their num guesses by 1
  num_guesses <- num_guesses-1
  ### AB: Good way to decrease the number of guesses remaining with each attempt.
  
  #determine if the guess is correct
  
  #if they guess a single letter, see if it matches any in the secret word
  if (guess[1] == "single"){
    
    #extract the guess from the returned vector
    guess <- guess[2]
    
    #create a temporary version of progress to check against after
    previous_prog <- progress
    
    #check the guess against all the letters in the secret word
    for (i in 1:word_len){
      if (guess == secretwordvec[i]){
        
        #update the user's progress
        progress[i] <- guess
      }
    }
    ### AB: Functional use of code to compare user's guess/input to the secret word; satisfies the game requirements.
    
    #Check if none of the letters changed
    if (all(previous_prog == progress)){
      
      cat(guess, " is not in the word!\n\n")
      
      #add the guess to the list of letters already guessed
      guesses <- c(guesses, guess)
    }
    # if there are changes it means the letter was in the secret word
    else{
      cat(guess, " is in the word!\n\n")
    }
    
    ### AB: Effective use of "all" and "cat" functions to display to the user whether their guess was correct or not.
    
    #check win condition
    if (all(progress == secretwordvec)){
      win <- TRUE
      
      #stop the loop if they guessed correctly
      break
    }
    
    ### AB: Simple and effective use of code to establish user victory.
    
    #if they guess a word then see if it matches
  } else{
    #extract the guess from the returned vector
    guess <- guess[2]
    
    #check if the guess matches the secret word
    if (guess == secretword){
      
      # Change state to win
      win <- TRUE
      
      #stop the loop
      break
      # if the guess isn't right then print an appropriate message
      
    } else{
      
      cat("Wrong guess, try again!\n\n")
    }
  }
}

#Determine if the user won or lost
if (win == TRUE) {
  
  #print an appropriate message
  cat("CONGRATUALTIONS, you win!\n")
  cat("The word was:", secretword, "\n")
  
} else {
  cat("OH NO!, you lost!\n")
  cat("The word was:", secretword, "\n")
}

### AB General Comments: 
# Stylistically speaking the game is very good; clear and well displayed to the user.
#-The code runs efficiently; no errors nor warning messages returned.
#-The coder’s language in the script is simple, colloquial, yet accurate in terms of necessary coding terms.
#-The coder’s language is also very descriptive, which not only shows an understanding of the functions being used and for what purpose, but also allows for increased reproducibility of the code.
#-The code fulfills the requirements of the Assignment 3 criteria, returning a fun and functional game of Hangman.
#-Code shows there was attention taken to not only game requirement fulfilment, but also readability, etc.  
