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

# Defining a function which takes a guess from the user
#' @name take_guess
#' @param none
#' @output returns the guess and whether the user guessed a letter or a whole word
take_guess <- function (){
  
  
  #Create a flag that will tell us what kind of guess the user returns
  guess_type <- NULL
  
  #string of characters that are not allowed | indicates OR
  not_allowed <- 
  
  # Keep taking inputs until one is satisfactory
  repeat{
    #take an input
    #convert to lowercase since the word bank is all lowercase
    input <- tolower(readline("Please guess a letter, or guess the entire word: "))
    
    # note: the [some character] notation is called a regular expression (regex)
    # [0-9] means any string with any digit 0-9 in it
    # [[:punct:]] means any string with punctuation/special characters
    #the grepl function checks for any whatever pattern you choose in the input
    
    #check that it is not a number
    if (grepl("[0-9]", input)){
      cat("Numbers are not allowed: ")
      
      # Check that the user inputted something at all
    } else if (input == ""){
      cat("Please guess a letter or the entire word: ")
      
      #check for any special characters
    } else if (grepl("[[:punct:]]", input)){
      
      cat("Special characters are not allowed: ")
      
      #if all the checks are passed take the guess
    } else if (input == "quit game"){
      
      guess_type <- "quit"
      break
      
    } else{
      
      #check if the user entered a single letter
      if (nchar(input) == 1){
        
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

#set number of guesses and inform the user how many guesses
# RULE: User gets n+1 guesses for a word of length n, with a minimum of 5 guesses
word_len <- nchar(secretword)

if (word_len <= 5){
  num_guesses <- 5
} else {
  num_guesses <- word_len + 1
}


#turn the secret word into a vector so it's easier to work with later
secretwordvec <- unlist(strsplit(secretword, split = ""))

#Inform the user of the length of the word, and number of guesses
cat("The secret word is", word_len, "letters long. You have", num_guesses, "tries to guess the word!\n")
cat("Type: \"quit game\" at any time to exit the game\n")

#Create a vector the same size as the word with "_"s to represent missing letters
# We are going slowly to replace the "_"s with the letters they guess correctly
progress <- 1:word_len

# populating the vector with "_"s
for (i in progress){
  progress[i] <- "_"
}

#create a bank to hold all the previous guesses
guesses <- vector()

#Create a variable to determine win status
# We will check this later to determine if the user has won the game or not
win <- FALSE

# this is the main game loop
# We are going to repeat as many times as they have guesses, and potentially break out earlier
for (i in 1:num_guesses){
  
  # Show their progress
  cat(progress, " | ", "Guesses remaining: ", num_guesses, "\n")
  cat("Previous guesses:", guesses, "\n\n")
  
  #Take a guess from the user, and identify what kind of guess it is
  # Here the guess variable will be a character vector containing [The type of guess (word/just a letter), the actual guess (the input)]
  guess <- take_guess()
  
  #reduce their number of guesses by 1
  num_guesses <- num_guesses-1
  
  #determine if the guess is correct
  #if they guess a single letter, see if it matches any in the secret word
  if (guess[1] == "single"){
    
    #extract the input from the returned vector
    guess <- guess[2]
    
    #create a temporary version of progress to check against after
    previous_prog <- progress
    
    #check the guess against all the letters in the secret word
    for (i in 1:word_len){
      if (guess == secretwordvec[i]){
        progress[guess == secretword]
        
        #update the user's progress
        progress[i] <- guess
      }
    }
    
    #Check if none of the letters changed
    if (all(previous_prog == progress)){
      
      cat(guess, " is not in the word!\n\n")
      
      #add the guess to the list of letters already guessed
      guesses <- c(guesses, guess)
    }
    # if there are changes it means the letter was in the secret word
    else {
      cat(guess, " is in the word!\n\n")
    }
    
    #check win condition
    if (all(progress == secretwordvec)){
      win <- TRUE

      #stop the loop if they guessed correctly
      break
    }
    
    #if they guess a word then see if it matches
  } else if (guess[1] == "word"){
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
    
    #if the user quits
  } else {
    
    #break the loop
    break
  }
}

#Determine if the user won or lost
if (win == TRUE) {
  
  #print an appropriate message
  cat("CONGRATUALTIONS, you win!\n")
  cat("The word was:", secretword, "\n")
  
} else {
    cat("Game over!\n")
    cat("The word was:", secretword, "\n")
}

