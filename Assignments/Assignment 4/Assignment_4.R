#' Assignment 4
#' Author: Youssef Emam
#' Date: July 19th 2019
#' Notes: To run this program ensure that the CSV is in the working directory


#read the file into the environment
martians <- read.csv("ufo_subset.csv", header = T)
attach(martians)

# Important columns are: country, shape and duration.seconds

#no NAs but...
any(is.na(martians$country))

#ordering alphabetically shows that there are empty strings in some columns
martians[order(martians$country), c("country")]

#replace the "" with NA
martians$country[martians$country == ""] <- NA

