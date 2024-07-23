#' Assignment 4
#' Author: Youssef Emam
#' Date: July 19th 2019
#' Notes: To run this program ensure that the CSV is in the working directory

library(lubridate)
library(tidyverse)

### AB: Proper use of "read.csv" to read dataset into R-studio.
#read the file into the environment
martians <- read.csv("ufo_subset.csv", header = T)

# Important columns are: country, shape and duration.seconds
### AB: Good description of columns of interest; clear to the reviwer.

#no NAs but...
any(is.na(martians$country))
### AB: Good initial check for NAs.

#ordering alphabetically shows that there are empty strings in some columns
martians[order(martians$country), c("country")]
### AB: Effective use of the "order" function to assess the columns for empty strings.

#replace the "" with NA, this will do it for all the columns
martians[martians == ""] <- NA
### AB: Good call to assign all missing values to NAs to ensure they may be dealt with properly.

#redefine a new dataframe which omits any rows with an incomplete observation in the relevant columns
# Important columns are: country, shape and duration.seconds
# This wont remove any rows where the incomplete observations are in the unimportant columns
# I chose to do it this way so that you can analyze the important columns with respect to each other
martians_complete <- martians %>% 
  filter(!is.na(country)) %>% 
  filter(!is.na(shape)) %>% 
  filter(!is.na(duration.seconds))
### AB: Effective use of piping, "filter" and "is.na" function to preserve only the complete observations with repsect to the three columns of interest.

#No NAs in these columns
any(is.na(martians_complete$country))
any(is.na(martians_complete$shape))
any(is.na(martians_complete$duration.seconds))
### AB: Good check for NAs in each column of interest.

# removing observations where there are outliers in the duration.seconds
# we will define an outlier as anything more than 24 hours would be a new day and should have been a new observation  
# 24 hours = 86,400 seconds
martians_complete <- martians_complete %>%
  filter(duration.seconds < 86400)
### AB: Logical assumption made that a valid observation should be limited to those under 24hrs
### AB: Effective use of piping and "filter" function to limit the observations to those under 24hrs


#Removing hoax
#since NUFORC comment on potential hoax citings, the word NUFORC should appear in the comments
#We will filter out the rows where NUFORC appears in the comments
no_hoax <- martians_complete %>%
  filter(!str_detect(pattern = "NUFORC", comments))
### AB: Effective use of piping, "filter" function, and "str_detect" to locate and remove the hoax sitings that are defined by containing comments from NUFORC.

#Add the reporting delay

#first copy the data set
delay <- no_hoax
### AB: Effective reassignment to "no_hoax" as it increases the readability of the script as this variable name is more clear/descriptive.

#convert the  sighting time to ymd format
delay$datetime <- ymd_hm(delay$datetime)
### AB: Functional change to ymd format 

#convert the posting date to dmy, convert to posixct to do math with
delay$date_posted <- dmy(delay$date_posted, tz = "UTC")
### AB: Effective conversion to POSIX format to perform math on the dates of the observations

#add a new column, subtract times and divide by number of seconds in a day
delay$report_delay <- delay$date_posted - delay$datetime
### AB: Effective way of creating a new column that takes the difference between the two dates in question
### AB: Suggestion: you can also use the "mutate" function to create this new table as well.

#convert to days

#filter out rows with negative delay
delay <- delay %>% 
  filter(report_delay > 0)
### AB: Logical, simple, and effective way of removing the invalid circumstance where the date of reporting the siting is earlier than the date of the siting itself.

#convert the delay to days, if anything is less than a day set to 0s
delay <- delay %>%
  mutate(report_delay = case_when(
    report_delay > 86400 ~ seconds_to_period(report_delay),
    report_delay <= 86400 ~ seconds_to_period(0)
    )
  )
### AB: Effective use of "mutate" function and simple math to convert the delay to number of days 
### AB: Effective to also set the values less than a day to zero within the same lines of code.

#Create a table of average report delay grouped by country
delay %>% group_by(country) %>% 
  summarise(Average = mean(report_delay)) %>% 
  ungroup()
### AB: Proper use of piping, "group_by", and "summarise" functions to create a new table containing the average report delay per each country.


#histogram of delays
#log the seconds 
hist(log(delay$duration.seconds),
     main = "Duration of UFO Sightings",
     xlab = "Log of Duration of sighting"
     )
### AB: Histogram is properly lablled and clear.
### AB: Good idea to apply the "log" fucntion to the dataset as this will allow the bins with very small frequency to also be viewed clearly.

### AB GENERAL COMMENTS:
# Code is properly commented with description and concise language
# Code comments are descriptive enough to allow for easy reproducability by the reviewer
# Code is properly formatted to ensure efficient readability
# Code is simple and functional
# Code satisfies the assignment requirements
