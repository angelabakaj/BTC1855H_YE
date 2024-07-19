#' Assignment 4
#' Author: Youssef Emam
#' Date: July 19th 2019
#' Notes: To run this program ensure that the CSV is in the working directory

library(lubridate)
library(tidyverse)


#read the file into the environment
martians <- read.csv("ufo_subset.csv", header = T)

# Important columns are: country, shape and duration.seconds

#no NAs but...
any(is.na(martians$country))

#ordering alphabetically shows that there are empty strings in some columns
martians[order(martians$country), c("country")]

#replace the "" with NA, this will do it for all the columns
martians[martians == ""] <- NA

#redefine a new dataframe which omits any rows with an incomplete observation in the relevant columns
# Important columns are: country, shape and duration.seconds
# This wont remove any rows where the incomplete observations are in the unimportant columns
# I chose to do it this way so that you can analyze the important columns with respect to each other
martians_complete <- martians %>% 
  filter(!is.na(country)) %>% 
  filter(!is.na(shape)) %>% 
  filter(!is.na(duration.seconds))

#No NAs in these columns
any(is.na(martians_complete$country))
any(is.na(martians_complete$shape))
any(is.na(martians_complete$duration.seconds))

# removing observations where there are outliers in the duration.seconds
# we will define an outlier as anything more than 24 hours would be a new day and should have been a new observation  
# 24 hours = 86,400 seconds
martians_complete <- martians_complete %>%
  filter(duration.seconds < 86400)


#Removing hoax
#since NUFORC comment on potential hoax citings, the word NUFORC should appear in the comments
#We will filter out the rows where NUFORC appears in the comments

no_hoax <- martians_complete %>%
  filter(!str_detect(pattern = "NUFORC", comments))


#Add the reporting delay

#first copy the data set
delay <- no_hoax

#convert the  sighting time to ymd format
delay$datetime <- ymd_hm(delay$datetime)

#convert the posting date to dmy, convert to posixct to do math with
delay$date_posted <- dmy(delay$date_posted, tz = "UTC")

#add a new column, subtract times and divide by number of seconds in a day
delay$report_delay <- delay$date_posted - delay$datetime

#convert to days

#filter out rows with negative delay
delay <- delay %>% 
  filter(report_delay > 0)

#convert the delay to days, if anything is less than a day set to 0s
delay <- delay %>%
  mutate(report_delay = case_when(
    report_delay > 86400 ~ seconds_to_period(report_delay),
    report_delay <= 86400 ~ seconds_to_period(0)
    )
  )

#Create a table of average report delay grouped by country
delay %>% group_by(country) %>% 
  summarise(Average = mean(report_delay)) %>% 
  ungroup()


#histogram of delays
hist(log(delay$duration.seconds),
     main = "Duration of UFO Sightings",
     xlab = "Log of Duration of sighting"
     )
