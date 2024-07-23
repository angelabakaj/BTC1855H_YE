athlete <- read.csv("olympic_events.csv")

athlete <- rename(athlete, Country = NOC)

gap <- gapminder::gapminder_unfiltered

#join athlete to group

#this is join growth
# the table grew exponentially because there is a one to many relationship
# Ex. for every chinese athlete it created a new row for each year of economic data available for the country
bigdata <- athlete %>% 
  left_join(gap, by= c("Team" = "country"))

# matching by year AND country gives one to one match
betterdata <- athlete %>% 
  left_join(gap, by= c("Team" = "country", "Year" = "year")) %>% 
  
  #group by team name
  group_by(Team) %>% 
  
  #show me the mean height and average gdp for each of those groups
  summarise(AvgHeight = mean(Height, na.rm = TRUE),
            AvgGDP = mean(gdpPercap, na.rm = TRUE)
            )

#plot avg height vs avg GDP

plot(betterdata$AvgHeight, betterdata$AvgGDP)
