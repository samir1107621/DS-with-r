library(tidyverse)
library(rvest)


# Question E

html_e <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
Movie_Ranking <- html_e %>% html_elements(".countdown-index") %>% html_text() 
Movie_name <- html_e %>% html_elements(".article_movie_title a") %>% html_text()
movie_year <- html_e %>% html_elements(".subtle.start-year") %>% html_text() %>%
  substr(2, 5) %>% as.numeric()
movie_ratings <- html %>% html_elements(".tMeterScore") %>% html_text() 

movie_data <- data.frame( "Ranking" = Movie_Ranking, "name" = Movie_name, "year" = movie_year,
                          "rating" = movie_ratings)
movie_data




# Question A

html_a <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")

company_name <- html_a %>% html_elements("company-ellipses a") %>% html_text()
CMP <- html_a %>% html_elements("")
Price_change <- html_a %>% html_elements("text-green")
Market_cap <- html_a %>% html_elements("")
week_high52 <- html_a %>% html_elements("")
week_low52 <- html_a %>% html_elements("")
ROE <- html_a %>% html_elements("")
P/E <- html_a %>% html_elements("")
p/BV <- html_a %>% html_elements("")
EV/EBITDA <- html_a %>% html_elements("")
YSales_5yr  <- html_a %>% html_elements("")
YProfit_5yr  <- html_a %>% html_elements("")






# Question C
 
#c 1
tennis <- function(p)
{
  
  # count of wins for A and B
  A <- 0
  B <- 0
  for(k in 1:5)
  {
    next.game <- sample(0:1, size = 1, prob = c(p, 1-p))
    if(next.game == 0) B <- B+1
    if(next.game == 1) A <- A+1
    
    # best-of-series finishes when any team reaches 3
    if(A == 3 || B == 3)
    {
      x <- k 
      break
    }
  }  
  return(x)
}

# c2

matches <- numeric(length = 1000)
for(i in 1:1000)
{
  matches[i] <- tennis(.7)
}

ans <- mean(matches)
ans




# Question D

#  switching is always a good choice.
# part 1
MontyHall <- function() {
  doors <- c("car", "goat", "goat")  # Create a vector representing the doors
  chosen_door <- sample(doors)  # Randomly choose a door
  
  # Find the index of the chosen door and the index of the door with a goat
  chosen_index <- which(doors == chosen_door)
  goat_index <- which(doors == "goat" & seq_along(doors) != chosen_index)
  
  # Monty opens one of the other doors with a goat
  open_door <- goat_index[1]
  
  # Switch the choice to the other remaining door
  remaining_door <- setdiff(seq_along(doors), c(chosen_index, open_door))               
  
  # Check if the remaining door has the car
  if (doors[remaining_door] == "car") {
    return(1)  # Contestant wins
  } else {
    return(0)  # Contestant loses
  }
}

#Question D part 2
num_simulations <- 1000  # Number of simulations
num_wins <- 0  # Counter for wins

# Run the simulation 1000 times
for (i in 1:num_simulations) {
  result <- MontyHall()
  num_wins <- num_wins + result
}

# Calculate the probability of winning if the contestant switches
probability <- num_wins / num_simulations







\