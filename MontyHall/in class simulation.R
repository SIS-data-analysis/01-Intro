# SUBJECT:  Monty Hall simulation
# AUTHOR:   Austin Hart
# DATE:     1/17/2023

# SETUP -------------------------------

# load packages
  library(tidyverse)
    # install.packages('tidyverse') if needed
  

# SIMULATION --------------------------
  
# Set the stage
  doors = c('A','B','C')
  
# Find contestants to play 10,000 times and keep score
  Switcher = rep(NA, 10000) # aka Marilyn
  Stayer = rep(NA, 10000)   # aka Mr Skeptic
  
  records = tibble(Switcher,Stayer) # combine to single frame
  
# Simulate the game
  for (i in 1:10000){ # repeats the game 10,000 times
    # put the car behind a random door
      car = sample(doors, 1)
    
    # choose one door at random
      pick = sample(doors, 1)
    
    # host reveals a goat
      reveal = sample(doors[doors != pick & doors != car], 1)
    
    # have Switcher choose the other door
      switch = doors[doors != pick & doors != reveal]
    
    # Award prize to Stayer
      records$Stayer[i] = if_else(pick == car, 'NEW CAR', 'goat')
    
    # Award prize to Switcher
      records$Switcher[i] = if_else(switch == car, 'NEW CAR', 'goat')
  }
  
  rm(car, doors, i, pick, reveal, switch) # remove objects
  
  
# RESULTS -------------------
  
# Tabulate wins, calculate win-percentage
  outcome = 
    records %>%
    count(Switcher) %>%
    mutate(Percent = n/sum(n) * 100)
 
# Graph   
  ggplot(data = outcome, aes(x = Switcher, y = Percent)) +
    geom_bar(stat = 'identity') +
    labs(x = 'Switcher wins...',
         title = 'Monty Hall Simulations') +
    theme_minimal()


