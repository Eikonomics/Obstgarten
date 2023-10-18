# 1. Basic set-up ----
library(tidyverse)
library(ggplot2)

rm(list = ls()) # clean the house 
SEED = 1023 # set seed for replication

# How many simulations and bootstraps to do
NUMBER_OF_GAMES_TO_PLAY = 199 # Generate # games
NUMBER_OF_GAMES_TO_RE_SAMPLE = 100 # EACH ROUND OF GAMES ABOVE i DRAW # OF GAMES FROM TO RE-SAMPLE 
NUMBER_OF_TIMES_RESAMPLE = 100 # re-sample the games # times AND AVERAGE THAT TO GET A DISTRIBUTION

# 2. A Function to simmulate games ----
obstgarten_simmulation_function <- function(STRATEGY = c("toddler", "parent"),
                                            RABEN_STEPS = 5,
                                            N_GAMES = 999,
                                            SEED = NULL) {
  set.seed(SEED)
  
  # set player behaviour
  strategy <- STRATEGY #c("toddler", "parent")
  
  # select how many games to play
  n_games <- N_GAMES # c(1,...,n)
  
  i = 1 # index for storing data from loop (within game)
  # define dice
  dice <- c(1:6)
  raben_steps <- RABEN_STEPS
  
  # Set-up all vectors
  roll_n <- c()  # roll number in each game
  roll_i <- c()  # dice roll (1-6)
  round_j <- c() # game id (1-n)
  
  # vectors for dice results
  reds_1   <- c(4)
  greens_2 <- c(4)
  plums_3  <- c(4)
  pears_4  <- c(4)
  raben_6  <- c(raben_steps)
  joker_5  <- c(0)
  
  joker_choice <- c("") # character vector with choice of fruit when joker comes up
  
  # Simmulation
  for (j in c(0:n_games)) {
    
    # Set-up number of fruits on each tree
    n_reds_1   <- 4
    n_greens_2 <- 4
    n_plums_3  <- 4
    n_pears_4  <- 4
    n_joker_5 <- 0
    n_raben_6 <- raben_steps
    
    # reset jokers fruit
    randomly_pick_fruit <- ""
    
    # roll sequence within a game (reset)
    roll_ji = 0
    roll = 0
    # carry out proceedure until the Raven reaches the orchard
    while(n_raben_6 >= 0) {
      
      # index for saving out data
      i = i + 1
      
      # store order data
      roll_n[i] = roll_ji
      roll_i[i] = roll
      round_j[i] = j
      reds_1[i]   <- n_reds_1
      greens_2[i] <- n_greens_2
      plums_3[i]  <- n_plums_3
      pears_4[i]  <- n_pears_4
      joker_5[i]  <- n_joker_5
      raben_6[i]  <- n_raben_6
      
      joker_choice[i] <- randomly_pick_fruit
      
      roll_ji = roll_ji + 1
      
      # Roll the dice
      roll <- sample(dice,size = 1)
      
      
      
      # respond to dice, by...
      
      # move raven 
      if (roll == 6) {
        n_raben_6 = n_raben_6 - 1 
      }
      
      # remove friuit
      if (roll == 4 & n_pears_4 > 0) {
        n_pears_4 = n_pears_4 - 1 
      }
      
      # ...
      if (roll == 3 & n_plums_3 > 0) {
        n_plums_3 = n_plums_3 - 1 
        
      }
      
      # ...
      if (roll == 2 & n_greens_2 > 0) {
        n_greens_2 = n_greens_2 - 1 
      }
      
      # ...
      if (roll == 1 & n_reds_1 > 0 ) {
        n_reds_1 = n_reds_1 - 1 
      }
      
      # when a Joker comes up, we need to pick a fruit to remove
      if (roll == 5) {
        
        # exit this routine, if game is actually over 
        # (this should have been done at an earlier stage, but, not like anyone is paying me.
        n_fruits_left <- sum(n_reds_1, n_greens_2, n_plums_3, n_pears_4)
        if (n_fruits_left > 0) {
          
          if (strategy == "toddler") {
            
            # toddler strategy, pick at random
            while(sum(n_reds_1, n_greens_2, n_plums_3, n_pears_4) == n_fruits_left) {
              
              randomly_pick_fruit <- sample(c("reds_1", "greens_2", "plums_3", "pears_4"),1)
              
              # adjust fruits left on tree
              if (randomly_pick_fruit == "reds_1" & n_reds_1 > 0) {
                n_reds_1 = n_reds_1 - 1 
              }
              if (randomly_pick_fruit == "greens_2" & n_greens_2 > 0) {
                n_greens_2 = n_greens_2 - 1 
              }
              
              if (randomly_pick_fruit == "plums_3" & n_plums_3 > 0) {
                n_plums_3 = n_plums_3 - 1 
              }
              
              if (randomly_pick_fruit == "pears_4" & n_pears_4 > 0) {
                n_pears_4 = n_pears_4 - 1 
              }
            } 
          } else {
            
            # parent strategy, pick the tree with the most fruit
            jokers_choice <-   
              tibble(fruit_name = c("reds_1", "greens_2", "plums_3", "pears_4"),
                     number_on_tree = c(n_reds_1, n_greens_2, n_plums_3, n_pears_4)) %>% 
              mutate(rank = rank(-number_on_tree,ties.method = "random")) %>%
              filter(rank == 1) %>%
              pull(fruit_name)
            
            # adjust fruits left on tree
            if (jokers_choice == "reds_1" & n_reds_1 > 0) {
              n_reds_1 = n_reds_1 - 1 
            }
            if (jokers_choice == "greens_2" & n_greens_2 > 0) {
              n_greens_2 = n_greens_2 - 1 
            }
            
            if (jokers_choice == "plums_3" & n_plums_3 > 0) {
              n_plums_3 = n_plums_3 - 1 
            }
            
            if (jokers_choice == "pears_4" & n_pears_4 > 0) {
              n_pears_4 = n_pears_4 - 1 
            }
          }
        }
      }
    }
  }
  
  # get the data into shape for analysis 
  # this can be simplified, for quicker runtime
  master_data <-
    tibble(roll_n = roll_n,
           round_j = round_j,
           roll_i = roll_i , 
           red_apples = reds_1, 
           green_apples = greens_2,
           plums = plums_3,
           pears = pears_4,
           joker = 0,
           raben = raben_6,
           joker_choice = joker_choice) %>%
    mutate(fruits_left = red_apples + green_apples + plums + pears) %>%
    mutate(joker = if_else(roll_i == 5, 1, 0)) %>%
    replace(is.na(.), 0) %>% 
    group_by(round_j) %>%
    rowwise() %>%
    mutate(picked_highest = max(c_across(red_apples:pears))) %>%
    # game over means you lost against the raven
    mutate(game_over = case_when(raben == 0 & fruits_left > 0 ~ 1, 
                                 raben > 0 & fruits_left == 0 ~ 0)) %>%
    mutate(remove = fruits_left * raben) %>%
    group_by(round_j) %>%
    mutate(remove = lag(remove)) %>%
    filter(remove != 0)
  
  return(master_data)
  
}

# 3. Analysis ----
# 3.1 just calculate the probability of winning the game by strategy 

# Calculate the probabilities by playing 1m games with each strategy (total 2m games)
simple_probs <- 
  map_df(c("toddler", "parent"), 
         obstgarten_simmulation_function, 
         RABEN_STEPS = 5, 
         N_GAMES = NUMBER_OF_GAMES_TO_RE_SAMPLE * NUMBER_OF_TIMES_RESAMPLE, 
         .id = "scenario")

# do the math
simple_probs %>% 
  filter(!is.na(game_over)) %>%
  group_by(scenario, game_over) %>%
  summarise(n = n(),
            roll_n = mean(roll_n)) %>%
  mutate(pr_loose = sum(game_over * n) / sum(n)) %>%
  filter(game_over == 0)


single_scenario <- 
  obstgarten_simmulation_function(STRATEGY =  "parent", 
                                  N_GAMES = 999, 
                                  RABEN_STEPS = 5,
                                  SEED = 1023)

bootstra_data <-
map_df(.f = obstgarten_simmulation_function, .x =  c("toddler", "parent"), 
                                N_GAMES = 999, 
                                RABEN_STEPS = 5,
                                SEED = NULL, .id = "scenario")

# Bootstrap function
bootstrap_fun <- function(NUMBER_OF_ROUNDS = 1, SAMPLE_SIZE = 100) {
  bootstra_data %>%
    ungroup() %>%  
    filter(!is.na(game_over)) %>%
    group_by(scenario) %>%
    sample_n(size = SAMPLE_SIZE) %>%
    group_by(game_over, scenario) %>%
    summarise(n = n(),
              roll_n = mean(roll_n)) %>%
    group_by(scenario) %>%
    mutate(pr_loose = sum(game_over * n) / sum(n)) %>%
    mutate(round = NUMBER_OF_ROUNDS) 
}


bootstrap_results <- map_df(.x = c(1:10000), .f = bootstrap_fun, SAMPLE_SIZE = 100)
#bootstrap_results_parent <- map_df(.x = c(1:100), .f = bootstrap_fun, SAMPLE_SIZE = 50)

bootstrap_results %>%
  filter(game_over == 0) %>%
  ggplot() +
  aes(x = 1 - pr_loose, fill = as.factor(scenario), color = as.factor(scenario)) + 
  geom_density(alpha = 0.5) 

bootstrap_results %>%
  filter(game_over == 0) %>%
  group_by(scenario) %>%
  ggplot() +
  aes(x = pr_loose, fill = as.factor(scenario), color = as.factor(scenario)) + 
  geom_histogram(bins = 40, alpha = 0.7)   
  facet_wrap(~scenario)

  bootstrap_results %>%
    filter(game_over == 0) %>%
    group_by(scenario) %>%
    summarise(mean_pr_loose = mean(pr_loose))


# Function for estimating a single scenario
single_scenario <- 
  obstgarten_simmulation_function(STRATEGY =  "toddler", 
                                  N_GAMES = 999, 
                                  RABEN_STEPS = 5,
                                  SEED = 1023)
# example of probabilites
single_scenario %>%
  filter(!is.na(game_over)) %>%
  group_by(game_over) %>% 
  summarise(n = n(),
            roll_n = mean(roll_n)) %>%
  mutate(pr_loose = sum(game_over * n) / sum(n))

# need to fix
single_scenario %>%
  mutate_at((vars(red_apples:pears)), .funs =  list(lag = ~lag(.))) %>%
  mutate(red_apples_lag = red_apples / red_apples_lag) %>%
  mutate(green_apples_lag = green_apples  / green_apples_lag ) %>%
  mutate(plums_lag = plums  / plums_lag) %>%
  mutate(pears_lag = pears  / pears_lag) %>%
  select(roll_i, joker, red_apples:pears, red_apples_lag:pears_lag) %>% print(n = 60)




