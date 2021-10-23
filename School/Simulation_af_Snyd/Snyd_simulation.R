library(tidyverse)
library(magrittr)

simulate_roll <- function(number_of_players, dice_vector, m){
  
  #Initialize
  total_dice <- sum(dice_vector)
  
  player_index_vec <- 1:number_of_players
  
  index_vec <- rep(1:number_of_players, times = dice_vector)
  
  index_dice_value_template <- expand_grid(index = player_index_vec, dice_value = 1:6)
  
  count_dice <- function(df, n){
    
    trappevalue <- df %>% filter(dice_value %in% 1:n) %$% 
      prod(dice_value_count)
    
    et_count <- df$dice_value_count[1]
    
    if(trappevalue == 1){
      df %>% mutate(dice_value_count = n + 1)
    } else {
      df %>% mutate(dice_value_count = ifelse(
        dice_value == 1, dice_value_count, dice_value_count + et_count
      ))
    }
  }
  
  result <- tibble(gameid = integer(), 
                   dice_value = integer(), 
                   total_dice = integer())
  
  #Simulation
  
  for(i in 1:m){
    result <- sample(1:6, total_dice, replace = TRUE) %>% 
      tibble(index = index_vec, dice_value = .) %>% 
      group_by(index, dice_value) %>% 
      summarise(dice_value_count = n()) %>% 
      left_join(index_dice_value_template, ., by = c("index", "dice_value")) %>% 
      replace(is.na(.), 0) %>% 
      nest(data = c(dice_value, dice_value_count)) %>% 
      mutate(data = map2(data, dice_vector, count_dice)) %>% 
      unnest(data) %>%
      ungroup() %>% 
      suppressMessages %>% 
      group_by(dice_value) %>% 
      summarise(total_dice = sum(dice_value_count)) %>%
      mutate(gameid = i) %>% 
      bind_rows(result, .)
  }
  
  return(result)
}