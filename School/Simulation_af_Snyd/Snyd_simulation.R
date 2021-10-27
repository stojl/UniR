library(tibble)
library(magrittr)
library(Rcpp)
sourceCpp(file = "School/Simulation_af_Snyd/dcount.cpp")

rollbeforecomp <- function(dice_vector, m){
  
  #Initialize
  total_dice <- sum(dice_vector)
  result <- c()
  
  #Simulation
  for(i in 1:m){
    result <- sample(1:6, total_dice, replace = TRUE) %>% 
      sumDice(dice_vector) %>% c(result, .)
  }
  
  result %>% tibble(gameid = rep(1:m, each = 6), dice_value = rep(1:6, m), dice_count = .)
  
}

simulate_roll <- compiler::cmpfun(rollbeforecomp)


