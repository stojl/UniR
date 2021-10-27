library(Rcpp)
sourceCpp(file = "School/Simulation_af_Snyd/dcount.cpp")

rollbeforecomp <- function(dice_vector, m = 1){
  
  #Initialize
  total_dice <- sum(dice_vector)
  result <- c()

  #Simulation
  for(i in 1:m){
    result <- c(result, sumDice(sample(1:6, total_dice, replace = TRUE), dice_vector))
  }
  
  #Output
  m <- matrix(c(rep(1:m, each = 6), rep(1:6, m), result), m*6, 3)
  colnames(m) <- c("gameid", "value", "count")
  return(m)
}

simulate_roll <- compiler::cmpfun(rollbeforecomp)